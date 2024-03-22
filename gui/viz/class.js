/*jshint esversion: 6*/

var opacity = {
    node_normal: 1,
    node_hidden: 0.25,
    line_highlight: 0.9,
    line_normal: 0.4,
    line_side: 0.15,
    line_hidden: 0.1
};

var snapshot = {
    NO: 0,
    YES: 1
};

/* Contains classes required for the SiteGraph visualization */

class Dimension {
    constructor (width, height) {
        this.width = width;
        this.height = height;
    }

    add(dimension) {
        this.width += dimension.width;
        this.height += dimension.height;
    }

    scale(s) {
        this.width *= s;
        this.height *= s;
    }

    update(dimension) {
        this.height = dimension.height;
        this.width = dimension.width;
    }


}

class D3Object {
    constructor(label) {
        this.label = label;
        this.dimension = new Dimension(0, 0);
    }

    setDimension(dimension) {
        this.dimension = dimension;
    }

    getDimension() {
        return this.dimension;
    }
}

class Site extends D3Object {
    constructor (siteData, agent) {
        super(siteData.site_name);
        let site = this;
        this.agent = agent;
        let type = siteData.site_type;
        if (type[0] == "port") {
            let port = type[1];

	    this.links = port.port_links.map(function(link) {
                return new SiteLink(link[0][1],link[1]);
	    });
	    this.states = port.port_states.map(function(state) {
                return new State(state, site);
	    });
	};
	if (type[0] == "counter") {
	    this.counter = type[1];
	};

        this.currentState = null;
        this.startAngle = 0;
        this.endAngle = 0;
        this.clicked = 0;
        this.hover = 0;
    }

    setId(id) {
        this.id = id;
    }
    setCurrentState(state) {
        this.state = state;
    }

    listLinks() {
        return this.links;
    }

    getAgent() {
        return this.agent;
    }

    setAngles(startAngle, endAngle) {
        this.startAngle = startAngle;
        this.endAngle = endAngle;
    }

    getStates() {
        return this.states;
    }

    generateSelfLoopPath(innerRadius) {
         let pathObj = [];
            let pathPointStart = {};
            pathPointStart.x = this.cartX(innerRadius);
            pathPointStart.y = this.cartY(innerRadius);

            let pathPointSide1 = {};

            pathPointSide1.x = 7 * innerRadius/8 * Math.cos(this.startAngle + 3 * Math.PI/2);
            pathPointSide1.y = 7 * innerRadius/8 * Math.sin(this.startAngle + 3 * Math.PI/2);

            let pathPointMid = {};
            pathPointMid.x = this.cartX(3 * innerRadius / 4);
            pathPointMid.y = this.cartY(3 * innerRadius / 4);

            let pathPointSide2 = {};

            pathPointSide2.x = 7 * innerRadius/8 * Math.cos(this.endAngle + 3 * Math.PI/2);
            pathPointSide2.y = 7 * innerRadius/8 * Math.sin(this.endAngle + 3 * Math.PI/2);

            let pathPointEnd = {};
            pathPointEnd.x = this.cartX(innerRadius);
            pathPointEnd.y = this.cartY(innerRadius);
            pathObj.push(pathPointStart);
            pathObj.push(pathPointSide1);
            pathObj.push(pathPointMid);
            pathObj.push(pathPointSide2);
            pathObj.push(pathPointEnd);
            return pathObj;
    }
    generateTreeObj() {
        let treeObj = {};
        treeObj.name = this.label;
        let childArray = [];
        if(this.states !== undefined) {
            for (let state in this.states) {
                let childObj = {};
                childObj.name = this.states[state].name;
                childObj.children = [];
                childArray.push(childObj);
            }
        }
        treeObj.children = childArray;
        return treeObj;
    }

    getAngle() {
        return (this.startAngle + this.endAngle)/2 +3 * Math.PI/2;
    }

    cartX (r) {
        //console.log(this.startAngle);
        return r * Math.cos(((this.startAngle + this.endAngle)/2 + 3 * Math.PI/2));

    }
    cartY (r) {
        return r * Math.sin(((this.startAngle + this.endAngle)/2 + 3 * Math.PI/2));
    }
}

class Node extends D3Object {
    constructor (nodeData) {
        super(nodeData.node_type);
        let node = this;
        this.clicked = 0; // for detect cycle
        this.side = 0; // for detect cycle
        this.sites = nodeData.node_sites.map(function(siteData, i) {
            let site = new Site(siteData, node);
            site.setId(i);
            return site;
        });

        /* make fake empty site if no site is present on agent */
        if (nodeData.node_sites.length === 0) {
                this.sites = [];
                let siteData = {};
                siteData.site_name = null;
	        siteData.site_type = "port";
                siteData.site_links = [];
                siteData.site_states = [];
                let site = new Site(siteData,this);
                site.setId(-1);
                this.sites.push(site);
        }

    }

    setId(id) {
        this.id = id;
    }

    generateIdMap() {
        this.idHashMap = {};
        /* generate id hashmap */
        let node = this;
        this.sites.forEach(function(site) {
                //console.log(node.idHashMap);
                node.idHashMap[site.id] = site;
        });
    }

    sortSites() {
        this.generateIdMap();
        this.sites.sort(function(a,b) {
            if (a.label > b.label)
                return 1;
            else if (a.label < b.label)
                return -1;
            else
                return 0;
        });
    }

    listSites() {


        return this.sites;

    }

    getSite(siteId) {
        if (this.idHashMap !== undefined)
            {return this.idHashMap[siteId];}
        return this.sites[siteId];

    }

}

class State {
    constructor(name, site) {
        this.name = name;
        this.site = site;
    }

}

class SiteLink {
    constructor(nodeId, siteId) {
        this.nodeId = nodeId;
        this.siteId = siteId;
    }

    equals(otherLink) {
        return otherLink.nodeId == this.nodeId && otherLink.siteId == this.siteId;
    }

}

/* stores multiple datastorage objects for the mixture/snapshot data */
class DataWareHouse {
    constructor(data) {
        this.snapshot_file = data.snapshot_file;
        this.event = data.snapshot_event;
        this.time = data.snapshot_time;
        this.tokens = data.snapshot_tokens;
        this.snapshot = [];
        for (let map in data.snapshot_agents) {
            let sitegraph = {};
            sitegraph.id = parseInt(map);
            sitegraph.count = data.snapshot_agents[map][0];
            sitegraph.mapData = new DataStorage(data.snapshot_agents[map][1], true);
            this.snapshot.push(sitegraph);
        }

        //console.log(this.snapshot);
    }

    /* generates tree for for each individual species in the snapshot */
    getSpeciesTree(id) {
        let species = this.snapshot[id];
        let tree = {};
        tree.name = "mixture"+id;
        let obj = {};
        tree.children = [];
        for (let i in species.mapData.data) {
            obj[species.mapData.data[i].label] = (obj[species.mapData.data[i].label] || 0) + 1;
        }

        for (let item in obj) {
            let tempObj = {};
            tempObj.name = item;
            tempObj.size = obj[item];
            tree.children.push(tempObj);
        }
        return tree;
    }

    /* generates tree for snapshot visualization based on the count or number of agent within the species */
    generateTreeData(count) {
        this.treeData = {};
        this.treeData.name = "root";
        this.treeData.children = [];
        for (let child in this.snapshot) {
            let children = {};
            children.name = "mixture" + this.snapshot[child].id;
            children.data = this.snapshot[child].mapData;
            //console.log(this.snapshot[child]);

            children.count = this.snapshot[child].count;
            children.size = this.snapshot[child].mapData.data.length;

            this.treeData.children.push(children);
        }
    }

}

/* used to store sitegraph/contactmap information */
class DataStorage {
    constructor(data, isSnapshot) {
        if(!data) {return null;} /* check that there is data*/
        this.nodeCount = data.length;
        this.isSnapshot = isSnapshot;
        var tempData = [];
	var i = 0;
        data.forEach(function (line) {
	    line.forEach(function (nodeData) {
		let node = new Node(nodeData);
		node.setId(i);
		i++;
		tempData.push(node)
	    })
	});
        this.data = tempData;

    }

    getNode(id) {
        if (this.idHashMap !== undefined)
            {return this.idHashMap[id];}
        return this.data[id];
    }

    listNodes() {
        return this.data;
    }

    getSite(node, s) {
        return this.getNode(node).getSite(s);
    }

    generateIdMap() {
        this.idHashMap = {};
        /* generate id hashmap */
        let nodes = this;
        this.data.forEach(function(node) {
                nodes.idHashMap[node.id] = node;
        });
    }

    sortNodes() {
        this.generateIdMap();
        this.data.sort(function(a,b) {
            if (a.label > b.label)
                return 1;
            else if (a.label < b.label)
                return -1;
            else
                return 0;
        });
    }

    constructHierarchy() {
        let siteList = [];
        let data = this.data;
       // console.log(data);
        for (let key in data) {
            let sites = data[key].listSites();
            for (let key in sites) {
                siteList.push(sites[key]);
            }
        }

        let hierarchyBase = [];
        for (let sites in siteList)  {
            //console.log(siteList[sites]);
            let entry = {};
            entry.name = 'root.' + siteList[sites].getAgent().label + '.' + siteList[sites].label;
            entry.parentId = siteList[sites].getAgent().id;
            entry.id = siteList[sites].id;
            let links = siteList[sites].listLinks();
            let linkArray = [];

            for (let link in links) {
                //console.log(data);
                let site = this.getSite(links[link].nodeId, links[link].siteId);
                linkArray.push('root.' + site.getAgent().label + '.' + site.label);
            }

            entry.links = linkArray;
            hierarchyBase.push(entry);
        }

        /* taken from Michael Bostock's hierarchy edge bundling example at https://bl.ocks.org/mbostock/7607999  */
        let map = {};

        function find(name, data) {
            let node = map[name], i;
            if (!node) {
            node = map[name] = data || {name: name, children: []};
            if (name.length) {
                node.parent = find(name.substring(0, i = name.lastIndexOf(".")));
                node.parent.children.push(node);
                node.key = name.substring(i + 1);
            }
            }
            return node;
        }

        hierarchyBase.forEach(function(d) {
            find(d.name, d);
        });

        return d3.hierarchy(map[""]);
    }

    generateForceDirectedNodes() {
        let nodes = this.data;
        let nodeList = [];
        for (let node in nodes) {
            let nodeObj = {};
            nodeObj.id = nodes[node].id;
            nodeObj.sites = nodes[node].sites;
            nodeObj.label = nodes[node].label;
            nodeList.push(nodeObj);
        }
        //console.log(nodeList);
        return nodeList;
    }

    addForceDirectedSites(node) {

    }

    removeForceDirectedSites(node) {

    }

    generateForceDirectedLinks() {
        let nodes = this.data;
        let linkList = [];
        for (let node in nodes) {
            let currentNode = nodes[node];
            for (let site in currentNode.sites) {
                let currentSite = currentNode.sites[site];
                for (let link in currentSite.links) {
                    let currentLink = currentSite.links[link];
                    let linkObj = {};
                    linkObj.source = currentNode.id;
                    linkObj.sourceSite = currentSite.id;
                    linkObj.target = currentLink.nodeId;
                    linkObj.targetSite = currentLink.siteId;
                    linkObj.value = 0.5;
                    linkList.push(linkObj);
                }
            }
        }
        //console.log(linkList);
        return linkList;
    }

    addForceDirectedLinks(node) {

     }

    removeForceDirectedLinks(node) {

    }

    packageLinks(nodes) {

        let map = {},
            links = [];

        // Compute a map from name to node.
        nodes.forEach(function(d) {
            //console.log(d);
            map[d.data.name] = d;
        });

        // For each import, construct a link from the source to target node.
        nodes.forEach(function(d) {
            if (d.data.links) d.data.links.forEach(function(i) {
            links.push(map[d.data.name].path(map[i]));
            });
        });

        //console.log(links);
        return links;
    }

    sortSites() {
        this.listNodes().map(function(node) {node.sortSites();});
    }
}
