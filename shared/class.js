/*jshint esversion: 6*/

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
        this.contentDimension = new Dimension(0, 0);
    }
    
    anchor(point) {
        //console.log(this.dimension);
        let newPoint = this.dimension.toPoint();
        newPoint.scale(-0.5);
        newPoint.translate(point, translate.ADD);
        return newPoint;
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
        this.links = siteData.site_links.map(function(link)
            { 
                return new SiteLink(link[0],link[1]); 
            });
        this.agent = agent;
        this.states = siteData.site_states.map(function(state) {
                return new State(state);
            });
        this.currentState = null;
        this.startAngle = 0;
        this.endAngle = 0;
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

    generateTreeObj() {
        let treeObj = {};
        treeObj.name = this.label;
        console.log(this.states);
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
        super(nodeData.site_node_name);
        let node = this;
        this.sites = nodeData.site_node_sites.map(function(siteData, i) {
            let site = new Site(siteData, node);
            site.setId(i); 
            return site;
        });
        
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
    constructor(name) {
        this.name = name;
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

class DataStorage {
    constructor(data, isSnapshot) {
        if(!data) {return null;} /* check that there is data*/
        this.nodeCount = data.length;
        this.isSnapshot = isSnapshot;
        var tempData = [];
        Object.keys(data).forEach(function(nodeData, i) {
            let node = new Node(data[nodeData]);
            node.setId(i);
            tempData.push(node);
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

    sortSites() {
        this.listNodes().map(function(node) {node.sortSites();});
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

        return links;
    }
}

