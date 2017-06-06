/*jshint esversion: 6*/

class Layout {

    constructor(contactMap, dimension, margin) {
        this.contactMap = contactMap;
        this.dimension = dimension;
        this.margin = margin ||
            { top: 10, right: 10,
            bottom: 10, left: 10 };

    }
    /*
    circleNodes(){
        let w = this.dimension.width;
        let h = this.dimension.height;
        let nodes = contactMap.data.listNodes();

        console.log(this.dimension);
        nodes.forEach(function(node,index,nodes){
            let dx = 0;
            let dy = 0;
            let length = nodes.length;

            if(length > 1){
                let angle = 2*index*Math.PI/length;
                dx = w * Math.cos(angle)/4;
                dy = h * Math.sin(angle)/3;
                console.log("x:" + dx + " y:" + dy);
            }
            nodes[index].absolute = new Point(dx + w/2,
                                              dy + h/2);

            //set static node dimensions for now
            nodes[index].dimension = new Dimension(50,50);

      });
    }



    setNodeDimensions(node,dimensions){
        console.log(dimensions);
        node.contentDimension = new Dimension(dimensions.width, dimensions.height);
    }

    setSiteDimension(site,dimensions){
        site.contentDimension = dimensions.clone();
    }
*/
}


class ContactMap {
    constructor(id, isSnapShot) {
        this.id = '#'+id;
        this.isSnapShot = isSnapShot;
    }
    setData(response) {
        let map = this;
	let root = d3.select(this.id);

        map.data = new DataStorage(JSON.parse(response),0);
        map.data.sortNodes();
        map.data.sortSites();
        let margin = { top: 10, right: 10,
        bottom: 10, left: 10 };
        let w = root.node().getBoundingClientRect().width - margin.left - margin.right;
        let h = w;//window.innerHeight - margin.top - margin.bottom;
        if (map.data) {
	    map.clearData();
            let layout = new Layout(map, new Dimension(w, h), margin);
            let renderer = new Render(root, layout);
            renderer.generateLinks();
            renderer.render();
          }

    }

    clearData() {
        d3.select(this.id).selectAll("svg").remove();
       // d3.selectAll(".contact-tooltip").remove();
    }

}


class Render {
    constructor(root, layout) {
        let width = layout.dimension.width;
        let height = layout.dimension.height;
        this.layout = layout;
        //console.log(layout);
        /* create svg to draw contact maps on */

        let container = root
            .append("svg")
            .attr("class", "svg-group")
            .attr("id", "map-container")
            .attr("width", width +
                            this.layout.margin.left +
                            this.layout.margin.right)
            .attr("height", height +
                            this.layout.margin.top +
                  this.layout.margin.bottom);

         this.svg = container.append('g')
                .attr('transform', 'translate(' + [width/2, height/2] + ')')
                .append('g');

        container.call(d3.zoom().on('zoom', () => this.svg.attr('transform', d3.event.transform)));
        container.call(d3.drag().on('drag', () => this.svg.attr('transform', 'translate(' + d3.event.x + ',' + d3.event.y +')')));

        this.agentNames = layout.contactMap.data
                              .listNodes()
                              .map(function(node){
                                return node.label;
                              });

        this.siteList = [];
        let data = this.layout.contactMap.data;

        this.hierarchy = data.constructHierarchy();

        //console.log(this.hierarchy);
        //console.log(data);
        for (let key in data.listNodes()) {
            let sites = data.listNodes()[key].listSites();
            for (let key in sites) {
                this.siteList.push(sites[key]);
            }
        }



    }

    render() {
        let width = this.layout.dimension.width;
        let height = this.layout.dimension.height;

        let radius = Math.min(width, height)/2;
        this.paddingw = 0;
        this.nodew = radius/6;
        this.statew = radius/12;
        this.sitew = radius/8;
        this.outerRadius = radius - this.nodew - this.statew;
        this.innerRadius = radius - this.nodew - this.statew - this.sitew;
        //console.log("rendering");
        this.renderDonut();
        this.renderLinks();
        this.renderSitetoEdgeLinks();
    }

    generateLinks() {
        let data = this.layout.contactMap.data;
        this.siteLinks = [];

        for (let sites in this.siteList) {
            let siteList = this.siteList;
            let links = siteList[sites].listLinks();
            //console.log(links);
            for (let link in links) {
                //console.log(data.getNode(links[link].nodeId).getSite(links[link].siteId));
                //console.log(siteList[sites]);
                let target = data.getSite(links[link].nodeId, links[link].siteId);
                let source = siteList[sites];
                //console.log(target);
                let linkEdge = {target: target, source: source};
                //linkEdge.addData(target, source);
                this.siteLinks.push(linkEdge);
            }
        }
    }

    renderLinks() {
        let data = this.layout.contactMap.data;
        let layout = this.layout;
        let width = layout.dimension.width;
        let height = layout.dimension.height;

        let radius = Math.min(width, height)/2;
        let nodew = radius/6;
        let statew = radius/12;
        let sitew = radius/8;
        let innerRadius = radius - nodew - statew - sitew;

        let svg = this.svg;
        let hierarchy = this.hierarchy;
        let cluster =  d3.cluster()
            .separation(function(a, b) { return 1; })
            .size([360, innerRadius]);
        let line = d3.radialLine()
            .curve(d3.curveBundle.beta(0.85))
            .radius(function(d) { return d.y; })
            .angle(function(d) { return d.x / 180 * Math.PI; });

        cluster(hierarchy);

        //console.log(data.packageLinks(hierarchy.leaves()));
        let links = svg.selectAll('.links')
            .data(data.packageLinks(hierarchy.leaves()))
            .enter().append("path")
            .each(function(d) { d.source = d[0], d.target = d[d.length - 1]; })
            .attr("class", "link")
            .attr("d", line)
            .attr("stroke", "steelblue")
            .attr("stroke-opacity", 0.4);

    }

    renderSitetoEdgeLinks() {
        let circleRadius = 5;
        let siteLine = this.svg.selectAll('.site')
            .data(this.siteList)
        .enter().append('g')
            .attr('class','site')
            .attr('transform', d => 'rotate(' + d.getAngle() * 180/Math.PI + ')');

        siteLine.append('line')
            .attr('opacity', 0.5)
            .attr('stroke','white')
            .attr('stroke-dasharray', [2,2])
            .attr('stroke-width', 2)
            .attr('x1', this.innerRadius + circleRadius)
            .attr('x2', this.outerRadius - circleRadius);
    }

    calculateTextWidth(size) {
        let svg = d3.select("svg");
        let text = svg.append("text")
	        .attr("x", 10)
	        .attr("y", 30)
            .style('font-size', size)
	        .text("a");
        let tWidth = svg.select("text").node().getComputedTextLength();
        text.remove();
        return tWidth;

    }

    renderDonut() {
        let siteList = this.siteList;
        let layout = this.layout;
        let width = layout.dimension.width;
        let height = layout.dimension.height;

        let radius = Math.min(width, height)/2;
        let nodew = radius/6;
        let statew = radius/12;
        let sitew = radius/8;
        let outerRadius = radius - nodew - statew;
        let innerRadius = radius - nodew - statew - sitew;
        let paddingSite = this.calculateTextWidth(20) * 2;
        let renderer = this;

        let c20 = d3.scaleOrdinal(d3.schemeCategory20);
        let cluster = d3.cluster();
           // .size([360, innerRadius - 2.5]);

        let nodeArc = d3.arc()
                    .outerRadius(outerRadius)
                    .innerRadius(innerRadius)
                    .padAngle(Math.PI/renderer.siteList.length/4);

        let nodeTextArc = d3.arc()
                    .outerRadius((outerRadius + innerRadius) / 2)
                    .innerRadius((outerRadius + innerRadius) / 2);

        let siteArc = d3.arc()
                    .outerRadius(outerRadius)
                    .innerRadius(outerRadius + paddingSite);


        let node = d3.pie()
                    .sort(null)
                    .value(function(d) {
                        return d.listSites().length;
                    });


        let site = d3.pie()
                    .sort(null)
                    .value(function(d) {
                        return 1;
                    });


        let data = this.layout.contactMap.data;

        let svg = this.svg;
        let gNode = svg.selectAll(".nodeArc")
                    .data(node(data.listNodes()))
                    .enter().append("g");


        let gSite = svg.selectAll(".siteArc")
                    .data(site(siteList))
                    .enter().append("g");

        /* render node arcs paths */
        gNode.append("path")
            .attr("d", nodeArc)
            //.attr("id", function(d,i) { return "nodeArc_" + i;})
            .style("fill", function(d,i) {
                d.data.color = d3.rgb(c20(i)).darker(1);
                return c20(i);});


        /* render invisible text arc path */
        gNode.append("path")
            .attr("d", nodeTextArc)
            .attr("id", function(d,i) { return "nodeTextArc_" + i;})
            .style("fill", "transparent");

        gNode.append("text")
            .append("textPath")
            .attr('alignment-baseline', "middle")
            .attr("xlink:href",  function(d,i) { return "#nodeTextArc_" + i;})
           // .attr("transform", function(d) { //set the label's origin to the center of the arc
           //     return "translate(" + nodeArc.centroid(d) + ")";
           // })
            .attr("startOffset", function (d) {
                if ( (d.startAngle + d.endAngle + 3 * Math.PI ) / 2 < 2 * Math.PI) {
                    return  "25%"; }
                else if ( (d.startAngle + d.endAngle + 3 * Math.PI ) / 2 >=  2 * Math.PI &&  (d.startAngle + d.endAngle + 3 * Math.PI ) / 2 < 3 * Math.PI) {
                    return "75%";
                }
                else
                    return "25%";
            })
            .style("text-anchor", "middle")
			.style('font-size', '20px')

            //.attr('text-anchor', 'middle')
            .style("fill", "black")
            .text(function(d) {
                let label = d.data.label;
                label = label.length > 10 ? label.substring(0,8): label;
                return label; });



        /* render site text */
        gSite.append("text")
            .attr("text-anchor", function (d) {
                if ( (d.startAngle + d.endAngle + 3 * Math.PI ) / 2 < 5 * Math.PI/2) {
                    return  "start"; }
                else
                    return "end"; })
            .attr('alignment-baseline', "middle")
            .attr("transform", function(d) {
                let xy = siteArc.centroid(d) ;
                let angle = ( d.startAngle + d.endAngle + 3 * Math.PI ) / 2;
                if ( ((d.startAngle + d.endAngle + 3 * Math.PI ) / 2 >= 5 * Math.PI/2)) {
                    angle += Math.PI;
                }
                //xy[0] -= renderer.calculateTextWidth(20) * Math.cos(angle) / 10;
                //xy[1] -= renderer.calculateTextWidth(20) * Math.sin(angle) / 10;
                //console.log("angle: " + angle + " label: " + d.data.label );
                return "translate(" + xy + ") rotate(" + angle * 180/Math.PI + ")";
            })
			.style('font-size', 20)
            //.attr('text-anchor', 'middle')
			//.attr("xlink:href",function(d,i){return "#nodeArc_"+i;})
            .style("fill", function(d, i) { return d.data.agent.color; })
             //place the text halfway on the arc
            .text(function(d) {
                let label = d.data.label;
                d.data.startAngle = d.startAngle;
                d.data.endAngle = d.endAngle;
                label = label.length > 10 ? label.substring(0,8): label;
                return label; });

        //console.log(siteList);

        /* render dots at center of arc */
        gSite
            .data(siteList)
            .append("circle")
            .attr('cx', function(d) {
                return d.cartX(innerRadius);
            })
            .attr('cy', function(d) {
                return d.cartY(innerRadius);
            })
            .attr('r', 5)
            .attr("fill", function(d) {
                //console.log(d);
                return d.agent.color;
            });

         gSite
            .data(siteList)
            .append("circle")
            .attr('cx', function(d) {
                return d.cartX(outerRadius);
            })
            .attr('cy', function(d) {
                return d.cartY(outerRadius);
            })
            .attr('r', 5)
            .attr("stroke", function(d) {
                return d.agent.color;
            })
            .attr("fill", function(d,i) {
                d.currentColor = d.agent.color;
                return d.agent.color;

            })
            .on("click", click);


            /*
            let site = this.siteList[sites];
            //console.log(site.getStates());

            let state = d3.pie()
                    .sort(null)
                    .value(function(d) {
                        return 1;
                    })
                    .startAngle(site.startAngle)
                    .endAngle(site.endAngle)
                    .padAngle(0.01);

            let stateArc = d3.arc()
                .outerRadius(radius - 10 )
                .innerRadius(radius - nodew + paddingw);

            /* draw state arc paths
            let gState = svg.selectAll(".stateArc")
                    .data(state(site.getStates()))
                    .enter().append("g");

            gState.append("path")
            .attr("d", stateArc)
            .attr("id", function(d,i) { return "stateArc_" + site.label + "_" + i;})
            .style("fill", function(d,i) { return c20(i);});

            gState.append("text")
            .attr("transform", function(d) { //set the label's origin to the center of the arc
                return "translate(" + stateArc.centroid(d) + ")";
            })
			.style('font-size', '20px')
            .attr('text-anchor', 'middle')
			//.attr("xlink:href",function(d,i){return "#nodeArc_"+i;})
            .style("fill", "black")
             //place the text halfway on the arc
            .text(function(d) {
                let label = d.data.name;
                label = label.length > 10 ? label.substring(0,8): label;
                return label; });
        */
        function click (d) {
            let originalColor = d.agent.color;
            let data = site;
            /* render states */
            /*let root = d3.hierarchy(d.generateTreeObj());

            let treeData = treemap(root);
            let nodes = treeData.descendants(),
                links = treeData.descendants().slice(1);

            let link = svg.selectAll(".state_link")
                .data(links)
                .enter().append("path")
                .attr("class", "state_link")
                .attr("d", function(d) {
                    console.log(d);
                    return "M" + d.y + "," + d.x
                        + "C" + (d.parent.y + 100) + "," + d.x
                        + " " + (d.parent.y + 100) + "," + d.parent.x
                        + " " + d.parent.y + "," + d.parent.x;
                });
            */
            console.log(d);
            let link = svg.selectAll(".stateLink")
                .data(d)
                .enter().append("path")
                .attr("class", "stateLink");
            d3.select(this).style("fill", function() {
                if (site.currentColor == originalColor)
                    site.currentColor = "white";
                else
                    site.currentColor = originalColor;
                return site.currentColor;
            } );
        }

    }
}
