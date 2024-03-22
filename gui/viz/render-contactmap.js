/*jshint esversion: 6*/

class Layout {
    constructor(contactMap, dimension, margin) {
        this.contactMap = contactMap;
        this.dimension = dimension;
        this.margin = margin ||
            { top: 10, right: 10, bottom: 10, left: 10 };
    }
}

class ContactMap {
    constructor(id, coloring) {
        this.id = '#' + id;
        this.coloring = coloring || {};
    }

    redraw(tip = true) {
        let map = this;
        let root = d3.select(this.id);
        let margin = { top: 10, right: 10, bottom: 10, left: 10 };
        let w = d3.select(this.id).node().getBoundingClientRect().width - margin.left - margin.right;
        let h = d3.select(this.id).node().getBoundingClientRect().height - margin.top - margin.bottom - 34.5 - 34 - 15;
        if (map.data) {
	    map.clearData();
            let layout = new Layout(map, new Dimension(w, h), margin);
            let renderer = new Render(root, layout, tip, this.coloring, this.selection, this.nodemapping);
            renderer.generateLinks();
            renderer.render();
        }
    }

    setData(response) {
        let map = this;
        map.data = new DataStorage(JSON.parse(response),0);
        map.data.sortNodes();
        map.data.sortSites();
	map.redraw();
    }

    // for snapshot contact maps
    setParsedData(data, id, selection, nodemapping) {
        let map = this;
        this.selection = selection;
        this.nodemapping = nodemapping;
        map.data = data;
        map.data.sortNodes();
        map.data.sortSites();
        map.redraw(false);
    }

    clearData() {
        d3.select(this.id).selectAll("svg").remove();
        d3.select(this.id).select(".tooltip").remove();
        d3.select(this.id).select(".toolbox").remove();
        // d3.selectAll(".contact-tooltip").remove();
    }
}

class Render {
    constructor(root, layout, tip = true, coloring, selection, nodemapping) {
        this.root = root;
        let renderer = this;
        let width = layout.dimension.width;
        let height = layout.dimension.height;
        this.layout = layout;
        this.centerZoom = true;
        this.coloring = coloring;
        this.selection = selection;
        this.nodemapping = nodemapping;

        /* create svg to draw contact maps on */
        let svgWidth = width +
            this.layout.margin.left +
            this.layout.margin.right;
        let svgHeight = height +
            this.layout.margin.top +
            this.layout.margin.bottom;
        let container = this.root
            .append("svg")
            .attr("xmlns:xlink", "http://www.w3.org/1999/xlink")
            .attr("class", "svg-group")
            .attr("id", "map-container")
            .attr("preserveAspectRatio", "xMinYMin meet")
            .attr("viewBox", "0 0 " + svgWidth + " " + svgHeight );

        let svg = this.svg = container.append('g')
            .attr('transform', 'translate(' + [width/2, height/2] + ')')
            .append('g');

        container.style("transform-origin", "50% 50% 0");

        function zoomed() {
            let centerX = getBoundingBoxCenterX(container);
            let centerY = getBoundingBoxCenterY(container);
            let containerWidth = (2 * centerX) ;
            let containerHeight = (2 * centerY) ;
            if (renderer.centerZoom) {
                d3.event.transform.x = centerX - containerWidth / 2;
                d3.event.transform.y = centerY - containerHeight / 2;
            }

            svg.attr('transform', d => d3.event.transform );
	    //            console.log("zoom called");
            if (d3.event.sourceEvent && d3.event.sourceEvent.constructor.name === "MouseEvent") {
                renderer.centerZoom = false;
            }

        }

        let zoom = d3.zoom()
        //.center([svgWidth / 2, svgHeight / 2])
            .scaleExtent([0.5, 10])
            .on('zoom', zoomed);

        container.call(zoom);

        this.siteList = [];
        let data = this.layout.contactMap.data;

        this.hierarchy = data.constructHierarchy();

        for (let key in data.listNodes()) {
            let sites = data.listNodes()[key].listSites();
            for (let key in sites) {
                this.siteList.push(sites[key]);
            }
        }
        if (tip) {
            let tip = this.tip = new UIManager(this);

            /* add behavior for reset zoom button */
            d3.select("#resetZoomButton").on("click", reset);
        }
        function reset() {
            //console.log("reset");
            container.transition().duration(750)
                .call(zoom.transform, d3.zoomIdentity);
            renderer.centerZoom = true;
        }

        this.agentNames = layout.contactMap.data
            .listNodes()
            .map( node => node.label );

        this.cycleDetect = false;
        this.toggleLines = false;
    }

    render() {
        //console.log(this.selection);
        //console.log(this.nodemapping);
        this.width = this.layout.dimension.width;
        this.height = this.layout.dimension.height;
        this.radius = Math.min(this.width, this.height)/2;
        this.padding = this.radius/6;
        this.nodew = this.radius/6;
        this.outerRadius = this.radius - this.padding;
        this.innerRadius = this.radius - this.nodew - this.padding;
        this.siteRadius = 1.5 * this.radius/(this.siteList.length) > 6 ? 6: 1.5 * this.radius/(this.siteList.length);
        this.renderLinks();
        this.renderDonut();
        this.renderSitetoEdgeLinks();
        this.renderStates();
        //this.checkStateCollusion(110);
    }
    rerender() {
        this.rerenderNodes();
        this.rerenderLinks();
    }

    generateLinks() {
        let data = this.layout.contactMap.data;
        this.siteLinks = [];

        for (let sites in this.siteList) {
            let siteList = this.siteList;
            let links = siteList[sites].listLinks();
            for (let link in links) {
                let target = data.getSite(links[link].nodeId, links[link].siteId);
                let source = siteList[sites];
                let linkEdge = {target: target, source: source};
                this.siteLinks.push(linkEdge);
            }
        }
    }

    rerenderNodes() {
        let renderer = this;
        let svg = this.svg;
        if(renderer.cycleDetect === true ) {
            svg.selectAll('.nodeArcPath').style("fill-opacity", opacity.node_hidden);
            return;
        }
        else {
            svg.selectAll('.nodeArcPath')
                .style("fill-opacity", d => { d.clicked = 0; d.side = 0; return opacity.node_normal;})
                .style("stroke-width", 0);
            return;
        }
    }

    rerenderLinks() {
        let renderer = this;
        let svg = this.svg;
        if(renderer.cycleDetect === true ) {
            svg.selectAll('.link').style("stroke-opacity", opacity.line_hidden);
            return;
        }
        else {
            svg.selectAll('.link')
                .style("stroke-width", d => { d.clicked = 0; d.side = 0; return 2;});
            svg.selectAll('.siteText')
                .classed("siteText--normal", true);
            renderer.resetLinksAndEdges();
            return;
        }
    }
    renderLinks() {
        let renderer = this;
        let siteRadius = this.siteRadius;
        let data = this.layout.contactMap.data;
        let layout = this.layout;
        let width = this.width;
        let height = this.height;
        let radius = this.radius;
        let innerRadius = this.innerRadius;

        let svg = this.svg;
        let hierarchy = this.hierarchy;
        let cluster =  d3.cluster()
            .separation( (a, b) =>  1 )
            .size([360, innerRadius - siteRadius + 2]);
        let line = d3.radialLine()
            .curve(d3.curveBundle.beta(0.85))
            .radius( d => d.y )
            .angle( d =>  d.x / 180 * Math.PI );

        cluster(hierarchy);

        let links = svg.selectAll('.link')
            .data(data.packageLinks(hierarchy.leaves()))
            .enter().append("path")
            .each(d => { d.clicked = 0; d.side = 0; d.source = d[0], d.target = d[d.length - 1]; })
            .attr("class", "link")
            .attr("d", line)
            .attr("stroke", "steelblue")
            .attr("stroke-width", 2)
            .attr("fill", "transparent")
            .style("stroke-opacity", opacity.line_normal)
            .on("mouseover", mouseoverLink)
            .on("mouseout", mouseoutLink)
            .on("click", clickLink);

        // transitions
        /*
          .attr("stroke-dasharray", function() {
          let totalLength = this.getTotalLength();
          console.log(this.getTotalLength());
          return totalLength + " " + totalLength ;
          })
          .style("stroke-dashoffset", function() {
          let totalLength = this.getTotalLength();
          console.log(this.getTotalLength());
          return totalLength;
          })
          .classed("offset", true);
        */

        function clickLink(d) {
            if (renderer.cycleDetect) {
                let selectedLink = d;
                let targetNodes = [];
                let clickedLink = d3.select(this);

                let targetTexts = svg.selectAll(".siteText")
		    .filter(d => (d.data.getAgent().id === selectedLink.target.data.parentId &&
                                  d.data.id === selectedLink.target.data.id) ||
			    (d.data.getAgent().id === selectedLink.source.data.parentId &&
                             d.data.id === selectedLink.source.data.id));

                let sideLinks = svg.selectAll(".link")
		    .filter( d => (
			((selectedLink.target.data.parentId === d.source.data.parentId) &&
			 ((selectedLink.target.data.id != d.source.data.id) && (selectedLink.source.data.id != d.target.data.id))) ||
                            ((selectedLink.source.data.parentId === d.source.data.parentId) &&
                             (selectedLink.source.data.id != d.source.data.id) && ((selectedLink.target.data.id != d.target.data.id) || (selectedLink.target.data.parentId !== d.target.data.parentId))) ||
                            ((selectedLink.source.data.parentId == d.target.data.parentId) &&
                             (selectedLink.source.data.id != d.target.data.id) && (selectedLink.target.data.id != d.source.data.id)) ||
                            ((selectedLink.target.data.parentId == d.target.data.parentId) &&
                             (selectedLink.target.data.id != d.target.data.id) && ((selectedLink.source.data.id != d.source.data.id) || (selectedLink.source.data.parentId !== d.source.data.parentId)))
                    ));
                targetNodes.push(d.target.data.parentId);
                targetNodes.push(d.source.data.parentId);

                targetNodes = dedup(targetNodes);
                let nodes = svg.selectAll(".nodeArcPath").filter( d => targetNodes.includes(  d.data.id ) );

                if(!selectedLink.clicked) {
                    nodes
                        .style("stroke-width", d => { d.clicked += 1; return 5; });
                    clickedLink
                        .attr("stroke-width", d => { d.clicked = 1; return 8;} )
                        .style("stroke-opacity", 1);
                    sideLinks
                        .attr("stroke-width", d => { d.side += 1; if (d.clicked) return 8; else return renderer.toggleLines ? 2: 8;  } )
                        .style("stroke-opacity", d => { if (d.clicked) return opacity.line_highlight; else return renderer.toggleLines ? opacity.line_hidden: opacity.line_side; } )
                        .style("stroke", "grey");
                    targetTexts
                        .classed("siteText--normal", false);

                } else {
                    nodes.style("stroke-width", d => { d.clicked -= 1; return 2; });

                    clickedLink
                        .attr("stroke-width", d => { d.clicked = 0; return 8;} )
                        .style("stroke-opacity", opacity.line_normal);

                    sideLinks
                        .attr("stroke-width", d => { d.side -= 1; if( d.clicked) return 8; else return 2; } )
                        .style("stroke-opacity", d => { if (d.clicked) return opacity.line_highlight; else return renderer.toggleLines ? opacity.line_hidden: opacity.line_side; } );

                    targetTexts
                        .classed("siteText--normal", true);
                }
            }
        }

        function mouseoverLink(d) {
            if (renderer.cycleDetect) {
                svg.selectAll(".link").style("stroke-opacity", d => { if(d.clicked) return opacity.line_highlight; else if(d.side) return opacity.line_side; else return opacity.line_hidden; });
                svg.selectAll(".selfLoop").style("stroke-opacity", opacity.line_hidden);
                svg.selectAll(".siteText").filter(".siteText-normal").attr("opacity", opacity.line_normal);

                if(!d.clicked) {
                    let selectedLink = d;
                    let targetNodes = [];
                    let sideNodes = [];
                    let sideLinks = svg.selectAll(".link").filter( d => (
                        ((selectedLink.target.data.parentId === d.source.data.parentId) &&
                         ((selectedLink.target.data.id != d.source.data.id) && (selectedLink.source.data.id != d.target.data.id))) ||
                            ((selectedLink.source.data.parentId === d.source.data.parentId) &&
                             (selectedLink.source.data.id != d.source.data.id) && ((selectedLink.target.data.id != d.target.data.id) || (selectedLink.target.data.parentId !== d.target.data.parentId))) ||
                            ((selectedLink.source.data.parentId == d.target.data.parentId) &&
                             (selectedLink.source.data.id != d.target.data.id) && (selectedLink.target.data.id != d.source.data.id))  ||
                            ((selectedLink.target.data.parentId == d.target.data.parentId) &&
                             (selectedLink.target.data.id != d.target.data.id) && ((selectedLink.source.data.id != d.source.data.id) || (selectedLink.source.data.parentId !== d.source.data.parentId)))
                    )
                                                                 );

                    let targetTexts = svg.selectAll(".siteText").filter( d => (d.data.getAgent().id === selectedLink.target.data.parentId &&
                                                                               d.data.id === selectedLink.target.data.id) ||
									 (d.data.getAgent().id === selectedLink.source.data.parentId &&
									  d.data.id === selectedLink.source.data.id) );

                    targetNodes.push(d.target.data.parentId);
                    targetNodes.push(d.source.data.parentId);
                    for (let link in sideLinks.data()) {
                        sideNodes.push(sideLinks.data()[link].target.data.parentId);
                        sideNodes.push(sideLinks.data()[link].source.data.parentId);
                    }

                    targetNodes = dedup(targetNodes);
                    //console.log(targetNodes);
                    let snodes = svg.selectAll(".nodeArcPath").filter( d => sideNodes.includes(  d.data.id ) );
                    snodes
                        .style("stroke-width", 5)
                        .style("stroke", d =>  d.data.color.darker(0.5) )

                    let nodes = svg.selectAll(".nodeArcPath").filter( d => targetNodes.includes(  d.data.id ) );
                    nodes
                        .style("stroke-width", 15)
                        .style("stroke", d =>  d.data.color.darker(0.5) )
                        .style("fill-opacity", opacity.node_normal);

                    sideLinks
                        .style("stroke-opacity", d => d.clicked ? opacity.line_highlight: opacity.line_side)
                        .style("stroke-width", 8)
                        .style("stroke", d => d.clicked ? "steelblue": "grey");

                    d3.select(this)
                        .style("stroke-opacity", opacity.line_highlight)
                        .style("stroke-width", 8)
                        .style("stroke", "steelblue");

                    targetTexts
                        .attr("opacity", 1)
                        .style("font-weight", "bold")
                        .style("font-size", "150%");
                }
            }
        }

	function mouseoutLink(d) {
            if (renderer.cycleDetect) {
                renderer.resetLinksAndEdges();
            }
        }
    }

    renderSitetoEdgeLinks() {
        let siteRadius = this.siteRadius;
        let siteLine = this.svg.selectAll('.site')
            .data(this.siteList)
            .enter().append('g')
            .attr('class','site')
            .attr('transform', d => 'rotate(' + d.getAngle() * 180/Math.PI + ')');

        siteLine.append('line')
            .attr('opacity',  d => {if (d.label === null) return 0; return 0.5;} )
            .attr('stroke', function(d) { return d.agent.color; })
            .attr('stroke-dasharray', [2,2])
            .attr('stroke-width', 2)
            .attr('x1', this.innerRadius + siteRadius)
            .attr('x2', this.outerRadius)
            .style('pointer-events', 'none');
    }

    renderStates() {
        let paddingSite = calculateTextWidth("110%") * 1.5;
        let siteRadius = this.siteRadius;
        let lineScale = this.radius/60;
        let lineLength;
        let width = this.width;
        let height = this.height;
        let outerRadius = this.outerRadius;
        let siteNum = this.siteNum = this.siteList.length;
        for (let sIndex in this.siteList) {
            let site = this.siteList[sIndex];
            let textLength = this.svg.selectAll(".siteText").filter( function(d) { return d.data.id === site.id && d.data.agent.id === site.agent.id ;}).node() ?
                this.radius/30 + this.svg.selectAll(".siteText").filter( function(d) { return d.data.id === site.id && d.data.agent.id === site.agent.id ;}).node().getComputedTextLength() + paddingSite :
                this.radius/30 + paddingSite;

            let gState = this.svg.selectAll('.stateLink')
                .data(this.siteList);

            let stateLine = gState.enter()
                .merge(gState)
                .filter( d => d.label === site.label && d.agent.label === site.agent.label )
                .append('g')
                .attr('class','stateLink')
                .attr('id', function(d) { return "stateLink" + "a" + d.agent.id + "s" + d.id; })
                .attr('opacity', 0);

            if (site.states.length > 0) {
                stateLine.append('line')
                    .attr('transform', d => 'rotate(' + d.getAngle() * 180/Math.PI + ')')
                    .attr('stroke', d => site.agent.color.darker() )
                    .attr('stroke-width', 2)
                    .attr('x2', d => {
                        lineLength = lineScale * 6 * site.states.length + lineScale/8 * siteNum;
                        if (lineLength < textLength) {
                            lineLength = textLength + paddingSite;
                        }
                        else if(lineLength < this.radius/4) {
                            lineLength = this.radius/4;
                        }
                        d.x2 = outerRadius + siteRadius + lineLength;
                        return d.x2;
                    })
                    .attr('x1', d => {
                        let lineStart;
                        lineStart = this.outerRadius + siteRadius + textLength;
                        if (textLength > lineLength) {
                            lineStart = textLength;
                        }
                        d.x1 = lineStart;
                        return d.x1;
                    })
                    .transition();

                let stateArc = d3.arc()
                    .outerRadius(this.outerRadius + siteRadius + lineLength + 1.5 )
                    .innerRadius(this.outerRadius + siteRadius + lineLength)
                    .startAngle( d => d.startAngle )
                    .endAngle( d => d.endAngle )
                    .padAngle(Math.PI/(10 * siteNum));

                stateLine.append('path')
                    .attr("d", stateArc)
                    .attr("id", d => "stateArc" + "a" + site.agent.id + "s" + site.id)
                    .style("fill", d => site.agent.color.darker() );

                for ( let state in site.states ) {
                    if (state) {
                        stateLine.append("text")
                            .attr("text-anchor", d => {
                                if ( (d.startAngle + d.endAngle + 3 * Math.PI ) / 2 < 5 * Math.PI/2) {
                                    return  "start"; }
                                else
                                    return "end";
                            })
                            .attr("class", "stateText")
                            .attr("id", "stateText" + "a" + site.agent.id + "s" + site.id + "id" + state)
                            .attr('dominant-baseline', "central")
                            .style("fill", d => site.agent.color.darker() )
                            .style('font-size', d => {
                                let textSize = 2000 / (siteNum/4 + 6 * site.states.length - this.radius/25);
                                textSize = textSize > 110 ? 110: textSize;
                                return textSize + "%";
                            })
                            .attr("transform", d => {
                                let r = (outerRadius + siteRadius + lineLength  + 10);
                                let offset = (d.endAngle - d.startAngle - 2 * Math.PI/(10 * siteNum))/(site.states.length);
                                let angle = d.startAngle + Math.PI/(10 * siteNum) + 3/2 * Math.PI + (state) * offset + offset/2;
                                let newX = r * Math.cos(angle) ;
                                let newY = r * Math.sin(angle) ;
                                if ( ((d.startAngle + d.endAngle + 3 * Math.PI ) / 2 >= 5 * Math.PI/2)) {
                                    angle += Math.PI;
                                }
                                return "translate(" + newX + "," + newY + ") rotate(" + angle * 180/Math.PI + ")";
                            })
                            .text(site.states[state].name);
                    }
                }
            }
        }
    }
    /*
      checkStateCollusion(textSize) {
      /* collision detection from stackoverflow https://stackoverflow.com/questions/19681724/how-to-avoid-labels-overlapping-in-a-d3-js-pie-chart */
    /*            let prev;
                  let count = 0;
                  this.svg.selectAll(".stateText").each( function(d,i) {
                  if (i > 0) {
                  let thisbb = this.getBoundingClientRect(),
                  prevbb = prev.getBoundingClientRect();

                  if(!(thisbb.right < prevbb.left ||
                  thisbb.left > prevbb.right ||
                  thisbb.bottom < prevbb.top ||
                  thisbb.top > prevbb.bottom)) {
                  d3.select(this).style("font-size", ( textSize - 10 )+ "%");
                  d3.select(prev).style("font-size", ( textSize - 10 )+ "%");
                  count += 1;
                  }
                  }
                  prev = this;
                  });
                  if (count > 0)
                  this.checkStateCollusion(textSize - 10);
		  }
    */

    renderDonut() {
        let siteRadius = this.siteRadius;
        let siteList = this.siteList;
        let layout = this.layout;
        let width = this.width;
        let height = this.height;
        let radius = this.radius;
        let outerRadius = this.outerRadius;
        let innerRadius = this.innerRadius;
        let paddingSite = calculateTextWidth("150%") * 2;
        let renderer = this;
        let tip = this.tip;

        let cluster = d3.cluster();
        // .size([360, innerRadius - 2.5]);

        let nodeArc = d3.arc()
            .outerRadius(outerRadius)
            .innerRadius(innerRadius)
            .padAngle(Math.PI/(renderer.siteList.length * 4));

        let nodeTextArc = d3.arc()
            .outerRadius((outerRadius + innerRadius) / 2)
            .innerRadius((outerRadius + innerRadius) / 2);

        let siteArc = d3.arc()
            .outerRadius(outerRadius + siteRadius + paddingSite)
            .innerRadius(outerRadius + siteRadius) ;

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
        //console.log(data);
        let svg = this.svg;
        let gNode = svg.selectAll(".nodeArc")
            .data(node(data.listNodes()))
            .enter().append("g");

        let gSite = svg.selectAll(".siteArc")
            .data(site(siteList))
            .enter().append("g");

	/* Cleanup coloring */
	for (let member in renderer.coloring)
	    if (data.listNodes().find( (d) => { return d.label === member ; })
	       === undefined)
		delete renderer.coloring[member];
        /* render node arcs paths */
        gNode.append("path")
            .attr("class", "nodeArcPath")
            .attr("d", nodeArc)
        //.attr("id", function(d,i) { return "nodeArc_" + i;})
            .style("fill", (d,i) => {
                d.clicked = 0;
                if (renderer.coloring[d.data.label]) {
                    d.data.color = renderer.coloring[d.data.label];
                }
                else {
                    d.data.color =
			d3.rgb(d3.schemeCategory20[Object.keys(renderer.coloring).length % 20]).darker(1);
                    renderer.coloring[d.data.label] = d.data.color;
                }
                return d.data.color.brighter(1.5);})
            .style("opacity", (d,i) => {
                if (!this.nodemapping || !this.selection)
                    return opacity.node_normal;
                if (i === this.nodemapping.get(this.selection.label)) {
                    return opacity.node_normal;
                }
                return opacity.node_hidden;
            })
            .on("mouseover", mouseoverNode)
            .on("mouseout", mouseoutNode);

        /* render invisible text arc path */
        gNode.append("path")
            .attr("d", nodeTextArc)
            .attr("class", "nodeTextArc")
            .attr("id", (d,i) => { return "nodeTextArc_" + i;})
            .style("fill", "none")
            .style("stroke-width", 0)
            .style("stroke", "transparent");

        /* render node text */
        gNode.append("text")
            .append("textPath")
            .attr('dominant-baseline', "central")
            .attr("xlink:href",  (d,i) => { return "#nodeTextArc_" + i;})
            .attr("startOffset", (d) => {
                if ( (d.startAngle + d.endAngle + 3 * Math.PI ) / 2 < 2 * Math.PI) {
                    return  "25%"; }
                else if ( (d.startAngle + d.endAngle + 3 * Math.PI ) / 2 >=  2 * Math.PI &&  (d.startAngle + d.endAngle + 3 * Math.PI ) / 2 < 3 * Math.PI) {
                    return "75%";
                }
                else
                    return "25%";
            })
            .style("text-anchor", "middle")
            .style('font-size', "110%")
            .style("fill", (d,i) => { return d.data.color.darker(2);})
            .style('pointer-events', 'none')
            .text( d => {
                let label = d.data.label;
                label = label.length > 10 ? label.substring(0,8): label;
                if (d.endAngle - d.startAngle < label.length/50) {
                    return "";
                }
                return label; });

        /* render site text */
        gSite.append("text")
            .attr("text-anchor", d => {
                if ( (d.startAngle + d.endAngle + 3 * Math.PI ) / 2 < 5 * Math.PI/2) {
                    return  "start"; }
                else
                    return "end"; })
            .attr("class", "siteText siteText--normal")
            .attr('dominant-baseline', "central")
            .attr("transform", d => {
                let xy = siteArc.centroid(d) ;
                let angle = ( d.startAngle + d.endAngle + 3 * Math.PI ) / 2;
                if ( ((d.startAngle + d.endAngle + 3 * Math.PI ) / 2 >= 5 * Math.PI/2)) {
                    angle += Math.PI;
                }
                return "translate(" + xy + ") rotate(" + angle * 180/Math.PI + ")";
            })
            .style('font-size', "110%")
            .style("fill", (d, i) => { return d.data.agent.color; })
            .text( d => {
                let label = d.data.label;
                if (label === null) return;
                d.data.startAngle = d.startAngle;
                d.data.endAngle = d.endAngle;
                label = label.length > 10 ? label.substring(0,8): label;
                return label; });

        let gSiteNodes = gSite.data(siteList);

        // render inner sites
        /*
          gSiteNodes
          .append("circle")
          .attr('cx', function(d) {
          return d.cartX(innerRadius);
          })
          .attr('cy', function(d) {
          return d.cartY(innerRadius);
          })
          .attr('r', siteRadius)
          .attr("fill", "none")
          .on("mouseover", mouseoverInnerSite)
          .on("mouseout", mouseoutInnerSite);
        */
        // render outer sites
        gSiteNodes
            .append("circle")
            .attr('class', 'outerSite')
            .attr('cx', d => d.cartX(innerRadius))
            .attr('cy', d => d.cartY(innerRadius))
            .attr('r', d => { if (d.label === null) return 0; return siteRadius;})
            .attr("stroke", d => d.agent.color)
            .attr("fill", (d,i) => {
                d.currentColor = d.agent.color;
                return d.agent.color;
            })
            .on("click", clickSite)
            .on("mouseover", mouseoverSite)
            .on("mouseout", mouseoutSite);

        // render self loops
        var selfLoopLine = d3.line()
            .x( d => d.x )
            .y( d => d.y )
            .curve(d3.curveBundle.beta(1));

        gSiteNodes
            .filter(d => {for (let link in d.links) {
                if (d.links[link].siteId === d.id && d.links[link].nodeId === d.getAgent().id)
                    return true;
                if (link === d.links.length - 1)
                    return false;
            }})
            .append("path")
            .attr("d", d => {
                let pathObj = d.generateSelfLoopPath(innerRadius);
                return selfLoopLine(pathObj);
            })
            .attr("class", "selfLoop")
            .style("stroke", "steelblue")
            .style("stroke-opacity", 0.4)
            .attr("fill", "none")
            .style("stroke-width", 2);

        function mouseoverNode(d) {
            let event = this;
            let node = d;
            let sites = node.data.listSites();
            let targetSites = [];
            d3.select(this)
                .style("stroke-width", 5)
                .style("stroke", () =>  node.data.color.darker(1) );

            svg.selectAll(".link").style("stroke-opacity", d => { if (d.clicked) return opacity.line_highlight; else return opacity.line_hidden; });
            svg.selectAll(".selfLoop").style("stroke-opacity", 0.1);
            svg.selectAll(".siteText").filter(".siteText--normal").attr("opacity", 0.4);
            svg.selectAll(".nodeArcPath").style("fill-opacity", opacity.node_hidden);

            let links = svg.selectAll(".link").filter( d => {
                let site = {};
                if(d.target.data.parentId === node.data.id || d.source.data.parentId === node.data.id) {
                    site.id = d.target.data.id ;
                    site.parentId = d.target.data.parentId;
                    targetSites.push(site);
                }
                return d.target.data.parentId === node.data.id;
            });

            let selfLoops = svg.selectAll(".selfLoop").filter( d => d.getAgent().id === node.data.id );

            let targetNodes = dedup(targetSites.map( d => data.getNode(d.parentId) ));
            let nodes = svg.selectAll(".nodeArcPath").filter( d => targetNodes.includes(  d.data ));
            nodes
                .style("fill-opacity", opacity.node_normal);

            targetSites = targetSites.map( d => data.getSite(d.parentId, d.id ));

            let targetTexts = svg.selectAll(".siteText").filter( d => targetSites.includes( d.data ) );

            targetTexts
                .attr("opacity", 1)
                .style("font-weight", "bold")
                .style("font-size", "150%");
            links
                .style("stroke", d => data.getNode(d.source.data.parentId).color.brighter() )
                .style("stroke-width", 8)
                .style("stroke-opacity", opacity.line_highlight);
            //.raise();

            selfLoops
                .style("stroke", node.data.color.brighter())
                .style("stroke-width", 8)
                .style("stroke-opacity", opacity.line_highlight);

            if (renderer.tip) {
                tip.showNode(node);
          }
        }

        function mouseoutNode(d) {
            let node = d;
            let sites = node.data.listSites();
            let targetSites = [];

            let nodes = svg.selectAll(".nodeArcPath").filter( d => !d.clicked );
            if (!renderer.cycleDetect) {
                nodes
                    .style("fill-opacity", opacity.node_normal)
                    .style("stroke-width", 0);
            }
            let links = svg.selectAll(".link");
            /*.filter(function(d) {
              let site = {};
              if(d.target.data.parentId === node.data.id || d.source.data.parentId === node.data.id) {
              site.id = d.target.data.id ;
              site.parentId = d.target.data.parentId;
              targetSites.push(site);
              }
              return d.target.data.parentId === node.data.id || d.source.data.parentId === node.data.id;
              });  */

            svg.selectAll(".selfLoop").style("stroke-opacity", 0.4);
            let selfLoops = svg.selectAll(".selfLoop").filter(function(d) {
                return d.getAgent().id === node.data.id;
            });

            //targetSites = targetSites.map(function(d) { return data.getSite(d.parentId, d.id); });
            renderer.resetLinksAndEdges();

            if (renderer.tip) {
                tip.hide();
            }
        }
    /*
            function mouseoverInnerSite(d) {
                if(d.links.length > 0) {
                    let event = this;
                    let innerSite = d;
                    let targetSites = [];
                    if(d == null) {
                        return;
                    }

                    svg.selectAll(".link").style("stroke-opacity", opacity.line_hidden);
                    svg.selectAll(".selfLoop").style("stroke-opacity", opacity.line_hidden);
                    svg.selectAll(".siteText").filter(".siteText--normal").attr("opacity", 0.4);
                    let links = svg.selectAll(".link").filter( d => {
                        let siteS = {};
                        let siteT = {};
                        if(d.target.data.parentId === innerSite.getAgent().id && d.target.data.id === innerSite.id) {
                            siteT.id = d.target.data.id ;
                            siteT.parentId = d.target.data.parentId;
                            siteS.id = d.source.data.id ;
                            siteS.parentId = d.source.data.parentId;
                            targetSites.push(siteS);
                            targetSites.push(siteT);
                        }
                        return d.target.data.parentId === innerSite.getAgent().id && d.target.data.id === innerSite.id;
                    });

                    let selfLoops = svg.selectAll(".selfLoop").filter( d => {
                        return d.getAgent().id === innerSite.getAgent().id && d.id === innerSite.id;
                    });

                    links
                        .style("stroke", d => data.getNode(d.source.data.parentId).color.brighter() )
                        .style("stroke-width", 8)
                        .style("stroke-opacity", opacity.line_highlight);

                    selfLoops
                        .style("stroke", innerSite.getAgent().color.brighter())
                        .style("stroke-width", 8)
                        .style("stroke-opacity", opacity.line_highlight);

                    targetSites = targetSites.map( d => data.getSite(d.parentId, d.id) );
                    let targetTexts = svg.selectAll(".siteText").filter( d => targetSites.includes( d.data ) );

                    targetTexts
                        .attr("opacity", 1)
                        .style("font-weight", "bold")
                        .style("font-size", "150%");
                }
            }

            function mouseoutInnerSite(d) {
                if(d.links.length > 0) {
                    let event = this;
                    let innerSite = d;
                    let links = innerSite.links;
                    let targetSites = [];
                    renderer.resetLinksAndEdges();
                }
            }
    */
        function mouseoverSite(d) {
            let site = d;
            //console.log(this);
            if (renderer.tip) {
                tip.showSite(site);
            }

            renderer.adjustState(site, this, false, true, true);
            if(d.links.length > 0) {
                let event = this;
                let innerSite = d;
                let targetSites = [];
                if(d == null) {
                    return;
                }

                svg.selectAll(".link").style("stroke-opacity", opacity.line_hidden);
                svg.selectAll(".selfLoop").style("stroke-opacity", opacity.line_hidden);
                svg.selectAll(".siteText").filter(".siteText--normal").attr("opacity", 0.4);
                let links = svg.selectAll(".link").filter( d => {
                    let siteS = {};
                    let siteT = {};
                    if(d.target.data.parentId === innerSite.getAgent().id && d.target.data.id === innerSite.id) {
                        siteT.id = d.target.data.id ;
                        siteT.parentId = d.target.data.parentId;
                        siteS.id = d.source.data.id ;
                        siteS.parentId = d.source.data.parentId;
                        targetSites.push(siteS);
                        targetSites.push(siteT);
                    }
                    return d.target.data.parentId === innerSite.getAgent().id && d.target.data.id === innerSite.id;
                });

                let selfLoops = svg.selectAll(".selfLoop").filter( d => {
                    return d.getAgent().id === innerSite.getAgent().id && d.id === innerSite.id;
                });

                links
                    .style("stroke", d => data.getNode(d.source.data.parentId).color.brighter() )
                    .style("stroke-width", 8)
                    .style("stroke-opacity", opacity.line_highlight);

                selfLoops
                    .style("stroke", innerSite.getAgent().color.brighter())
                    .style("stroke-width", 8)
                    .style("stroke-opacity", opacity.line_highlight);

                targetSites = targetSites.map( d => data.getSite(d.parentId, d.id) );
                let targetTexts = svg.selectAll(".siteText").filter( d => targetSites.includes( d.data ) );

                targetTexts
                    .attr("opacity", 1)
                    .style("font-weight", "bold")
                    .style("font-size", "150%");
            }
        }

        function mouseoutSite(d) {
            let site = d;
            if (renderer.tip) {
                tip.hide();
            }
            renderer.adjustState(site, this, true, true, false);
            if(d.links.length > 0) {
                let event = this;
                let innerSite = d;
                let links = innerSite.links;
                let targetSites = [];
                renderer.resetLinksAndEdges();
            }
        }

        function clickSite(d) {
            let originalColor = d.agent.color;
            let site = d;
            if (site.currentColor === originalColor) {
                site.clicked += 1;
                site.currentColor = "white";
                if(site.clicked === 1) {
                    renderer.adjustState(site, this, false, false, false);
                }
            }
            else {
                site.clicked = 0;
                site.currentColor = originalColor;
                renderer.adjustState(site, this, true, false, false);
            }
        }
    }

    resetLinksAndEdges() {
        let svg = this.svg;
        let renderer = this;
        svg.selectAll('.link')
            .style("stroke", d => !d.clicked && d.side ? "grey" : "steelblue")
            .style("stroke-width", d => {
                if(d.clicked)
                    return 8;
                else {
                    if (d.side && !renderer.toggleLines)
                        return 8;
                    else
                        return 2;
                }
            })
            .style("stroke-opacity",d => {
                if (renderer.cycleDetect) {
                    if(d.clicked)
                        return opacity.line_highlight;
                    else if(d.side && !renderer.toggleLines)
                        return opacity.line_side;
                    else
                        return opacity.line_hidden;
                }
                else {
                    return opacity.line_normal;
                }
            });
        svg.selectAll(".selfLoop")
            .style("stroke", "steelblue")
            .style("stroke-width", 2)
            .style("stroke-opacity", d =>  {if (renderer.cycleDetect) return opacity.line_hidden; else return opacity.line_normal; });
        svg.selectAll(".siteText").filter(".siteText--normal").attr("opacity", 1)
            .style("font-weight", "normal")
            .style("font-size", "110%");

        if(renderer.cycleDetect) {
            svg.selectAll(".nodeArcPath")
                .style("fill-opacity", d => { if ( d.clicked ) return opacity.node_normal; else return opacity.node_hidden;} )
                .style("stroke-width", d => { if ( d.clicked ) return 5; else return 0; } );
        }
    }

    adjustState(site, circle, hide, text, textMove) {
        let paddingSite = calculateTextWidth("110%") * 1.5;
        let siteRadius = this.siteRadius;
        let radius = this.radius;
        let outerRadius = this.outerRadius;
        let siteNum = this.siteNum;
        let lineScale = radius/60;
        d3.select(circle).style("fill", () => site.currentColor)
            .style("opacity", 1)
            .attr("r", () => {
                if(!hide) {
                    return siteRadius * 2; }
                else {
                    return siteRadius;
                }
            }).raise();

        let siteText = this.svg.selectAll(".siteText").filter( d => { return d.data.label === site.label && d.data.agent.label === site.agent.label; });
        let transform = getTransform(siteText);

        if (text) {
            siteText
                .style("font-weight", () => {
                    if (hide)
                        return "normal";
                    else
                        return "bold";})
                .style("font-size", () => {
                    if (hide)
                        return "110%";
                    else
                        return "150%";})
                .attr("transform", d => {
		    let angle = d.data.getAngle();
                    let newX;
                    let newY;
                    if(textMove) {
                        newX = (parseFloat(transform.translate[0]) + ( 1.5 * siteRadius  ) * Math.cos(angle));
                        newY = (parseFloat(transform.translate[1]) + ( 1.5 * siteRadius  ) * Math.sin(angle));
                    }
                    else {
                        newX = (parseFloat(transform.translate[0]) - ( 1.5 * siteRadius  ) * Math.cos(angle));
                        newY = (parseFloat(transform.translate[1]) - ( 1.5 * siteRadius  ) * Math.sin(angle));
                    }
                    return "translate(" +  newX +
                        ","  + newY +
                        ") rotate(" + transform.rotate + ")" ;
                });
        }

        let stateLine = d3.select("#stateLink" + "a" + site.agent.id + "s" + site.id);
        let textLength = siteText.node().getComputedTextLength() + paddingSite + this.radius/30;
        let lineLength;
        if (!hide) {
            adjustStateLines(true);
            stateLine.attr("opacity", 1);
        }
        else {
            adjustStateLines(false);
            if (!site.clicked) {
                stateLine.attr('opacity', 0);
            }
        }

        function adjustStateLines(mouseover) {
            stateLine.selectAll("line")
                .attr('x2', d => {
                    lineLength = lineScale * 6 * site.states.length + lineScale/8 * siteNum;
                    if (lineLength < textLength) {
                        lineLength = textLength + paddingSite;
                    }
                    else if(lineLength < radius/4) {
                        lineLength = radius/4;
                    }
                    return mouseover ? outerRadius + (mouseover ? 2: 1) * siteRadius + lineLength: d.x2;
                })
                .attr('x1', d => {
                    let lineStart;
                    lineStart = outerRadius + 2 * siteRadius + textLength;
                    if (textLength > lineLength) {
                        lineStart = textLength + siteRadius;
                    }
                    return mouseover ? lineStart: d.x1;
                });

            let stateArc = d3.arc()
                .outerRadius(outerRadius + (mouseover ? 2: 1) * siteRadius + lineLength + 1.5 )
                .innerRadius(outerRadius + (mouseover ? 2: 1) * siteRadius + lineLength)
                .startAngle( site.startAngle )
                .endAngle( site.endAngle )
                .padAngle(Math.PI/(10 * siteNum));

            let arcPath = d3.select("#stateArc" + "a" + site.agent.id + "s" + site.id);
            arcPath.attr("d", stateArc);

            for (let state in site.states) {
                let stateText = d3.selectAll("#stateText" + "a" + site.agent.id + "s" + site.id + "id" + state);
                stateText
                    .attr("transform", d => {
                        let r = (outerRadius + (mouseover ? 2: 1) * siteRadius + lineLength  + 10);
                        let offset = (d.endAngle - d.startAngle - 2 * Math.PI/(10 * siteNum))/(site.states.length);
                        let angle = d.startAngle + Math.PI/(10 * siteNum) + 3/2 * Math.PI + (state) * offset + offset/2;
                        let newX = r * Math.cos(angle) ;
                        let newY = r * Math.sin(angle) ;
                        if ( ((d.startAngle + d.endAngle + 3 * Math.PI ) / 2 >= 5 * Math.PI/2)) {
                            angle += Math.PI;
                        }
                        return "translate(" + newX + "," + newY + ") rotate(" + angle * 180/Math.PI + ")";
                    });
            }
        }
    }
}
