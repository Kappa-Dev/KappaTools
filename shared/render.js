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
        console.log(this.id);
        map.data = new DataStorage(JSON.parse(response),0);
        map.data.sortNodes();
        map.data.sortSites();
        let margin = { top: 10, right: 10,
        bottom: 10, left: 10 };
        let w = root.node().getBoundingClientRect().width - margin.left - margin.right;
        
        let h = 3 * w / 4;//window.innerHeight - margin.top - margin.bottom;
        console.log(w, h);
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
        d3.select(this.id).select(".tooltip").remove();
        d3.select(this.id).select(".stateButtonDiv").remove();
       // d3.selectAll(".contact-tooltip").remove();
    }

}


class Render {
    constructor(root, layout) {
        this.root = root;
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

        for (let key in data.listNodes()) { 
            let sites = data.listNodes()[key].listSites();
            for (let key in sites) {
                this.siteList.push(sites[key]);
            }
        }

    let tip = this.tip = new UIManager(this);
        
    }

    render() {
        this.width = this.layout.dimension.width;
        this.height = this.layout.dimension.height;
        this.radius = Math.min(this.width, this.height)/2;
        this.padding = this.radius/6; 
        this.nodew = this.radius/6;
        this.outerRadius = this.radius - this.padding;
        this.innerRadius = this.radius - this.nodew - this.padding;
        this.siteRadius = 5;
        this.renderDonut();
        this.renderLinks();
        this.renderSitetoEdgeLinks();
        this.renderStates();
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

    renderLinks() {
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
            .separation(function(a, b) { return 1; })
            .size([360, innerRadius - siteRadius/2]);
        let line = d3.radialLine()
            .curve(d3.curveBundle.beta(0.85))
            .radius(function(d) { return d.y; })
            .angle(function(d) { return d.x / 180 * Math.PI; });

        cluster(hierarchy);
        
        let links = svg.selectAll('.link')
            .data(data.packageLinks(hierarchy.leaves()))
            .enter().append("path")
            .each(function(d) { d.source = d[0], d.target = d[d.length - 1]; })
            .attr("class", "link")
            .attr("d", line)
            .attr("stroke", "steelblue")
            .attr("stroke-width", 2)
            .style("stroke-opacity", 0.4)
            .on("mouseover", mouseoverLinks)
            .on("mouseout", mouseoutLinks);
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
        function mouseoverLinks(d) {
            /*
            console.log("link moused over");
            svg.selectAll(".link").style("stroke-opacity", 0.1);
            svg.selectAll(".selfLoop").style("stroke-opacity", 0.1);
            svg.selectAll(".siteText").attr("opacity", 0.4);
            d3.select(this)
                .attr("opacity", 0.8)
                .style("stroke", function(d) {
                    return data.getNode(d.source.data.parentId).color.brighter();
                });
            */
        }

        function mouseoutLinks(d) {
            /*
            svg.selectAll(".link")
                .style("stroke-opacity", 0.4)
                .style("stroke", "steelblue");
            svg.selectAll(".selfLoop").style("stroke-opacity", 0.4);
            svg.selectAll(".siteText").attr("opacity", 0.4);
            */
        }
    }

    renderSitetoEdgeLinks() {
        let siteRadius = 5;
        let siteLine = this.svg.selectAll('.site')
            .data(this.siteList)
        .enter().append('g')
            .attr('class','site')
            .attr('transform', d => 'rotate(' + d.getAngle() * 180/Math.PI + ')');

        siteLine.append('line')
            .attr('opacity', 0.5)
            .attr('stroke', function(d) { return d.agent.color; })
            .attr('stroke-dasharray', [2,2])
            .attr('stroke-width', 2)
            .attr('x1', this.innerRadius + siteRadius)
            .attr('x2', this.outerRadius - siteRadius);
    }

    renderStates() {
        let lineLength = this.radius/4;   
        let width = this.width;
        let height = this.height;
        let outerRadius = this.outerRadius;
        for (let sIndex in this.siteList) {
            let site = this.siteList[sIndex];
            let textLength = this.radius/30 + this.svg.selectAll(".siteText").filter( function(d) { return d.data.label === site.label && d.data.agent.label === site.agent.label ;}).node().getComputedTextLength() * 1.2;
                let gState = this.svg.selectAll('.stateLink')
                    .data(this.siteList);

                let stateLine = gState.enter() 
                    .merge(gState)
                    .filter( function(d) { return d.label === site.label && d.agent.label === site.agent.label ;} )   
                    .append('g') 
                    .attr('class','stateLink')
                    .attr('id', function(d) { return "stateLink" + d.agent.id + d.id; })
                    .attr('opacity', 0);
                
                if (site.states.length > 0) { 
                    stateLine.append('line')
                        .attr('transform', d => 'rotate(' + d.getAngle() * 180/Math.PI + ')')
                        .attr('stroke','black')
                        .attr('stroke-width', 2)
                        .attr('x1', this.outerRadius + textLength)
                        .attr('x2', this.outerRadius + textLength + (lineLength - textLength))
                        .transition();

                    let stateArc = d3.arc()
                            .outerRadius(this.outerRadius + textLength + (lineLength - textLength) + 2 )
                            .innerRadius(this.outerRadius + textLength + (lineLength - textLength))
                            .startAngle(function(d) { return d.startAngle; 
                            })
                            .endAngle(function(d) { return d.endAngle;
                            })
                            .padAngle(Math.PI/200);
            
                    
                    stateLine.append('path')
                        .attr("d", stateArc)
                        .style("fill", "black");

                    
                    
                    for ( let state in site.states ) {
                        if (state) {
                            stateLine.append("text")
                                .attr("text-anchor", function (d) {
                                    if ( (d.startAngle + d.endAngle + 3 * Math.PI ) / 2 < 5 * Math.PI/2) { 
                                        return  "start"; }
                                    else 
                                        return "end"; 
                                    })
                                .attr("class", "stateText")
                                .attr('alignment-baseline', "middle")
                                .style("fill", "black")
                                .style('font-size', '110%')
                                .attr("transform", function(d) {
                                    let r = (outerRadius + textLength + (lineLength - textLength) + 10);
                                    
                                    let offset = (d.endAngle - d.startAngle)/(site.states.length + 1);
                                    let angle = d.startAngle + 3/2 * Math.PI + (state) * offset + offset;
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
        
        let c20 = d3.scaleOrdinal(d3.schemeCategory20);
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
                    .outerRadius(outerRadius + paddingSite)
                    .innerRadius(outerRadius );
                    

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
                return d3.rgb(c20(i)).brighter(0.5);})
            .on("mouseover", mouseoverNode)
            .on("mouseout", mouseoutNode);
        
        /* render invisible text arc path */
        gNode.append("path")
            .attr("d", nodeTextArc)
            .attr("id", function(d,i) { return "nodeTextArc_" + i;})
            .style("fill", "transparent");

        /* render node text */
        gNode.append("text")
            .append("textPath")
            .attr('alignment-baseline', "middle")
            .attr("xlink:href",  function(d,i) { return "#nodeTextArc_" + i;})
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
			.style('font-size', "110%")
            .style("fill", function(d,i) { return d.data.color.darker(2);})
            .text(function(d) { 
                let label = d.data.label;
                label = label.length > 10 ? label.substring(0,8): label;
                if (d.endAngle - d.startAngle < label.length/50) {
                    return "";
                }
                return label; });


        
        /* render site text */
        gSite.append("text")
            .attr("text-anchor", function (d) {
                if ( (d.startAngle + d.endAngle + 3 * Math.PI ) / 2 < 5 * Math.PI/2) { 
                    return  "start"; }
                else 
                    return "end"; })
            .attr("class", "siteText")
            .attr('alignment-baseline', "middle")
            .attr("transform", function(d) {
                let xy = siteArc.centroid(d) ;
                let angle = ( d.startAngle + d.endAngle + 3 * Math.PI ) / 2;
                if ( ((d.startAngle + d.endAngle + 3 * Math.PI ) / 2 >= 5 * Math.PI/2)) {
                    angle += Math.PI;
                } 
                return "translate(" + xy + ") rotate(" + angle * 180/Math.PI + ")";
            })
			.style('font-size', "110%")
            .style("fill", function(d, i) { return d.data.agent.color; })
            .text(function(d) { 
                let label = d.data.label;
                d.data.startAngle = d.startAngle;
                d.data.endAngle = d.endAngle;
                label = label.length > 10 ? label.substring(0,8): label;
                return label; });
        
        let gSiteNodes = gSite.data(siteList);

        // render inner sites
        gSiteNodes
            .append("circle")
            .attr('cx', function(d) {
                return d.cartX(innerRadius);
            })
            .attr('cy', function(d) {
                return d.cartY(innerRadius);
            })
            .attr('r', siteRadius)
            .attr("fill", function(d) {
                return d.agent.color; 
            })
            .on("mouseover", mouseoverInnerSite)
            .on("mouseout", mouseoutInnerSite);

        // render outer sites
        gSiteNodes
            .append("circle")
            .attr('class', 'outerSite')
            .attr('cx', function(d) {
                return d.cartX(outerRadius);
            })
            .attr('cy', function(d) {
                return d.cartY(outerRadius);
            })
            .attr('r', siteRadius)
            .attr("stroke", function(d) { 
                return d.agent.color; 
            })
            .attr("fill", function(d,i) {
                d.currentColor = d.agent.color; 
                return d.agent.color; 

            })
            .on("click", clickSite)
            .on("mouseover", mouseoverSite)
            .on("mouseout", mouseoutSite);
            
        // render self loops
        var selfLoopLine = d3.line()
                        .x(function(d){return d.x;})
                        .y(function(d){return d.y;})
                        .curve(d3.curveBundle.beta(1));

        gSiteNodes
            .filter(function(d) {for (let link in d.links) {return d.links[link].siteId === d.id && d.links[link].nodeId === d.getAgent().id; }})
            .append("path")
            .attr("d", function(d) {
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
                .style("stroke", function() {return node.data.color.darker(1);});

            svg.selectAll(".link").style("stroke-opacity", 0.1);
            svg.selectAll(".selfLoop").style("stroke-opacity", 0.1);
            svg.selectAll(".siteText").attr("opacity", 0.4);
            let links = svg.selectAll(".link").filter(function(d) { 
                let site = {};
                if(d.target.data.parentId === node.data.id || d.source.data.parentId === node.data.id) {
                    site.id = d.target.data.id ;
                    site.parentId = d.target.data.parentId;
                    targetSites.push(site);
                }
                return d.target.data.parentId === node.data.id;
                
            });  
            
            let selfLoops = svg.selectAll(".selfLoop").filter(function(d) { 
                return d.getAgent().id === node.data.id;
            });  

            targetSites = targetSites.map(function(d) { return data.getSite(d.parentId, d.id); });
            let targetTexts = svg.selectAll(".siteText").filter(function(d) { return targetSites.includes( d.data );});
         
            targetTexts
                .attr("opacity", 1)
                .style("font-weight", "bold")
                .style("font-size", "150%");
            links
                .style("stroke", function(d) {
                    return data.getNode(d.source.data.parentId).color.brighter();
                })
                .style("stroke-width", 8)
                .style("stroke-opacity", 0.75);  

            selfLoops
                .style("stroke", node.data.color.brighter())
                .style("stroke-width", 8)
                .style("stroke-opacity", 0.75); 

            tip.show(node);
        
        }

        function mouseoutNode(d) {
            let node = d;   
            let sites = node.data.listSites();
            let targetSites = []; 
            d3.select(this)
                .style("stroke-width", 0)
                .style("stroke", function() {return node.data.color;});  

            
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
            let targetTexts = svg.selectAll(".siteText");
            targetTexts
                .attr("opacity", 1)
                .style("font-weight", "normal")
                .style("font-size", "110%");
            links
                .style("stroke", "steelblue")
                .style("stroke-width", 2)
                .style("stroke-opacity", 0.4);  
            selfLoops
                .style("stroke", "steelblue")
                .style("stroke-width", 2)
                .style("stroke-opacity", 0.4); 

            tip.hide();
        }

        function mouseoverInnerSite(d) {
            let event = this;
            let innerSite = d;;
            let targetSites = [];
            d3.select(this)
                .style("stroke", function() {return innerSite.currentColor.darker(1);});

            svg.selectAll(".link").style("stroke-opacity", 0.1);
            svg.selectAll(".selfLoop").style("stroke-opacity", 0.1);
            svg.selectAll(".siteText").attr("opacity", 0.4);
            let links = svg.selectAll(".link").filter(function(d) { 
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

            let selfLoops = svg.selectAll(".selfLoop").filter(function(d) { 
                return d.getAgent().id === innerSite.getAgent().id;
            });  

            links
                .style("stroke", function(d) {
                    return data.getNode(d.source.data.parentId).color.brighter();
                })
                .style("stroke-width", 8)
                .style("stroke-opacity", 0.75); 

            selfLoops
                .style("stroke", innerSite.getAgent().color.brighter())
                .style("stroke-width", 8)
                .style("stroke-opacity", 0.75); 

            targetSites = targetSites.map(function(d) { return data.getSite(d.parentId, d.id); });
            let targetTexts = svg.selectAll(".siteText").filter(function(d) { return targetSites.includes( d.data );});
         
            targetTexts
                .attr("opacity", 1)
                .style("font-weight", "bold")
                .style("font-size", "150%");
        }

        function mouseoutInnerSite(d) {
            let event = this;
            let innerSite = d;
            let links = innerSite.links;
            let targetSites = [];
            d3.select(this)
                .style("stroke", function() {return innerSite.currentColor;});

            svg.selectAll(".link")
                .style("stroke", "steelblue")
                .style("stroke-width", 2)
                .style("stroke-opacity", 0.4); 
            svg.selectAll(".selfLoop")
                .style("stroke", "steelblue")
                .style("stroke-width", 2)
                .style("stroke-opacity", 0.4);
            svg.selectAll(".siteText").attr("opacity", 1)
                .style("font-weight", "normal")
                .style("font-size", "110%");
            
        }

        function mouseoverSite(d) {
            let site = d;   
            //console.log(this);
            renderer.adjustState(site, this, false, true, true);            
        }

        function mouseoutSite(d) {
            let site = d;           
            renderer.adjustState(site, this, true, true, false);    
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

    adjustState(site, circle, hide, text, textMove) {
        let siteRadius = this.siteRadius;
        let outerRadius = this.outerRadius;
        d3.select(circle).style("fill", function() {                
                return site.currentColor;
            }).attr("r", function() { 
                if(!hide) {
                    return siteRadius * 2.5; }
                else {
                    return siteRadius; 
                }
            });
            
            let siteText = this.svg.selectAll(".siteText").filter(function(d) { return d.data.label === site.label && d.data.agent.label === site.agent.label; });
            let transform = getTransform(siteText);
            
            if (text) {
                siteText
                    .style("font-weight", function() {
                        if (hide) 
                            return "normal";
                        else 
                            return "bold";})
                    .style("font-size", function() {
                        if (hide) 
                            return "110%";
                        else 
                            return "150%";}) 
                    .attr("transform", function(d) {    let angle = d.data.getAngle();
                                                        let newX;
                                                        let newY;
                                                        if(textMove) {
                                                            newX = (parseFloat(transform.translate[0]) + 1.25 * siteRadius  * Math.cos(angle));
                                                            newY = (parseFloat(transform.translate[1]) + 1.25 * siteRadius  * Math.sin(angle));
                                                        }
                                                        else {
                                                            newX = (parseFloat(transform.translate[0]) - 1.25 * siteRadius  * Math.cos(angle));
                                                            newY = (parseFloat(transform.translate[1]) - 1.25 * siteRadius  * Math.sin(angle));
                                                        }
                                                        return "translate(" +  newX +
                                                        ","  + newY +
                                                        ") rotate(" + transform.rotate + ")" ;
                                                    }); 
            }

            let stateLine = d3.select("#stateLink" + site.agent.id + site.id);
            let siteLength = siteText.node().getComputedTextLength() * 1.2 + this.radius/30;

            if (!hide) {
                stateLine.selectAll("line")
                    .attr("x1", function() {return outerRadius + siteLength;} );
                stateLine.attr("opacity", 1);
            }
            else {
                stateLine.selectAll("line")
                .attr("x1", function() {return outerRadius + siteLength;} );
                if (!site.clicked) {
                    stateLine.attr('opacity', 0);   
            }
        }
    }
}