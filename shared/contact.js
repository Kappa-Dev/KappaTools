/**
 * Dimensions used to store and manipulate length
 * and width.
 **/
function Dimensions(height,width){
    var that = this;

    this.height = height;
    this.width = width;

    this.scale = function(s){
        return new Dimensions(that.height * s, that.width * s);
    }

    this.add = function(dimensions){
        return new Dimensions(that.height + dimensions.height,
                              that.width + dimensions.width);
    }
    this.update = function(dimensions){
        that.height = dimensions.height;
        that.width = dimensions.width;
    }
    this.toPoint = function(){
        return new Point(that.width,that.height);
    }
    this.larger = function(dimensions){
        var result =
        (that.height > dimensions.height)
            &&
            (that.width > dimensions.width);
        return result;
    }
    this.min = function(dimensions){
        var height = (that.height < dimensions.height)?that.height:dimensions.height;
        var width = (that.width < dimensions.width)?that.width:dimensions.width;
        return new Dimensions(height,width);
    }

    this.max = function(dimensions){
        var height = (that.height > dimensions.height)?that.height:dimensions.height;
        var width = (that.width > dimensions.width)?that.width:dimensions.width;
        return new Dimensions(height,width);
    }

    this.area = function(){
        return that.height * that.width;
    }

    this.square =function(){
        var size = Math.max(that.width,that.height);
        var result = new Dimensions(size,size);
        return result;
    }

    this.rectangle = function(){
        var result = (that.height > that.width)?that.square():that;
        return result;
    }
};
Dimensions.clone = function(d){ return new Dimensions(d.height,d.width); }

/**
 * Point used for coordinates.
 */
function Point(x,y){

    var that = this;

    this.x = x;
    this.y = y;

    this.distance = function(point){
        return Math.sqrt(((that.x - point.x)*(that.x - point.x))
                         +
                         ((that.y - point.y)*(that.y - point.y)));
    }
    this.magnitude = function(){
        return that.distance(new Point(0,0));
    }

    this.translate = function(point){
        return new Point(that.x + point.x, that.y + point.y);
    }

    this.scale = function(scalar){
        return new Point(that.x * scalar, that.y * scalar);
    }
    /**
     * Return a point with unit magnitude.
     */
    this.normalize = function(){
        return that.scale(1.0/that.magnitude());
    }

    this.update = function(point){
        that.x = point.x;
        that.y = point.y;
    }
    /**
     * Find the nearest neighbor and the penality
     * for not choosing the nearest neighbor.
     */
    this.nearest = function(neighbors){
        if(neighbors.length == 0){
            throw "no neighbors"
        }else if (neighbors.length == 1){
            var neighbor = neighbors[0];
            return { nearest : neighbor
                   , penalty : 0
                   , distance : that.distance(neighbor) };
        }else {
            var neighbor = neighbors[0];
            var r = that.nearest(neighbors.slice(1));
            var distance = that.distance(neighbor);
            if(distance < r.distance){
            return { nearest : neighbor
                   , penalty : r.distance - distance
                   , distance : distance };
            } else { return r; }
        }
    }
}
/**
 * DTO
 */
function D3Object(label){
    var that = this;

    this.label = label;
    this.absolute = new Point(0,0);
    this.relative = new Point(0,0);
    this.dimensions = new Dimensions(0,0);
    this.contentDimensions = new Dimensions(0,0);

    this.anchor = function(point){
        return that.dimensions
                   .toPoint()
                   .scale(-0.5)
                   .translate(point);
    }
    this.setDimensions = function(dimensions){ that.dimensions = dimensions; }
    this.getDimensions = function(){ return that.dimensions; }
}
/**
 * DTO for Site
 */
function Site(siteData,agent){
    var that = new D3Object(siteData.site_name);
    that.links = siteData.site_links.map(function(link){ return new SiteLink(link[0],link[1]); });
    that.agent = agent;


    that.listLinks = function(){
        return that.links;
    }
    that.getAgent = function(){
        return that.agent;
    }
    return that;
}

function SiteLink(nodeId,siteId){
    this.nodeId = nodeId;
    this.siteId = siteId;
}

/**
 * DTO for Node
 */
function Node(nodeData){
    var that = new D3Object(nodeData.node_name);
    if(nodeData.node_quantity){
        that.node_quantity = nodeData.node_quantity;
    }
    that.sites = nodeData.node_sites.map(function(siteData){
        var site = Site(siteData,that);
        site.node_quantity = nodeData.node_quantity;
        return site;
    });

    that.sitesList = function(){
        return that.sites;
    }
    that.site = function(siteLabel){
        return that.sitesList()[siteLabel];
    }

    that.preferredSize = function(){
        var d = that.sitesList()
                    .reduce(function(acc,site){ return acc.add(site.getDimensions()); }
                           ,that.contentDimensions.scale(1.0));

        return that.contentDimensions.scale(2.0).max(d);
    }
    return that;
}

/**
 * DTO for contact map.
 */
function DataTransfer(data,isSnapshot){
    var that = this;
    this.isSnapshot = isSnapshot;
    that.data = data.map(
        function(nodeData){
            return Node(nodeData);
        });

    this.node = function(id){
        return that.data[id];
    }

    this.nodeList = function(){
        return that.data;
    }

    this.site = function(node,s){
        var result = that.node(node).site(s);
        return result;
    }

    // layout of sites
    this.siteDistance = function(site,point){
        var distances = site.listLinks().map(function(link){
            var nodeLocation = that.node(link.nodeId).absolute;
            return nodeLocation.distance(point);
        });
        var result = distances.reduce(function(a,b){ return a+b }, 0);
        return result;
    }
}

/**
 * Contact Layout
 */
function Layout(contactMap,dimensions,margin){
    var that = this;
    this.contactMap = contactMap;
    this.margin = margin ||
        { top: 10, right: 10,
          bottom: 10, left: 10 };
    this.padding = dimensions.scale(0.025);
    this.padding.width = 10;
        this.padding.height = 10;
    this.dimensions = new Dimensions(dimensions.height - 20,
                                     dimensions.width - 20);
/*
        this.margin = margin || { top: dimensions.height/8,
                                  right: dimensions.width/8,
                                  bottom: dimensions.height/8,
                                  left: dimensions.width/8};
        this.padding = dimensions.scale(0.025);
        this.padding.width = Math.max(this.padding.width,5);
        this.padding.width = Math.min(this.padding.width,20);
        this.padding.height = Math.max(this.padding.height,5);
        this.padding.height = Math.min(this.padding.height,20);
        this.dimensions = new Dimensions(dimensions.height
                                         - this.margin.top
                                         - this.margin.bottom
                                         - this.padding.height,
                                         dimensions.width
                                         - this.margin.left
                                         - this.margin.right
                                         - this.padding.width);
*/


    /* Position nodes along a circle */
  this.circleNodes = function(){
      var that = this;
      var nodes = that.contactMap.nodeList();
      nodes.forEach(function(node,index,nodes){
          var dx = 0;
          var dy = 0;

          var length = nodes.length;
          if(length > 1){
              var angle = 2*index*Math.PI/length;
              dx = that.dimensions.width * Math.cos(angle) / 4;
              dy = that.dimensions.height * Math.sin(angle) / 3;
          }
          nodes[index].absolute = new Point(dx + that.dimensions.width/2,
                                            dy + that.dimensions.height/2);
      });
  }

  this.sitePoint = function(i,dimensions){
      var point = null;
      if(i >= 0 && i < 0.25){           /* 12 oclock */
            point = new Point((-dimensions.width/2)+(dimensions.width*i/0.25)
                              ,dimensions.height/2);
      } else if (i >= 0.25 && i < 0.5){ /* 3 oclock */
          point = new Point(dimensions.width/2
                            ,(dimensions.height/2)+(dimensions.height*(0.25-i)/0.25));
      } else if (i >= 0.5 && i < 0.75){ /* 6 oclock */
          point = new Point((dimensions.width/2)+(dimensions.width*(0.5-i)/0.25)
                            ,-dimensions.height/2);

      } else if (i >= 0.75){
          point = new Point(-dimensions.width/2
                            ,(-dimensions.height/2)+(dimensions.height*(i-0.75)/0.25));
        }
      point.id = i;
      return point;
  }

  this.setNodeDimensions = function(node,dimensions){
      node.contentDimensions = Dimensions.clone(dimensions);
      node.dimensions = that.padding.add(dimensions);
  }
  this.setSiteDimensions = function(site,dimensions){
      node.contentDimensions = Dimensions.clone(dimensions);
      site.dimensions = that.padding.add(dimensions);
  }
  this.layout = function(){
        that.circleNodes();
    }

  this.resizeNodes = function(){
      var nodes = that.contactMap.nodeList()
      var maxDimension = nodes[0].preferredSize();
      var minDimension = maxDimension;
      nodes.forEach(function(node)
                    { var dimension = that.padding.add(node.preferredSize());
                      maxDimension = dimension.max(maxDimension);
                      minDimension = dimension.min(minDimension); });
      // group the size of the nodes
      var numberOfBuckets = 4;
      var delta = maxDimension.add(minDimension.scale(-1.00)).scale(1.00/(numberOfBuckets-1));
      var buckets =  Array.from((new Array(numberOfBuckets)))
          .map(function(o,index){
              return minDimension.add(delta.scale(index));
          });
      nodes.forEach(function(node){
          var preferredSize = node.preferredSize();
          var newSize = preferredSize;
          for(var i = 0;buckets.length < i && preferredSize.larger(buckets[i]);i++){
              newSize = buckets[i];
          }
          var rectangle = newSize.rectangle();
          node.setDimensions(rectangle);
      });
  }
  this.layoutSites = function(){
      var nodes = that.contactMap.nodeList();

      nodes.forEach(function(node){
          var dimensions = node.getDimensions();
          var center = dimensions.toPoint();
          var sites = node.sitesList();
          var n = Math.max(8,sites.length);
          var relative = Array.from(new Array(n)
                                    ,function(x,index){ var point = that.sitePoint(index/n,dimensions);
                                                        return point;
                                                      });
          var absolute = relative.map(function(point){ return node.absolute.translate(point); });

          var distances = sites.map(function(site){
              var distances = absolute.map(function(point,i){
                  var distance = that.contactMap.siteDistance(site,point);
                  return { distance : distance , id : i };
              });
              distances.sort(function(l,r){ return l.distance - r.distance; });
              var result = { site : site ,
                             distances : distances };
                return result;
          });

          while(distances.length > 1){
              // calculate penalty
              distances.forEach(function(calculation){
                  calculation.penalty = calculation.distances[1].distance - calculation.distances[0].distance;
              });
              distances.sort(function(l,r){return r.penalty - l.penalty; });
              // pick minimum
              var  preferred = distances.shift();
              var eviction_id = preferred.distances[0].id;
              // remove preference
              distances.forEach(function(calculation){
                  calculation.distances = calculation.distances.filter(function (d) { return d.id != eviction_id });
              });
              // update with preference
              preferred.site.absolute.update(absolute[eviction_id]);
              preferred.site.relative.update(relative[eviction_id]);
          }
          if(distances.length == 1){
              var preferred = distances.shift();
              var eviction_id = preferred.distances[0].id;
              preferred.site.absolute.update(absolute[eviction_id]);
              preferred.site.relative.update(relative[eviction_id]);
             }
      });
  }
}

/**
 * Handle rendering and support for
 * controls
 */
function Render(id,contactMap){
    var that = this;
    that.contactMap = contactMap;
    that.root = id?d3.select(id):d3.select('body');
    var node = that.root.node();
    var width = Math.max(400, node.offsetWidth);
    var height = Math.max(2*width/3, node.offsetHeight);
    that.layout = new Layout(contactMap,new Dimensions( height, width));
    that.svg = that.root
        .append('svg')
        .attr("class","svg-group")
        .attr("width", that.layout.dimensions.width
              + that.layout.margin.left
              + that.layout.margin.right)
        .attr("height", that.layout.dimensions.height
              + that.layout.margin.top
              + that.layout.margin.bottom);
    createSVGDefs(that.svg);
    that.svg = that.svg.append("g")
                       .attr("transform"
                            , "translate("
                            + that.layout.margin.left
                            + ","
                            + that.layout.margin.top
                            + ")");

    // http://stackoverflow.com/questions/10805184/d3-show-data-on-mouseover-of-circle
//    if (!$(".contact-tooltip").length){
        that.tooltip = that.root
                           .append("div")
                           .attr("class", "contact-tooltip")
                           .style("visibility", "hidden");
//    }
//    that.tooltip = d3.select(".contact-tooltip");
    /* needed to add the stylesheet to the export */


    var agentNames = that.contactMap
        .nodeList()
        .map(function(node){
            return node.label;
        });

    //if (agentNames.length > 10)
    { that.color = hashColor }
    //else
    //{ that.color = d3.scale.category10().domain(agentNames); }
    /* given a node return the fill color */
    that.fill = function(node){
        var color = that.color(node.label).toString();
        return color;
    };

    if(that.contactMap.isSnapshot){
        var min = 0;
        var max = 0;
        that.contactMap.nodeList().forEach(function(node){
            min = (node.node_quantity < min)?node.node_quantity:min;
            max = (node.node_quantity > max)?node.node_quantity:max;
            });

        that.handleMouseOver = function(d,i){
            var background_color = that.fill(d);
            debug(background_color);
            that.tooltip
                .style("background-color", background_color)
                .style("visibility", "visible")
                .text("Quantity : "+d.node_quantity);
        };
        that.handleMouseOut = function(d,i){
            that.tooltip
                .style("visibility", "hidden");
        };
        } else {
            that.handleMouseOver = function(){};
            that.handleMouseOut = function(){};
        }
    this.renderNodes = function(){
        var dragmove = function(d){
            that.updateLinks();
            that.updateSites();
            d.absolute.update(d3.event);
            d3.select(this).attr("transform",
                                 "translate(" + d.absolute.x + "," + d.absolute.y + ")");
        };

        var drag = d3.behavior
                     .drag()
            .on("drag", dragmove);
        that.layout.circleNodes();
        that.svg
            .selectAll(".svg-group")
            .data(that.contactMap.nodeList())
            .enter()
            .append("g")
            .attr("class","node-group")
            .attr("transform",function(d) {
                return "translate("+d.absolute.x+","+d.absolute.y+")";
            })
            .call(drag);

        that.svg.selectAll(".node-group")
            .append("text")
            .attr("class","node-text")
            .style("text-anchor", "middle")
            .style("alignment-baseline", "middle")
            .text(function(d){ return d.label; })

        that.svg.selectAll(".node-group")
            .append("rect")
            .attr("rx", 5)
            .attr("ry", 5)
            .attr("class","node-rect")
            .attr("fill",that.fill)
            .on("mouseover",that.handleMouseOver)
            .on("mouseout",that.handleMouseOut)
            .on("mousemove", function(){
                var event = d3.event;
                var style_top = (event.clientY-10)+"px"; // pageY , clientY , layerY , screenY
                var style_left = (event.clientX+10)+"px";
                return that.tooltip
                           .style("top",style_top)
                           .style("left",style_left);
            });
        /* keep proof for alignment checks
           that.nodes configure using cess
         */
        if(window.level.debug){
            that.svg
                .selectAll(".node-group")
                .append("circle")
                .attr("class","node-proof")
                .attr("cy", 0)
                .attr("cx", 0)
                .attr("r", 2);
        }

        /* datum is map for data */
        that.svg.selectAll(".node-text")
            .datum(function(d){ that
                                .layout
                                .setNodeDimensions(d,this.getBBox());
                                return d; });
        /* render nodes */
        that.svg.selectAll(".node-text")
                   .attr("x", function(d){ return d.relative.x; })
                   .attr("y", function(d){ return d.relative.y; });

        that.svg.selectAll(".node-rect")
                   .attr("x", function(d){ return d.anchor(d.relative).x; })
                   .attr("y", function(d){ return d.anchor(d.relative).y; })
                   .attr("width", function(d){ return d.getDimensions().width; })
                   .attr("height", function(d){ return d.getDimensions().height; });



    }
    this.renderSites = function(){
        that.svg.selectAll(".node-group")
            .each(function(d){
        if(window.level.debug){
            that.svg
                .selectAll("svg-circle")
                .data(d.sitesList())
                .enter()
                .append("circle")
                .attr("class","link-proof")
                .attr("cy", 0)
                .attr("cx", 0)
                .attr("r", 2);
        };
        var sites = d3.select(this)
                      .selectAll("g")
                      .data(d.sitesList())
                      .enter()
                      .append("g")
                      .attr("class","site-group");


       sites.append("rect")
            .attr("class","site-rect")
            .attr("rx", 3)
            .attr("ry", 3)
            .attr("fill",function(d){ return that.fill(d.getAgent()); });

       if(window.level.debug){
                  sites.append("circle")
                       .attr("class","site-proof")
                       .attr("cy", 0)
                       .attr("cx", 0)
                       .attr("r", 2);
       }

              sites.append("text")
              .attr("class","site-text")
              .style("site-anchor", "middle")
              .style("alignment-baseline", "middle")
              .style("text-anchor", "middle")

              .text(function(d){ var label = d.label;
                                 return label; });

               /* size of sites */
              sites.selectAll(".site-text")
                   .datum(function(d){ that
                                       .layout
                                       .setNodeDimensions(d,this.getBBox());
                                return d; });

              that.layout.resizeNodes();
              that.updateSites();

        })

    }
  this.renderLinks = function(){
      var edges = that.contactMap
                        .nodeList()
                        .reduce(function(edges,node,index,nodes){
                            var sites = node.sitesList();
                            sites.forEach(function(source_site){
                                source_site.links.forEach(function(target_reference){
                                    var target_site = that.contactMap
                                                          .site(target_reference.nodeId
                                                               ,target_reference.siteId);
                                    var lineData = { source : source_site.absolute
                                                   , target : target_site.absolute };
                                    edges.push(lineData);
                                });
                            });
                            return edges;
                        },[]);

        edges.forEach(function(lineData){
            var lineFunction = d3.svg.line()
                .x(function(d) { return d.x; })
                .y(function(d) { return d.y; })
                .interpolate("basis");
            that.svg
                .selectAll('path.line')
                .data([[lineData.source , lineData.target ]])
                .enter()
                .append("path")
                .attr("class","link-line")
                .attr("d", lineFunction);
        });
    }

  this.updateSites = function(){
      that.layout.layoutSites();
      that.svg.selectAll(".node-rect")
          .attr("width", function(d){ return d.getDimensions().width; })
          .attr("height", function(d){ return d.getDimensions().height; });

      that.svg.selectAll(".node-rect")
          .attr("x", function(d){ return d.anchor(d.relative).x; })
          .attr("y", function(d){ return d.anchor(d.relative).y; });

      that.svg.selectAll(".site-group")
          .attr("transform",
                function(d){
                    var anchor = d.anchor(d.relative);
                    return  "translate("
                        + anchor.x
                        + ","
                        + anchor.y + ")";
                });

      if(window.level.debug){
          that.svg
              .selectAll(".site-proof")
              .attr("transform",
                    function(d){
                        var anchor = d.relative;
                        return  "translate("
                            + 0
                            + ","
                            + 0 + ")";
                    });
      }

      if(window.level.debug){
          that.svg.selectAll(".link-proof")
              .attr("cx",function(d){ return d.absolute.x; })
              .attr("cy",function(d){ return d.absolute.y; });
      }

      that.svg.selectAll(".site-text")
          .attr("x", function(d){ return d.getDimensions().width/2; })
          .attr("y", function(d){ return d.getDimensions().height/2; });


      that.svg.selectAll(".site-rect")
          .attr("width",
                function(d){ return d.getDimensions().width; })
          .attr("height",
                function(d){ return d.getDimensions().height; });

  };
  this.updateLinks = function(){
      that.layout
          .layoutSites();
      var lineFunction = d3.svg
                             .line()
                             .x(function(d) { return d.x; })
                             .y(function(d) { return d.y; })
                             .interpolate("basis");
        that.svg
            .selectAll('.link-line').attr("d", lineFunction);

    }
  this.render = function(){
        that.renderLinks();
        that.renderNodes();
        that.renderSites();
        that.updateLinks();
    }
}

function ContactMap(id,isSnapshot){
    var that = this;
    this.id = "#"+id;
    this.isSnapshot = isSnapshot;
    this.setData = function(json){
        debug(json);
        var data = JSON.parse(json);
        that.data = data;
        var contactMap = new DataTransfer(data,isSnapshot);
        that.clearData();
        if(that.data.length > 0){
            this.clearData();
            var render = new Render(that.id,contactMap);
            render.render();
        }
    }
    this.setData = wrap(this.setData);

    this.clearData = function(){
        d3.select(that.id).selectAll("svg").remove();
        d3.selectAll(".contact-tooltip").remove();
    }

    this.exportJSON = function(filename){
        var json = JSON.stringify(that.data);
        saveFile(json,"application/json",filename);
    }
    this.exportJSON = wrap(this.exportJSON);

}
