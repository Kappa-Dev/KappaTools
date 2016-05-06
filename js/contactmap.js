/**
 * Dimensions used to store and manipulate length
 * and width.
 **/
class Dimensions{
    constructor(height,width){
        this.height = height;
        this.width = width;
    }
    scale(s){
        return new Dimensions(this.height * s, this.width * s);
    }

    add(dimensions){
        return new Dimensions(this.height + dimensions.height, this.width + dimensions.width);
    }
    update(dimensions){
        this.height = dimensions.height;
        this.width = dimensions.width;
    }
    toPoint(){ return new Point(this.width,this.height); }
};
/**
 * Point used for coordinates.
 */
class Point{
    constructor(x,y){
        this.x = x;
        this.y = y;
    }
    distance(point){
        return Math.sqrt(((this.x - point.x)*(this.x - point.x))
                         +
                         ((this.y - point.y)*(this.y - point.y)));

    }
    magnitude(){
        return this.distance(new Point(0,0));
    }

    translate(point){
        return new Point(this.x + point.x, this.y + point.y);
    }

    scale(scalar){
        return new Point(this.x * scalar, this.y * scalar);
    }
    /**
     * Return a point with unit magnitude.
     */
    normalize(){
        return this.scale(1.0/this.magnitude());
    }

    update(point){
        this.x = point.x;
        this.y = point.y;
    }
    /**
     * Find the nearest neighbor and the penality
     * for not choosing the nearest neighbor.
     */
    nearest(neighbors){
        if(neighbors.length == 0){
            throw "no neighbors"
        }else if (neighbors.length == 1){
            var neighbor = neighbors[0];
            return { nearest : neighbor
                   , penalty : 0
                   , distance : this.distance(neighbor) };
        }else {
            var neighbor = neighbors[0];
            var r = this.nearest(neighbors.slice(1));
            var distance = this.distance(neighbor);
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
class D3Object {
    constructor(label){
        this.label = label;
        this.absolute = new Point(0,0);
        this.relative = new Point(0,0);
        this.dimensions = new Dimensions(0,0);
    }

    anchor(point){
        var that = this;
        return that.dimensions.toPoint().scale(-0.5).translate(point);
    }
}
/**
 * DTO for Site
 */
class Site extends D3Object {
    constructor(label){
        super(label);
        this.adjacent = {};
        this.targets = [];
    }
}
/**
 * DTO for Agent
 */
class Agent extends D3Object {

    constructor(label){
        super(label);
        this.sites = {};
        this.adjacent = {};
    }

    sitesList(){
        return Object.keys(this.sites).map(function (key) { return this.sites[key] });
    }
}
/**
 * DTO for contact map.
 */
class ContactMap{
    constructor(data){
        var that = this;
        that.state = {};

        data.forEach(function(agent){
            var agent_label = agent["node_name"];
            if(!that.state.hasOwnProperty(agent_label)){
                that.state[agent_label]= new Agent(agent_label);
            };
        });
    }

    agent(a){
        var that = this;
        return that.state[a];
    }
    agentLabels(){
        var that = this;
        var result = Object.getOwnPropertyNames(that.state).sort();
        return result;
    }
    agentList(){
        var that = this;
        return that.agentLabels().map(function (key) { return that.state[key] });
    }

    site(agent,s){
        var that = this;
        var result = that.agent(agent).sites[s];
        return result;
    }
}
/**
 * Contact Layout
 */
class Layout{
    constructor(contactMap,dimensions,margin){
        this.contactMap = contactMap;
        this.dimensions = dimensions
        this.margin = margin || { top: dimensions.height/8,
                                  right: dimensions.width/8,
                                  bottom: dimensions.height/8,
                                  left: dimensions.width/8};
        this.padding = dimensions.scale(0.025);
        this.padding.width = Math.max(this.padding.width,5);
        this.padding.width = Math.min(this.padding.width,20);
        this.padding.height = Math.max(this.padding.height,5);
        this.padding.height = Math.min(this.padding.height,20);
    }
    /* Position agents along a circle */
    circleAgents(){
        var that = this;
        var agents = that.contactMap.agentList();
        agents.forEach(function(agent,index,agents){
            var dx = 0;
            var dy = 0;

            var length = agents.length;
            if(length > 1){
                var angle = 2*index*Math.PI/length;
                dx = that.dimensions.width * Math.cos(angle) / 2;
                dy = that.dimensions.height * Math.sin(angle) / 2;
            }
            agents[index].absolute = new Point(dx + that.dimensions.width/2,
                                               dy + that.dimensions.height/2);
        });
    }

    sitePoint(i,dimensions){
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
    setAgentDimensions(site,dimensions){
        var that = this;
        site.dimensions = that.padding.add(dimensions);
    }
    layout(){
        var that = this;
        that.circleAgents();
    }
}

/**
 * Handle rendering and support for
 * controls
 */
class Render{

    constructor(contactMap,id){
        this.contactMap = contactMap;
        this.root = id?d3.select(id):d3.select('body');
        var bBox = this.root.node().getBoundingClientRect();
        this.layout = new Layout(contactMap,new Dimensions( 2*(bBox.width+bBox.height)/6
                                                          , 3*(bBox.width+bBox.height)/6));
        this.svg = this.root
                       .append('svg')
                       .attr("width", this.layout.dimensions.width
                                    + this.layout.margin.left
                                    + this.layout.margin.right)
                       .attr("height", this.layout.dimensions.height
                                     + this.layout.margin.top
                                     + this.layout.margin.bottom)
                       .append("g")
                       .attr("transform", "translate("
                                        + this.layout.margin.left
                                        + ","
                                        + this.layout.margin.top + ")");
    }

    dragmove(){
    }

    renderAgents(){
        var that = this;
        var drag = d3.behavior
                     .drag()
                     .on("drag", that.dragmove);
        that.layout.circleAgents();
        that.agents = that.svg.selectAll("g")
                          .data(that.contactMap.agentList())
                          .enter()
                          .append("g")
                          .attr("transform",function(d) {
                              return "translate("+d.absolute.x+","+d.absolute.y+")";
                          }).call(drag);

        /* keep proof for alignment checks
        that.agents.append("circle")
                   .attr("class","agent-proof")
                   .attr("cy", 0)
                   .attr("cx", 0)
                   .attr("r", 2);
        */

        that.agents.append("text")
                   .attr("class","agent-text")
                   .style("text-anchor", "middle")
                   .style("alignment-baseline", "middle")
                   .text(function(d){ var label = d.label;
                                      return label; });

        /* datum is map for data */
        that.agents.selectAll(".agent-text").datum(function(d){ that.layout.setAgentDimensions(d,this.getBBox()); return d; });

        that.agents.selectAll(".agent-text")
                   .attr("x", function(d){
                       return d.relative.x; })
                   .attr("y", function(d){
                       return d.relative.y; });

        that.agents.append("rect")
                   .attr("rx", 5)
                   .attr("ry", 5)
                   .attr("class", "agent-rect")
                   .attr("x", function(d){ return d.anchor(d.relative).x; })
                   .attr("y", function(d){ return d.anchor(d.relative).y; })
                   .attr("width",function(d){ return d.dimensions.width; })
                   .attr("height",function(d){ return d.dimensions.height; })
    }
    render(){
        var that = this;
        that.renderAgents();
    }
}

function createContactMap(data,id){
    var contactMap = new ContactMap(data);
    var render = new Render(contactMap,id);
    render.render();
}

function clearContactMap(){
    d3.select("#contact-map").selectAll("*").remove();
}

function drawContactMap(json){
    clearContactMap();
    var contact = JSON.parse(json)["contact_map"];
    createContactMap(contact,"#contact-map");
}
