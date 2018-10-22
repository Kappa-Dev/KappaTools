"use strict"

class StoryRendering {
    constructor(id) {
	this.id = "#"+id;
    }
    setData(json) {
	this.clearData();

	var story = JSON.parse(json);
	var topdiv = d3.select(this.id);
	var g = new dagreD3.graphlib.Graph().setGraph({});

	story.nodes.forEach(function (n) {
	    g.setNode(n.id, { label: n.directives.label,
			      style: "fill: #fff; stroke: #333"});
	});
	story.edges.forEach(function (e) {
	    g.setEdge(e.source, e.target, { style : "stroke: #333; fill: #fff; stroke-width: 1.5px;" });
	});
	

	// Create the renderer
	var render = new dagreD3.render();

	// Set up an SVG group so that we can translate the final graph.
	var svg = topdiv.append("svg"),
	    inner = svg.append("g");

	// Run the renderer. This is what draws the final graph.
	render(inner, g);

	// Center the graph
        let margin = { top: 10, right: 10, bottom: 10, left: 10 };
        let w = topdiv.node().getBoundingClientRect().width - margin.left - margin.right;
	let h = topdiv.node().getBoundingClientRect().height - margin.top - margin.bottom;
	svg.attr("width",w);
	svg.attr("height", h);
	var xCenterOffset = (w - g.graph().width) / 2;
	inner.attr("transform", "translate(" + xCenterOffset + ", 0)");
    }
    redraw() {

    }
    clearData() {
    	d3.select(this.id).selectAll("*").remove();
    }
}
