"use strict"

class GraphLogger {
    constructor(id) {
	this.id = "#"+id;
    }

    setData(json) {
	var data = JSON.parse(json);
	var topdiv = d3.select(this.id);

	this.clearData();

	let margin = { top: 5, right: 5, bottom: 5, left: 5 };
	/* multiplying by a factor to account for flex display */
	let width = topdiv.node().getBoundingClientRect().width * 5/6 - margin.left - margin.right;
	let height = topdiv.node().getBoundingClientRect().height - margin.top - margin.bottom;

	var svg = topdiv.append("svg");
	svg.attr("width",width);
	svg.attr("height",height);

	var simulation = d3.forceSimulation()
	    .force("link", d3.forceLink().id(function(d) { return d.id; }))
	    .force("charge", d3.forceManyBody())
	    .force("center", d3.forceCenter(width / 2, height / 2));

	var link = svg.append("g")
	    .attr("class", "links")
	    .selectAll("line")
	    .data(data.edges)
	    .enter().append("line")
	    .attr("stroke-width", 1)
	    .attr("stroke", "#999");

	var node = svg.append("g")
	    .attr("class", "nodes")
	    .selectAll("circle")
	    .data(data.nodes)
	    .enter().append("circle")
	    .attr("r", 5)
	    .attr("fill",function(d) { return d.directives.fillcolor; })
	/*	.call(d3.drag()
		.on("start", dragstarted)
		.on("drag", dragged)
		.on("end", dragended))*/;

	node.append("title")
	    .text(function(d) { return d.directives.label; });

	simulation
	    .nodes(data.nodes)
	    .on("tick", ticked);

	simulation.force("link")
	    .links(data.edges);

	function ticked() {
	    link
		.attr("x1", function(d) { return d.source.x; })
		.attr("y1", function(d) { return d.source.y; })
		.attr("x2", function(d) { return d.target.x; })
		.attr("y2", function(d) { return d.target.y; });

	    node
		.attr("cx", function(d) { return d.x; })
		.attr("cy", function(d) { return d.y; });
	}
    }

    clearData() {
	d3.select(this.id).selectAll("*").remove();
    }
}
