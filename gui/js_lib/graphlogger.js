"use strict"

// https://stackoverflow.com/questions/25595387/d3-js-how-to-convert-edges-from-lines-to-curved-paths-in-a-network-visualizatio
function draw_curve(Ax, Ay, Bx, By, M) {

    // Find midpoint J
    var Jx = Ax + (Bx - Ax) / 2
    var Jy = Ay + (By - Ay) / 2

    // We need a and b to find theta, and we need to know the sign of each to make sure that the orientation is correct.
    var a = Bx - Ax
    var asign = (a < 0 ? -1 : 1)
    var b = By - Ay
    var bsign = (b < 0 ? -1 : 1)
    var theta = Math.atan(b / a)

    // Find the point that's perpendicular to J on side
    var costheta = asign * Math.cos(theta)
    var sintheta = asign * Math.sin(theta)

    // Find c and d
    var c = M * sintheta
    var d = M * costheta

    // Use c and d to find Kx and Ky
    var Kx = Jx - c
    var Ky = Jy + d

    return "M" + Ax + "," + Ay +
           "Q" + Kx + "," + Ky +
           " " + Bx + "," + By
}

class GraphLogger {
    constructor(id,onClick) {
	this.id = "#"+id;
	this.onNodeClick = onClick;
    }

    setData(json) {
	var data = JSON.parse(json);
	var topdiv = d3.select(this.id);

	var graphlogger = this
	graphlogger.clearData();

	let margin = { top: 5, right: 5, bottom: 5, left: 5 };
	/* multiplying by a factor to account for flex display */
	let width = topdiv.node().getBoundingClientRect().width * 5/6 - margin.left - margin.right;
	let height = topdiv.node().getBoundingClientRect().height - margin.top - margin.bottom;

	var svg = topdiv.append("svg");
	svg.attr("width",width);
	svg.attr("height",height);

	let edgecolors = d3.map(data.edges,function (d) {return d.directives.color; }).keys();

	svg.append('defs').selectAll('marker').data(edgecolors).
	    enter().append('marker')
            .attr('id', function (c) {return ('arrowhead-'+c) ;})
	    .attr('viewBox','-0 -5 10 10')
	    .attr('refX',13)
	    .attr('refY',0)
	    .attr('orient','auto')
	    .attr('markerWidth',13)
	    .attr('markerHeight',13)
	    .attr('xoverflow','visible')
            .append('svg:path')
            .attr('d', 'M 0,-5 L 10 ,0 L 0,5')
            .attr('fill', function(c) { return c; })
            .style('stroke','none');

	var simulation = d3.forceSimulation()
	    .force("link", d3.forceLink().id(function(d) { return d.id; }).distance(100))
	    .force("charge", d3.forceManyBody())
	    .force("center", d3.forceCenter(width / 2, height / 2));

	var link = svg.append("g")
	    .attr("class", "links")
	    .selectAll("path")
	    .data(data.edges)
	    .enter().append("path")
	    .attr('id', function (d, i) {return 'linkpath' + i})
	    .attr("stroke-width", 1)
	    .attr("fill", "transparent")
	    .attr("stroke", function(d) { return d.directives.color; })
            .attr('marker-end',function(d) { return ('url(#arrowhead-'+d.directives.color+')') ; })

        var edgelabels = svg.selectAll(".edgelabel")
            .data(data.edges).enter()
            .append('text')
            .style("pointer-events", "none")
            .attr('class', 'edgelabel')
            .attr('id', function (d, i) {return 'edgelabel' + i})
            .attr('font-size', 10)
            .attr('fill', '#aaa');

        edgelabels.append('textPath')
            .attr('xlink:href', function (d, i) {return '#linkpath' + i})
            .style("text-anchor", "middle")
            .style("pointer-events", "none")
            .attr("startOffset", "50%")
            .text(function (d) {return d.directives.label});

	var node = svg.append("g")
	    .attr("class", "nodes")
	    .selectAll("g")
	    .data(data.nodes)
	    .enter().append("g")
	node.append("circle")
	    .attr("r", 5)
	    .attr("fill",function(d) { return d.directives.fillcolor; })
	    .on("click",function(d) { graphlogger.onNodeClick(JSON.stringify(d.directives.on_click)); })
	/*	.call(d3.drag()
		.on("start", dragstarted)
		.on("drag", dragged)
		.on("end", dragended))*/;

	node.append("text")
	    .attr("dy", -3)
	    .text(function(d) { return d.directives.label; });

	simulation
	    .nodes(data.nodes)
	    .on("tick", ticked);

	simulation.force("link")
	    .links(data.edges);

	function ticked() {
	    node.attr("transform", function (d) {return "translate(" + d.x + ", " + d.y + ")";});
	    link.attr('d', function (d) {
		return draw_curve(d.source.x,d.source.y,d.target.x,d.target.y,20);
            });

            edgelabels.attr('transform', function (d) {
		if (this.children.length !== 0 && d.target.x < d.source.x) {
                    var bbox = this.getBBox();

                    let rx = bbox.x + bbox.width / 2;
                    let ry = bbox.y + bbox.height / 2;
                    return 'rotate(180 ' + rx + ' ' + ry + ')';
		}
		else {
                    return 'rotate(0)';
		}
            });
	}
    }

    clearData() {
	d3.select(this.id).selectAll("*").remove();
    }
}
