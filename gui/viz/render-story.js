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
			      style: "fill: " + n.directives.color +"; stroke: #333",
			      shape: n.directives.shape });
	});
	story.edges.forEach(function (e) {
	    var linestyle = ""
	    switch (e.directives.linestyle) {
	    case "Dotted":
		linestyle = " stroke-dasharray: 2, 3;"
		break;
	    case "Dashed":
		linestyle = " stroke-dasharray: 5, 5;"
		break;
	    default:
	    }
	    g.setEdge(e.source, e.target, {
		style: "stroke: #333; fill: #fff; stroke-width: 1.5px;"+linestyle,
		arrowhead: e.directives.arrowhead,
		arrowtail: e.directives.arrowtail
	    });
	});

	// Create the renderer
	var render = new dagreD3.render();

	// Copy/Paste from https://dagrejs.github.io/project/dagre-d3/latest/demo/user-defined.html
	render.shapes().Invisible = render.shapes().rect

	render.shapes().House = function(parent, bbox, node) {
	    var w = bbox.width,
		h = bbox.height,
		points = [
		    { x:   0, y:        0 },
		    { x:   w, y:        0 },
		    { x:   w, y: -h * 4/5 },
		    { x: w/2, y:       -h },
		    { x:   0, y: -h * 4/5 }
		];
	    var shapeSvg = parent.insert("polygon", ":first-child")
		.attr("points", points.map(function(d) { return d.x + "," + d.y; }).join(" "))
		.attr("transform", "translate(" + (-w/2) + "," + h/2 + ")");

	    node.intersect = function(point) {
		return dagreD3.intersect.polygon(node, points, point);
	    };

	    return shapeSvg;
	};

	render.shapes().Invhouse = function(parent, bbox, node) {
	    var w = bbox.width,
		h = bbox.height,
		points = [
		    { x:   0, y: -h/5 },
		    { x: w/2, y:    0 },
		    { x:   w, y: -h/5 },
		    { x:   w, y:   -h },
		    { x:   0, y:   -h }
		];
	    var shapeSvg = parent.insert("polygon", ":first-child")
		.attr("points", points.map(function(d) { return d.x + "," + d.y; }).join(" "))
		.attr("transform", "translate(" + (-w/2) + "," + h/2 + ")");

	    node.intersect = function(point) {
		return dagreD3.intersect.polygon(node, points, point);
	    };

	    return shapeSvg;
	};

	render.arrows().Tee = function(parent, id, edge, type) {
	    var marker = parent.append("marker")
		.attr("id", id)
		.attr("viewBox", "0 0 2 8")
		.attr("refX", 1)
		.attr("refY", 4)
		.attr("markerUnits", "strokeWidth")
		.attr("markerWidth", 2)
		.attr("markerHeight", 8)
		.attr("orient", "auto");

	    var path = marker.append("path")
		.attr("d", "M 0 0 L 0 8")
		.style("stroke-width", 2)
		.style("fill", "#fff")
		.style("stroke", "#333");
	    dagreD3.util.applyStyle(path, edge[type + "Style"]);
	};

	// Set up an SVG group so that we can translate the final graph.
	var svg = topdiv.append("svg"),
	    inner = svg.append("g");

	// Set up zoom support
	var zoom = d3.zoom().on("zoom", function() {
	    inner.attr("transform", d3.event.transform);
	});
	svg.call(zoom);

	// Run the renderer. This is what draws the final graph.
	render(inner, g);

	// Center the graph
        let margin = { top: 10, right: 10, bottom: 10, left: 10 };
        let w = topdiv.node().getBoundingClientRect().width - margin.left - margin.right;
	let h = topdiv.node().getBoundingClientRect().height - margin.top - margin.bottom;
	svg.attr("width",w);
	svg.attr("height", h);
	var xCenterOffset = (w - g.graph().width) / 2;

	svg.call(zoom.transform, d3.zoomIdentity.translate(xCenterOffset, 5));
    }
    redraw() {

    }
    clearData() {
    	d3.select(this.id).selectAll("*").remove();
    }
}
