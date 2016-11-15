var errorsDom = document.getElementById("errors");
var svg = d3.select("svg");

var zoom = d3.behavior.zoom().on("zoom", function() {
    d3.select("svg g").attr("transform", "translate(" + d3.event.translate + ")" +
	       "scale(" + d3.event.scale + ")");});

var dealWithFiles = function (files) {
    if (files.length === 1) {
	var file = files[0], ka = new FileReader();
	ka.onloadend = function(e){
	    d3.json(e.target.result, function (error,graph) {
		if (error) {
		    errorsDom.innerHTML = error;
		} else {
		    errorsDom.innerHTML = null;
		    var g = new dagreD3.graphlib.Graph().setGraph({});
		    for (i = 0; i < graph.dag.length; i++) {
			g.setNode(i, {label: graph.dag[i].content,
				      style: "fill: #eee"});
			for (j = 0; j < graph.dag[i].sons.length; j++)
			    g.setEdge(i,graph.dag[i].sons[j].dst,
				      {label: JSON.stringify(graph.dag[i].sons[j].nav),
				       style: "fill: none; stroke: #222; stroke-width: 1.5px"});
		    }
		    var render = new dagreD3.render();
		    render(svg,g);
		    svg.call(zoom);
		    svg.attr('height', g.graph().height + 20);
		}
	    })
	}
	ka.readAsDataURL(file);
    }
}

var domainFileDom = document.getElementById("domainFile");
domainFileDom.onchange = function() {dealWithFiles(domainFileDom.files)};
