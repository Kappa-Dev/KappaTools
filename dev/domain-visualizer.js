var errorsDom = document.getElementById("errors");
var domainFileDom = document.getElementById("domainFile");
var rootSelectDom = document.getElementById("rootSelect");
var svg = d3.select("svg");

var domain = null

svg.call(d3.behavior.zoom().on("zoom", function() {
    svg.select("g").attr("transform", "translate(" + d3.event.translate + ")" +
			 "scale(" + d3.event.scale + ")");}));

var set_rootSelect = function () {
    while(rootSelectDom.firstChild){
	rootSelectDom.removeChild(rootSelectDom.firstChild);
    }
    domain.elementaries.forEach(function (a) {
	a.forEach(function (l) {
	    l.forEach(function (v) {
		var opt = document.createElement('option');
		opt.value = v[1];
		opt.innerHTML = domain.dag[v[1]].content;
		rootSelectDom.appendChild(opt);
	    })
	})
    });
}

var dealWithFiles = function (files) {
    if (files.length === 1) {
	var file = files[0], ka = new FileReader();
	ka.onloadend = function(e){
	    d3.json(e.target.result, function (error,graph) {
		if (error) {
		    errorsDom.innerHTML = error;
		} else {
		    errorsDom.innerHTML = null;
		    domain = graph;
		    set_rootSelect();
		}
	    })
	}
	ka.readAsDataURL(file);
    }
}

var addToGraph = function (cache,g,i) {
    if (cache.every(function (v) { return v !== i ; })) {
	g.setNode(i, {label: domain.dag[i].content,
		      style: "fill: #eee"});
	domain.dag[i].sons.forEach(function (s) {
	    addToGraph(cache,g,s.dst);
	    g.setEdge(i,s.dst,
		      {label: JSON.stringify(s.nav),
		       style: "fill: none; stroke: #222; stroke-width: 1.5px"});
	});
	cache.push(i);
    }
}

domainFileDom.onchange = function() {dealWithFiles(domainFileDom.files)};

rootSelectDom.onchange = function () {
    var g = new dagreD3.graphlib.Graph().setGraph({});
    addToGraph([],g,Number(rootSelectDom.value));
    var render = new dagreD3.render();
    render(svg,g);
    svg.attr('height', g.graph().height + 20);
};
