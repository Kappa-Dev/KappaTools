"use strict"

var errorsDom = document.getElementById("errors");
var domainFileDom = document.getElementById("domainFile");
var rootSelectDom = document.getElementById("rootSelect");
var nodeIndexDom = document.getElementById("node-index");
var edgeIndexDom = document.getElementById("edge-index");
var svg = d3.select("svg");

var domain = null

var stringOfInj = function (inj) {
    return inj.reduce(
	function (acc,v) {
	    if (v[0] === v[1])
		return v[0] + " " + acc;
	    else
		return v[0] + "->" + v[1] + " " + acc;
	},"")
}

var stringOfSite = function (types,ag,s) {
    return domain.signatures[types[ag]].decl[s].name;
}

var stringOfState = function (types,ag,si,st) {
    return domain.signatures[types[ag]].decl[si].decl.first[st].name;
}

var stringOfNav = function (nav,point) {
    var types = [];
    point.sorts.forEach(
	function (v,i) {
	    if (v !== null) types[i] = v;
	}
    );
    return nav.reduce(
	function (acc,v) {
	    var src = v[0][0] + "." + stringOfSite(types,v[0][0],v[0][1]);
	    var dst = "";
	    if (v[1] !== null) {
		if (typeof v[1] === 'number') {
		    dst = "~" + stringOfState(types,v[0][0],v[0][1],v[1]);
		} else {
		    var ag;
		    if (typeof v[1][0] === 'number') {
			ag = v[1][0];
		    } else {
			types[v[1][0].id] = v[1][0].type;
			ag = v[1][0].id;
		    }
		    dst = "-" + ag + "." + stringOfSite(types,ag,v[1][1]);
		}
	    }
	    return src + dst + "," + acc;
	},"");
}

var stringOfPattern = function (p) {
    var free_id=1, cache=[];
    return p.sorts.reduce(
	function (acc,v,i) {
	    if (v !== null) {
		var sig = domain.signatures[v];
		var intf =
		    p.nodes[i].reduce(
			function (acc,v,s) {
			    if (!v[0] && (v[1] === null))
				return acc;
			    else {
				var state = "", link = "?", index, sig_s = sig.decl[s];
				if (v[1] !== null) state = "~" + sig_s.decl.first[v[1]].name;
				if (v[0] === true) link = "";
				if (typeof v[0] === 'object') {
				    index = cache.findIndex(
					function (x) { return (x && x[0] === v[0].node && x[1] === v[0].site); });
				    if (index === -1) {
					index = free_id;
					cache[index] = [i,s];
					free_id++;
				    }
				    link = "!" + index;
				}
				return acc + ", " + sig_s.name + state + link;
			    }
			},"");
		return (sig.name + "/*" + i + "*/(" + intf + "), "+ acc);
	    } else return acc;
	},"")
}

svg.call(d3.behavior.zoom().on("zoom", function() {
    svg.select("g").attr("transform", "translate(" + d3.event.translate + ")" +
			 "scale(" + d3.event.scale + ")");}));

var set_rootSelect = function () {
    while(rootSelectDom.firstChild){
	rootSelectDom.removeChild(rootSelectDom.firstChild);
    }
    var opt = document.createElement('option');
    opt.value = 0;
    opt.innerHTML = "All";
    rootSelectDom.appendChild(opt);
    domain.elementaries.forEach(function (a) {
	a.forEach(function (l) {
	    l.forEach(function (v) {
		var opt = document.createElement('option');
		opt.value = v[1];
		opt.innerHTML = stringOfPattern(domain.dag[v[1]].content);
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
	var s_row = nodeIndexDom.insertRow(-1);
	s_row.insertCell(0).innerHTML = i;
	s_row.insertCell(1).innerHTML = stringOfPattern(domain.dag[i].content);

	g.setNode(i, {label: i,
		      style: "fill: #eee"});
	domain.dag[i].sons.forEach(function (s,j) {
	    addToGraph(cache,g,s.dst);
	    var s_row = edgeIndexDom.insertRow(-1);
	    s_row.insertCell(0).innerHTML = i + " &rarr; " + s.dst;
	    s_row.insertCell(1).innerHTML =
		stringOfNav(s.nav,domain.dag[i].content) +
		" ("+ stringOfInj(s.inj)  +")";
	    g.setEdge(i,s.dst,
		      {label: "",
		       style: "fill: none; stroke: #222; stroke-width: 1.5px"},
		      "n"+i+"e"+j);
	});
	cache.push(i);
    }
}

domainFileDom.onchange = function() {dealWithFiles(domainFileDom.files)};

rootSelectDom.onchange = function () {
    while (nodeIndexDom.rows.length>0) {nodeIndexDom.deleteRow(0);}
    while (edgeIndexDom.rows.length>0) {edgeIndexDom.deleteRow(0);}
    var g = new dagreD3.graphlib.Graph({ multigraph: true }).setGraph({});
    var cache = [], selected = Number(rootSelectDom.value);
    if (selected === 0)
	domain.elementaries.forEach(function (a) {
	    a.forEach(function (l) {
		l.forEach(function (v) {
		    addToGraph(cache,g,v[1]);
		})
	    })
	});
    else addToGraph(cache,g,selected);
    var render = new dagreD3.render();
    render(svg,g);
    svg.attr('height', g.graph().height + 20);
};
