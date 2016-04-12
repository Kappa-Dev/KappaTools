"use strict"

function fluxMap(configuration) {
    var that = this;
    this.configuration = configuration;
    this.selfInfluence = false;
    this.flux = { "bioBeginTime" : 0.0, "bioEndTime" : 0.0,
		  "rules" : [],
		  "hits" : [],
		  "fluxs" : [] };

    this.setFlux = function(flux){
	if(!is_same(that.flux.rules,flux.rules)){
	    that.selectedRules = flux.rules.map(function (v,i) {return (i !== 0);});
	    that.flux = flux;
            that.render_controls();
	} else {
	    that.flux = flux;
	}
        that.render();
    };

    this.aClick = function(id) {
        that.selectedRules[id] = (that.selectedRules[id]) ? false : true;
        that.drawDIM();
    };
    this.toggleSelfInfluence = function() {
        that.selfInfluence = (that.selfInfluence) ? false : true;
        that.drawDIM();
    };

    this.filterRules = function(val,id) {
        return that.selectedRules[id];
    };

    this.pointValue = function(i,j,e) {
        var correction = document.getElementById(that.configuration.selectCorrectionId).value;
        if (that.selfInfluence || i !== j)
        {   if (correction === "hits")
            {return (that.flux.hits[i] === 0.) ? 0 : Math.abs(e) / that.flux.hits[i];}
            else if (correction === "time")
            {return Math.abs(e) / (that.flux.endTime - that.flux.beginTime);}
            else {return Math.abs(e);}
        }
        else {return 0;}
    };

    this.fade = function fade(svg,opacity)
    { return function(g, i) { svg
                              .selectAll(".chord path")
                              .filter(function(d) { return d.source.index != i && d.target.index != i; })
                              .transition().style("opacity", opacity); }; };

    this.drawDIM = function(){
        var matrix = that.flux.fluxs
            .map(function(a,i){return a.map(function (e,j)
                                            {return that.pointValue (i,j,e)})
                               .filter(that.filterRules);}).filter(that.filterRules),
            rules = that.flux.rules.filter(that.filterRules),
            color = that.flux.fluxs.map(function(a)
                                   {return a.map(function (x) {return (x < 0) ? "#FF0000" : "#00FF00";})
                                    .filter(that.filterRules);}).filter(that.filterRules);
        var chord = d3.layout.chord().padding(.01).matrix(matrix);
        var width = configuration.width?configuration.width:960,
            height = configuration.height?configuration.height:700,
            innerRadius = Math.min(width, height) * .37;
        var arc = d3.svg.arc().innerRadius(innerRadius)
            .outerRadius(innerRadius + 8);
        var svg_element = (that.configuration.svgId)?d3.select("#"+that.configuration.svgId):d3.select("body").select("svg");
        var svg = svg_element.attr("width", width)
                  .attr("height", height).select("g").attr("transform", "translate(" + width / 2 + "," + height / 2 + ")");
        svg.selectAll("*").remove();
        svg.append("g").attr("class", "chord").selectAll("path")
            .data(chord.chords).enter().append("path")
            .filter(function (v) {return (v.source.value != 0); }).attr("d", d3.svg.chord().radius(innerRadius))
            .style("fill", function(d) { return color[d.source.index][d.target.index]; })
            .style("opacity", 1);
        svg.append("g").attr("id", "values").selectAll(".sources")
            .data(chord.chords).enter().append("text").attr("class","sources")
            .each(function(d) { d.angle = ( d.source.startAngle + d.source.endAngle) / 2; })
                .attr("dy", ".1em")
            .attr("transform", function(d) {
                return "rotate(" + (d.angle * 180 / Math.PI - 90) + ")"
                    + "translate(" + (innerRadius - 10) + ")"
                + (d.angle > Math.PI ? "rotate(180)" : ""); })
            .style("text-anchor", function(d) { return d.angle > Math.PI ? null : "end" ; })
            .text(function (d) { return d.source.value.toExponential(2);});
        svg.select("#values").selectAll(".targets")
            .data(chord.chords).enter().append("text")
            .filter(function (v) {return (v.target.value != 0); }).attr("class","targets")
            .each(function(d) { d.angle = ( d.target.startAngle + d.target.endAngle) / 2; })
                .attr("dy", ".1em")
            .attr("transform", function(d) {
                return "rotate(" + (d.angle * 180 / Math.PI - 90) + ")"
                    + "translate(" + (innerRadius - 10) + ")"
                    + (d.angle > Math.PI ? "rotate(180)" : ""); })
            .style("text-anchor", function(d) { return d.angle > Math.PI ? null : "end" ; })
            .text(function (d) { return d.target.value.toExponential(2);});
        var legends = svg.append("g").selectAll("g").data(chord.groups).enter()
            .append("g");
        legends.append("text")
            .each(function(d) { d.angle = (d.startAngle + d.endAngle) / 2; })
                .attr("dy", ".1em")
            .attr("transform", function(d) {
                return "rotate(" + (d.angle * 180 / Math.PI - 90) + ")"
                    + "translate(" + (innerRadius + 10) + ")"
                    + (d.angle > Math.PI ? "rotate(180)" : ""); })
            .style("text-anchor", function(d) { return d.angle > Math.PI ? "end" : null; })
            .text(function(d) { return rules[d.index]; });
        legends.append("path").style("fill", "#222222").attr("d", arc)
            .on("mouseover", that.fade(svg,.1)).on("mouseout", that.fade(svg,1));
    };

    this.render_other = function(){
        that.drawDIM();
    };
    this.render_controls = function(){
        var rulesCheckboxes = document.getElementById(that.configuration.rulesCheckboxesId);
        while (rulesCheckboxes.hasChildNodes()){
            rulesCheckboxes.removeChild(rulesCheckboxes.lastChild);
        };
        that.selectedRules.forEach(function (val,id,a) {
            var group = document.createElement("div")
            group.setAttribute("class","input-group");
            var boxbox = document.createElement("label"),
                box = document.createElement("input");
            boxbox.setAttribute("class","checkbox-inline")
            box.setAttribute("type", "checkbox");
            if (val) {box.setAttribute("checked","")};
            box.addEventListener("change",function () { that.aClick(id);});
            boxbox.appendChild(box);
            boxbox.appendChild(document.createTextNode(that.flux.rules[id]));
            group.appendChild(boxbox);
            rulesCheckboxes.appendChild(group);
        });
    };

    this.render_labels = function(){
        d3.select("#"+that.configuration.beginTimeId).text(that.flux.bioBeginTime);
        d3.select("#"+that.configuration.endTimeId).text(that.flux.bioEndTime);
        d3.select("#"+that.configuration.nbEventsId).text(that.flux.hits.reduce(function (acc,v) {return acc + v;},0));

    };
    this.add_handlers = function(){
        d3.select("#"+that.configuration.selectCorrectionId).on("change",function() { that.drawDIM()});
        d3.select("#"+that.configuration.checkboxSelfInfluenceId).on("click",function() { that.toggleSelfInfluence()});
    };

    this.exportJSON = function(filename){
	try { var json = JSON.stringify(that.flux);
	      saveFile(json,"application/json",filename);
	} catch (e) {
	    alert(e);
	}
    }

    this.render = function() {
        that.render_labels();
        that.render_other();
    };
    that.add_handlers();

}
