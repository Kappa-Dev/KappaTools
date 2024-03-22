"use strict"

function fluxMap(configuration) {
    var that = this;
    this.configuration = configuration;
    this.selfInfluence = false;
    this.flux = { "din_start" : 0.0, "din_end" : 0.0,
		  "din_kind" : "ABSOLUTE",
		  "din_rules" : [],
		  "din_hits" : [],
		  "din_fluxs" : [] };

    this.setFlux = function(flux){
	if(!is_same(that.flux.din_rules,flux.din_rules)){
	    that.selectedRules = flux.din_rules.map(function (v,i) {return (i !== 0);});
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

    this.toggleSelectedRules = function() {
	that.selectedRules.forEach(function (b,i,a) {a[i] = (b) ? false : true; });
	that.render_controls();
	that.drawDIM();
    };

    this.filterRules = function(val,id) {
        return that.selectedRules[id];
    };

    this.pointValue = function(i,j,e) {
        var correction = document.getElementById(that.configuration.selectCorrectionId).value;
        if (that.selfInfluence || i !== j)
        {   if (correction === "hits")
            {return (that.flux.din_hits[i] === 0.) ? 0 : Math.abs(e) / that.flux.din_hits[i];}
            else if (correction === "time")
            {return Math.abs(e) / (that.flux.din_end - that.flux.din_start);}
            else {return Math.abs(e);}
        }
        else {return 0;}
    };

    this.fade = function fade(svg,opacity)
    { return function(g, i) { svg
                              .selectAll(".chord path")
                              .filter(function(d) { return d.source.index != i && d.target.index != i; })
                              .transition().style("opacity", opacity); }; };

    var svg = (that.configuration.svgId)?d3.select("#"+that.configuration.svgId):d3.select("body").select("#flux_container").append("svg");
    this.drawDIM = function(){
        var matrix = that.flux.din_fluxs
            .map(function(a,i){return a.map(function (e,j)
                                            {return that.pointValue (i,j,e)})
                               .filter(that.filterRules);}).filter(that.filterRules),
            rules = that.flux.din_rules.filter(that.filterRules),
            color = that.flux.din_fluxs.map(function(a)
                                   {return a.map(function (x) {return (x < 0) ? "#FF0000" : "#00FF00";})
                                    .filter(that.filterRules);}).filter(that.filterRules);
	var width = configuration.width?configuration.width:960,
            height = configuration.height?configuration.height:700,
            innerRadius = Math.min(width, height) * .37,
	    outerRadius = innerRadius + 8;
        svg.attr("width", width)
           .attr("height", height)
	   .select("g")
	   .attr("transform", "translate(" + width / 2 + "," + height / 2 + ")");
        svg.selectAll("*").remove();


	var formatValue = d3.formatPrefix(",.0", 1e3);

	var chord = d3.chord()
	    .padAngle(0.05)
	    .sortSubgroups(d3.descending);

	var arc = d3.arc()
	    .innerRadius(innerRadius)
	    .outerRadius(outerRadius);

	var ribbon = d3.ribbon()
	    .radius(innerRadius);

	var color = d3.scaleOrdinal()
	    .domain(d3.range(matrix.length))
	    .range(["#FF0000" , "#00FF00"]);

	var g = svg.append("g")
	    .attr("transform", "translate(" + width / 2 + "," + height / 2 + ")")
	    .datum(chord(matrix));
	var group = g.append("g")
	    .attr("class", "groups")
	    .selectAll("g")
	    .data(function(chords) { return chords.groups; })
	    .enter().append("g");

	group.append("path")
	    .style("fill", function(d) { return d3.rgb("#000000"); })
	    .style("stroke", function(){ return d3.rgb("#000000"); } )
	    .attr("d", arc);

        var legends = group.append("g");
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

	var ribbon = g.append("g")
	    .attr("class", "ribbons")
	    .selectAll("path")
	    .data(function(chords) { return chords; })
	    .enter().append("path")
	    .attr("d", ribbon)
	    .style("fill", function(d) { return color(d.target.index); })
	    .style("stroke", function(d) { return d3.rgb(color(d.target.index)).darker(); });


	ribbon.data(function(chords){ return chords; })
	    .enter()
  	    .append("text")
	    .each(function(d) { d.labelAngle = ( d.source.startAngle + d.source.endAngle) / 2; })
                .attr("dy", ".1em")
            .attr("transform", function(d) {
                return "rotate(" + (d.angle * 180 / Math.PI - 90) + ")"
                    + "translate(" + (innerRadius - 10) + ")"
                    + (d.labelAngle > Math.PI ? "rotate(180)" : ""); })
            .style("text-anchor", function(d) { return d.labelAngle > Math.PI ? null : "end" ; })
            .text(function (d) { return d.source.value.toExponential(2);});

    };

    this.render_other = function(){
        that.drawDIM();
    };
    this.render_controls = function(){
        var rulesCheckboxes = document.getElementById(that.configuration.rulesCheckboxesId);

        while (rulesCheckboxes.hasChildNodes()){
            rulesCheckboxes.removeChild(rulesCheckboxes.lastChild);
        };
        var correction_select =
	    document.getElementById(that.configuration.selectCorrectionId);
	correction_select.value = "none";
	if (that.flux.din_kind !== "ABSOLUTE")
	    correction_select.style.visibility="hidden";
	else
	    correction_select.style.visibility="visible";
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
            boxbox.appendChild(document.createTextNode(
		that.flux.din_rules[id].concat(" (",that.flux.din_hits[id]," occurences)")));
            group.appendChild(boxbox);
            rulesCheckboxes.appendChild(group);
        });
    };

    this.render_labels = function(){
        d3.select("#"+that.configuration.beginTimeId).text(that.flux.din_start);
        d3.select("#"+that.configuration.endTimeId).text(that.flux.din_end);
        d3.select("#"+that.configuration.nbEventsId).text(that.flux.din_hits.reduce(function (acc,v) {return acc + v;},0));

    };
    this.add_handlers = function(){
        d3.select("#"+that.configuration.selectCorrectionId).on("change",function() { that.drawDIM()});
        d3.select("#"+that.configuration.checkboxSelfInfluenceId).on("click",function() { that.toggleSelfInfluence()});
	d3.select("#"+that.configuration.toggleRulesId).on("click",function() { that.toggleSelectedRules()});
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
