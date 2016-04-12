function observable_plot(configuration){
    var that = this;
    this.configuration = configuration;

    this.state = { "plot" :  {"legend":[],"observables":[] }
	 	 , "selected" : []
	         , "observableMap" : new Object() };

    this.setPlot = function(plot){
	if(!is_same(that.state.plot.legend,plot.legend)){
	    var callback = function(acc, value, index) {
		acc[value] = index;
		return acc;
	    };
	    var observableMap = plot["legend"].reduce(callback,new Object());
	    that.state = { "plot" :  plot
		           , "selected" : plot["legend"]
			   , "observableMap" : observableMap };
	    this.renderControls();
	}
	that.state.plot = plot;
	that.renderPlot();
	that.renderLabel();
    }
    this.getPlot = function() {
	return that.state.plot;
    }
    this.getSelected = function(selected){
	return that.state.selected.indexOf(selected) > -1;
    }
    this.setSelected = function(selected,value){
	that.state.selected = that.state.selected.filter(function(s){ return selected != s;});
	if(value){
	    that.state.selected.push(selected);
	}
    }
    this.getObservableMap = function() {
	return that.state.observableMap;
    }
    this.observableIndex = function(name){
	return that.getObservableMap()[name];
    }

    this.showLegend = true;
    this.setShowLegend = function(showLegend){
	that.showLegend = showLegend;
    }
    this.getShowLegend = function(){
	return that.showLegend;
    }

    this.xAxisLog = false;
    this.setXAxisLog = function(xAxisLog){
	that.xAxisLog = xAxisLog;
    }
    this.getXAxisLog = function(){
	return that.xAxisLog;
    }

    this.yAxisLog = false;
    this.setYAxisLog = function(yAxisLog){
	that.yAxisLog = yAxisLog;
    }
    this.getYAxisLog = function(){
	return that.yAxisLog;
    }

    this.dimensions = { width : 960 , height : 500 };
    this.setDimensions = function(dimensions) {
	that.dimensions = dimensions;
    }
    this.getDimensions = function(){
	return that.dimensions;
    }
    this.plotName = "kappa-plot";
    this.getPlotName = function(suffix){
	var filename = that.plotName;
	filename = (filename.indexOf('.') === -1)?filename+suffix:filename;
	return filename;
    }
    this.setPlotName = function(plotName){ that.plotName = plotName; }

    this.renderPlot = function(){
	var margin = {top: 20, right: 80, bottom: 30, left: 80},
	    dimensions = that.getDimensions() ,
	    width = dimensions.width - margin.left - margin.right,
	    height = dimensions.height - margin.top - margin.bottom;

	var x = (that.getXAxisLog()?d3.scale.log().clamp(true):d3.scale.linear()).range([0, width]);
	var xAxis = d3.svg.axis().scale(x).orient("bottom");

	var y = (that.getYAxisLog()?d3.scale.log().clamp(true):d3.scale.linear()).range([height, 0]);
	var yAxis = d3.svg.axis().scale(y).orient("left").tickFormat(d3.format(".3n"));


	var line = d3.svg.line()
            .interpolate("basis")
	    .defined(function(d) { return !that.getYAxisLog() || d.value > 0; })
            .x(function(d) { return x(d.time); })
            .y(function(d) { return y(d.value); });
	// remove previously drawn
	d3.select("#"+that.configuration.plotDivId).html("");
	var svg = d3.select("#"+that.configuration.plotDivId).append("svg")
            .attr("width", width + margin.left + margin.right)
            .attr("height", height + margin.top + margin.bottom)
	    .append("g")
            .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

	var svgDefs = createSVGDefs(svg);

	var plot = that.getPlot();
	var color = d3.scale.category10();
	color.domain(plot["legend"].filter(that.getSelected));

	var observables = color.domain().map(function(name) {
	    return {
		name: name,
		values: plot["observables"].map(function(d) {
		    return { time: d["time"], value: d["values"][that.observableIndex(name)]};
		})
	    };
	});

	x.domain(d3.extent(plot["observables"], function(d) { return d.time; }));
	y.domain([
	    d3.min(plot["observables"], function(c) { return d3.min(c.values.filter(function(d){return !that.getYAxisLog() || d > 0; })); }),
	    d3.max(plot["observables"], function(c) { return d3.max(c.values); })
	]);

	svg.append("g")
	    .attr("class", "x axis")
	    .attr("transform", "translate(0," + height + ")")
	    .call(xAxis);

	svg.append("g")
	    .attr("class", "y axis")
	    .call(yAxis)
	    .append("text")
	    .attr("transform", "rotate(-90)");
	var observable = svg.selectAll(".observable")
            .data(observables)
            .enter().append("g")
            .attr("class", "observable");

	observable.append("path")
	    .attr("class", "line")
            .attr("d", function(d) { return line(d.values); })
            .style("stroke", function(d) { return color(d.name); });

	// legend
	if(that.getShowLegend()){
	// https://github.com/zeroviscosity/d3-js-step-by-step/blob/master/step-3-adding-a-legend.html
	var legendRectSize = 12;
	var legendSpacing = 4;
	var legend = svg.selectAll('.legend')
            .data(color.domain())
            .enter()
            .append('g')
            .attr('class', 'legend')
            .attr('transform', function(d, i) {
		var height = legendRectSize + legendSpacing;
		var offset = 0;
		var horz = legendRectSize;
		var vert = i * height - offset;
		return 'translate(' + horz + ',' + vert + ')';
	    });

	legend.append('rect')
            .attr('width', legendRectSize)
            .attr('height', legendRectSize)
            .style('fill', color)
            .style('stroke', color);

	legend.append('text')
            .attr('x', legendRectSize + legendSpacing)
            .attr('y', legendRectSize - legendSpacing)
            .text(function(d) { return d; });
	}

    };
    this.renderLabel = function(){
	if(configuration.plotLabelDivId){
	    var length = that.state.plot.observables.length;
	    if (that.state.plot && length > 1){
		var first = this.state.plot.observables[0];
		var last = this.state.plot.observables[length-1];
		var label = "Plot between t = "+last.time+"s and t = "+first.time+"s";
		d3.select("#"+configuration.plotLabelDivId).html(label);
	    }
	}
    }

    this.save = function(data,mime,filename){
	var blob = new Blob([data], {type: mime });
	var url = window.URL.createObjectURL(blob);
	var a = document.createElement("a");
	document.body.appendChild(a);
	a.style = "display: none";
	a.href = url;
	a.download = filename;
	a.click();
	document.body.removeChild(a);
    }
    this.handlePlotTSV = function(){
	try { var plot = that.getPlot();
	      var header = "'time'\t"+plot["legend"].join("\t");
	      var body = plot["observables"].map(function(d) { var row = [d["time"]];
        	                                               row = row.concat(d["values"]);
							       return row.join("\t") }).join("\n");
	      var tsv = header+"\n"+body;
	      saveFile(tsv,"text/tab-separated-values",that.getPlotName(".tsv"));
	} catch (e) {
	    alert(e);
	}
    }
    this.handlePlotSVG = function(){
	plotSVG(that.configuration.plotDivId
	       ,"kappa plot"
	       ,that.getPlotName(".svg")
	       ,that.configuration.plotStyleId);
    }
    this.handlePlotPNG = function(){
	plotPNG(that.configuration.plotDivId
	       ,"kappa plot"
	       ,that.getPlotName(".png")
	       ,that.configuration.plotStyleId);
    }

    this.addHandlers = function(){
	if(configuration.plotSVGButtonId){
	    d3.select("#"+that.configuration.plotSVGButtonId).on("click",function() { that.handlePlotSVG()});
	}
	if(configuration.plotTSVButtonId){
	    d3.select("#"+that.configuration.plotTSVButtonId).on("click",function() { that.handlePlotTSV()});
	}
	if(configuration.plotPNGButtonId){
	    d3.select("#"+that.configuration.plotPNGButtonId).on("click",function() { that.handlePlotPNG()});
	}
	function checkboxHandler(id,setter){
	    if(id){
		setter(document.getElementById(id).checked);
		var handler = function(){ setter(document.getElementById(id).checked);
					  that.render();};
		d3.select("#"+id).on("change",handler);
	    }
	}
	checkboxHandler(configuration.plotShowLegendCheckboxId,that.setShowLegend);
	checkboxHandler(configuration.plotXAxisLogCheckboxId,that.setXAxisLog);
	checkboxHandler(configuration.plotYAxisLogCheckboxId,that.setYAxisLog);
	if(configuration.plotLogCheckboxId){
	    d3.select("#"+that.configuration.plotLogCheckboxId).on("change",function() { that.handlePlotTSV()});
	}
    }
    this.renderControls = function() {
	if(that.configuration.plotControlsDivId){
	    d3.select("#"+that.configuration.plotControlsDivId).html("");
	    var legend = that.getPlot()["legend"];
	    legend.forEach(function(observable){
		var controlsDiv = document.getElementById(that.configuration.plotControlsDivId);
		var group = document.createElement("div")
		group.setAttribute("class","input-group");
		var boxbox = document.createElement("label"),
                    box = document.createElement("input");
		boxbox.setAttribute("class","checkbox-inline")
		box.setAttribute("type", "checkbox");
		if (that.getSelected(observable)) {box.setAttribute("checked","")};
		box.addEventListener("change",function () { that.setSelected(observable,box.checked);
							    that.renderPlot();
							    that.renderLabel();
							  });
		boxbox.appendChild(box);
		boxbox.appendChild(document.createTextNode(observable));
		group.appendChild(boxbox);
		controlsDiv.appendChild(group);
	    });
	}
    }
    this.render = function(){
	that.renderControls();
	that.renderPlot();
	that.renderLabel();

    };
    this.addHandlers();
}
