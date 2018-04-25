"use strict"

function ObservablePlot (mainDivId) {
    var that = this;
    /* Mode to render observable.  These codes determine
     * how an observable in a plot is to be rendered.
     */
    this.modes = {
	LINE   : 0,  // use dots
	DOT    : 1,  // use a continuious line
	HIDDEN : 2,  // hide observable
	XAXIS  : 3,  // observable is the x axis
	MARKS  : 4,  // use marks
	cycle : function(mode){
            return (mode+1) % 3;
	},
	DEFAULT : 0
    };

    // enum for tick marks
    this.ticks = {
	CIRCLE : 0,
	PLUS   : 1,
	CROSS  : 2,
	DOT    : 3,
	cycle : function(mode){
            return (mode+1) % 3;
	},
	index : function(index){
            return (index+1) % 3;
	},
	xlink : function(index){
        return ["#plot-circle"
	       ,"#plot-plus"
	       ,"#plot-cross"
	       ,"#plot-dot" ][index];
	}
    };

    this.timeLabel = "Time";

    this.plotLabelDivId = "plot-label-div"          // div to place plot interval
    this.plotDivAxisSelectId = "plot-axis-select"
    this.plotDivId = "plot-display"

    /* This should be a list of objects of the form
       index  - there is an assumption of could be duplicated so
                an index is used.
       label  - label of observable
       values - all values for a given observable
       mode   - mode for observable using the mode enumermation

       Note : An entry for time is placed with the label
       null.  This allows an arbitary axis to serve at the
       x-axis.
     */
    this.state =
	[{ label : this.timeLabel, values : [], mode : this.modes.XAXIS }];

    /* Return the list of labels for values in the plot.
       The time axis is filtered out as it is null.
     */
    this.getLabels = function(){
        return this.state.filter(function(obs){ return obs.label; })
    };

    /* Given a label return the corresponding label.
     */
    this.getObservable = function(index){
        return this.state[index];
    }
    /* Get all time series.
     */
    this.getTimeSeries = function(){
        return this.state.filter(function(obs)
				 { return obs.mode != that.modes.XAXIS });
    }
    this.getVisible = function(){
        return this.state.filter(function(obs)
				 { return obs.mode != that.modes.XAXIS
				   && obs.mode != that.modes.HIDDEN;
				 });
    }

    this.getStatesByMode = function(mode){
        return this.state.filter(function(obs){ return obs.mode == mode; })
    }

    this.rawData = { legend:[], values:[] }

    this.setRawData = function(plot_text){
	var plot = JSON.parse(plot_text);
	plot.legend.shift();
	plot.series = plot.series.map(function (obs) {
	    var tmp = obs.shift();
	    return { time: tmp, values: obs };
	});
        that.rawData = plot;
    }

    /* Update the plot data of the graph.  This is called
       when new data is to be displayed in a graph.  Care
       is taken not to reset the state of the plot when
       data is updated.
     */
    this.setPlot = function(plot){
        that.setRawData(plot);
        var legend = that.rawData.legend;

        /* An update new copy over state preserving settings
         * where possible.
         */
        var new_state = [];
        var time_values = []; // store the times values

        /* Initialize new state element with the corresponding
         * time series.
         */
        legend.forEach(function(legend,i){
            var old_observable = that.getObservable(i);
            var mode = that.modes.DEFAULT;
            if(old_observable && old_observable.label == legend){
                mode = old_observable.mode;
            };
            new_state.push({ label : legend ,
                             values : [] ,
                             mode : mode
                           });
        });
        // Switch over for the population



        /* we need to figure out what mode the time axis is
           1. the time values are the last element of the state
              array.
           2. lets default the time axis to be the x-axis
           3. double check it is the time values are what is
              selected by checking the label.
              4. if it is the x-axis then copy over the
           5. check that there is an x-axis
              6. if there is not then use the time axis
         */
        // not sure what is happening here
        var time_observable = that.state.slice(-1)[0]; // 1.
        var time_mode = that.modes.XAXIS;             // 2.
        if(time_observable && time_observable.label == that.timeLabel) // 3.
        { time_mode = time_observable.mode;     // 4.
        };

        if(new_state.every(function(state){ return state.mode != that.modes.XAXIS; })){ // 5.
            time_mode = that.modes.XAXIS; // 6.
        }
        that.state = new_state;

	var first_time = null;
	var last_time = null;

        //that.start_time = null;
        //that.end_time = null;
        // Populate timeSeries from data.
        that.rawData.series.forEach(function(observable){
	    first_time = first_time || observable.time;
	    last_time = observable.time;
            //that.start_time = that.start_time || observable.time;
            //that.end_time = observable.time;
            time_values.push(observable.time);
            var values = observable.values;
            that.state.forEach(function(state_observable,i){
                var current = values[i];
                state_observable.values.push(current);
            });
        });
	if (first_time !== null && last_time !== null){
            that.start_time = Math.min(first_time,last_time);
            that.end_time = Math.max(first_time,last_time);
	} else {
	    that.start_time = null;
	    that.end_time = null;
	}
        // Add time axis
        that.state.push({ label : this.timeLabel ,
                          values : time_values ,
                          mode : time_mode
                        });


        // setup colors
        var color = d3.scaleOrdinal(d3.schemeCategory10);
        color.domain(that.state.map(function(c,i){ return i; }));
        that.state.forEach(function(s,i){ s.color = color(i);
                                          s.tick = that.ticks.index(i);
                                        });
        that.redraw();

    };
    this.setData = wrap(this.setPlot);
    this.formatTime = d3.format(".02f");
    this.getXAxis = function(){
        return this.state.find(function(state){ state.mode = that.modes.XAXIS });
    }

    this.renderPlot = function(){
        // set margin
        var margin = {top: 20, right: 80, bottom: 30, left: 80},
            dimensions = d3.select("#plot-display").node().getBoundingClientRect(),
            width = dimensions.width - margin.left - margin.right,
            height = dimensions.height - margin.top - margin.bottom;

        // setup x-axis
        var x = (that.getXAxisLog()?d3.scaleLog().clamp(true):d3.scaleLinear()).range([0, width]);
        var xState = that.getStatesByMode(that.modes.XAXIS)[0];
	/* Get bounds , make sure to filter out nan's caused by log
	   (in particular that of time equal zero).
	 */
	var x_bounds = [d3.min(xState
			       .values
			       .filter(function(d)
				       { return !that.getXAxisLog() || d > 0; })),
			d3.max(xState
			       .values
			       .filter(function(d)
				       { return !that.getXAxisLog() || d > 0; }))];
        x.domain(x_bounds);
        var xAxis = d3.axisBottom(x);

        // setup y-axis
        var y = (that.getYAxisLog()?
		 d3.scaleLog().clamp(true):
		 d3.scaleLinear()).range([height, 0]);
        var yAxis = d3.axisLeft(y)
                      .tickFormat(d3.format(".3n"));

        var visible = that.getVisible();
        var y_bounds = [d3.min(visible,
                         function(c)
                         { return d3.min(c.values.filter(function(d)
							 {return !that.getYAxisLog() || d > 0; }));
			 }),
                        d3.max(visible,function(c) { return d3.max(c.values); })
                       ];
        y.domain(y_bounds);

        // Clear div first
        d3.select("#"+that.plotDivId).html("");
        // Add svg element
        var svg = d3.select("#"+that.plotDivId).append("svg")
            .attr("width", width + margin.left + margin.right)
            .attr("height", height + margin.top + margin.bottom);
        var svgDefs = createSVGDefs(svg);
        // defs

        /* Here the defs for the tick marks it was taken from
           an earlier implementation of plots.
         */
        // defs - plus
        svg.append("defs")
           .append("g")
           .attr("id","plot-plus")
           .append("path")
           .attr("d","M-3.5,0 h7 M0,-3.5 v7")
           .style("stroke", "currentColor");
        // defs - cross
        svg.append('defs')
           .append("g")
           .attr("id","plot-cross")
           .append("path")
           .attr("d","M-3.5,-3.5 L3.5,3.5 M3.5,-3.5 L-3.5,3.5")
           .style("stroke", "currentColor");

        // defs - circles
        svg.append("defs")
           .append("g")
           .attr("id","plot-circle")
           .append("circle")
           .attr("r", 1.5)
           .attr("fill","none")
           .style("stroke", "currentColor");

        // defs - dots
        svg.append("defs")
           .append("g")
           .attr("id","plot-dot")
           .append("circle")
           .attr("r", 0.5)
           .attr("fill","none")
           .style("stroke", "currentColor");

        svg = svg.append("g")
                 .attr("transform"
                       ,"translate(" + margin.left + "," + margin.top + ")");

        // draw axis
        svg.append("g")
            .attr( "class"
                 , "x plot-axis")
            .attr("transform"
                 ,"translate(0," + height + ")")
            .call(xAxis);

        svg.append("g")
            .attr("class","y plot-axis")
            .call(yAxis)
            .append("text")
            .attr("transform"
                 ,"rotate(-90)");

        // render data
        var line = d3.line()
            .curve(d3.curveLinear)
            .defined(function(d,i) { return !that.getXAxisLog() || xState.values[i] > 0; })
            .defined(function(d,i) { return !isNaN(d) && (!that.getYAxisLog() || d > 0); })
            .x(function(d,i) {
                var value = xState.values[i];
                    value = x(value);
                return value; })
            .y(function(d) {
                var value = d;
                return y(value); });
        // helper function to map values to screen coordinates to
        var xMap = function(d,i){
            var current = xState.values[i];
            current = x(current);
            return current;
        };
        var yMap = function(d) {
            var current = d;
            current = y(current);
            return current;
        };
        var timeSeries =
            svg.selectAll(".observable")
            .data(that.getTimeSeries())
            .enter().append("g")
            .attr("class", "plot-observable")
            .each(function(d)
                  { switch(d.mode) {
  	          case that.modes.DOT:
                      var values = d.values
			            .filter(function(d,i)
					    { var x = xMap(d,i);
                                              var y = yMap(d);
                                              return (!isNaN(x) && !isNaN(y)) });
                      var tick =
                          d3.select(this)
                            .selectAll(".plot-tick")
                            .data(values)
                            .enter()
                            .append("g")
                            .attr("class", "plot-tick")
                            .attr("fill",d.color);


                      // add link to definitions
                      tick.append("use")
                          .attr("xlink:href",that.ticks.xlink(that.ticks.DOT))
                          .style("color",d.color)
                          .attr("transform"
                                ,function(d,i)
				 { var x = xMap(d,i);
                                   var y = yMap(d);
                                   var t = "translate(" + x + "," + y + ")";
                                   return t;
                                 });
                      break;
                  case that.modes.MARKS:
                      var values = d.values
			            .filter(function(d,i)
					    { var x = xMap(d,i);
                                              var y = yMap(d);
                                              return (!isNaN(x) && !isNaN(y)) });
                      var tick =
                          d3.select(this)
                            .selectAll(".plot-tick")
                            .data(values)
                            .enter()
                            .append("g")
                            .attr("class", "plot-tick")
                            .attr("fill",d.color);


                      // add link to definitions
                      tick.append("use")
                          .attr("xlink:href",that.ticks.xlink(d.tick))
                          .style("color",d.color)
                          .attr("transform"
                                ,function(d,i){ var x = xMap(d,i);
                                                var y = yMap(d);
                                                var t = "translate(" + x + "," + y + ")";
                                                return t;
                                              });
                      break;
                  case that.modes.LINE:
                      d3.select(this)
                          .append("path")
                          .attr("class", "plot-line")
                          .attr("d", function(d) { return line(d.values); })
                          .style("stroke", function(d) { return d.color; })
			  .style("fill", "none");
                      break;
                  case that.modes.HIDDEN:
                      break;
                  case that.modes.XAXIS:
                      break;
                  default:
                      break;
                  } });

        // render legend
        var legendRectSize = 12;
        var legendSpacing = 4;
        var legend = svg.selectAll('.legend')
            .data(that.showLegend?that.getTimeSeries():[])
            .enter()
            .append('g')
            .attr('class', 'plot-legend')
            .attr('transform',
                  function(d, i) {
                      var height = legendRectSize + legendSpacing;
                      var offset = 0;
                      var horz = legendRectSize;
                      var vert = i * height - offset;
                      return 'translate(' + horz + ',' + vert + ')';
                  });
        // cycle through styles
        var cycle = function(d,i)
                    { d.mode = that.modes.cycle(d.mode);
                      that.renderPlot();
                    };
        // legend swatches
        legend.append('rect')
              .attr("class", "plot-legend-swatch")
              .attr('width', legendRectSize)
              .attr('height', legendRectSize)
              .style('fill',
                     function(d){
                         var color = d.color;
                         if(that.modes.HIDDEN == d.mode){
                             color = "white";
                         };
                         return color;
                     })
              .style('stroke',
                     function(d){
                         var color = d.color;
                         if(that.modes.HIDDEN == d.mode){
                             color = "white";
                         };
                         return color;
                     }) // handle clicking on legend
              .style('opacity',
                     function(d){
                         var opacity = 1.0;
                         if(that.modes.HIDDEN == d.mode){
                             opacity = 0.0;
                         };
                         return opacity;
                     })
              .style('stroke-opacity',
                     function(d){
                         var opacity = 1.0;
                         if(that.modes.HIDDEN == d.mode){
                             opacity = 0.0;
                         };
                         return opacity;
                     }) // handle clicking on legend
              .on('click', cycle);
        // legend text
        legend.append('text')
            .attr('x', legendRectSize + legendSpacing)
            .attr('y', legendRectSize - legendSpacing)
            .text(function(d) { return d.label; })
            .on('click', cycle);

    }
    this.renderPlot = wrap(this.renderPlot);

    this.renderAxisSelect = function(){
	var reRender = true;
	if(that.select){
	    var oldState = that.select.selectAll('option').data();
	    var oldLabels = oldState.map(function(option){ return option.label; });
	    var newLabels = that.state.map(function(observable){ return observable.label; });
	    reRender = !is_same(oldLabels,newLabels);
	}
	if (reRender){
            /* check for the div to add the axis */
            if(that.plotDivAxisSelectId){
		// Clear div first
		var divAxisSelect = d3.select("#"+that.plotDivAxisSelectId);
		divAxisSelect.html("");
		/* Handler to update when the select changes */
		var changeHandler = function(){
                    var index = parseInt(this.options[this.selectedIndex].value);
                    assert(typeof index !== 'undefined',"plot selection invalid");
                    that.state.forEach(function(state,i){
			if(state.mode == that.modes.XAXIS){
                            state.mode = that.modes.DEFAULT;
			}
			if(i == index){
                            state.mode = that.modes.XAXIS;
			}
                    });
                    // re-render plot
                    that.renderPlot();
		}
		that.select = divAxisSelect.append("select")
                    .attr("class","form-control")
                    .on("change", changeHandler);
		that.select.selectAll('option')
                    .data(that.state)
                    .enter().append("option")
                    .attr("value",function(d,i) { return i; })
                    .text(function(d) { return d.label; });
		that.select.selectAll('option')
                    .filter(function(d){ return d.mode == that.modes.XAXIS; })
                    .attr("selected","true");
            }
	}
    }
    this.renderAxisSelect = wrap(this.renderAxisSelect);

    /* add label for plot */
    this.renderLabel = function(){
        var that = this;
        if(that.plotLabelDivId){
            if (that.start_time !== null){
                var label =
                    "Plot between t = "
                    +that.formatTime(that.start_time)
                    +" and t = "
                    +that.formatTime(that.end_time);
                d3.select("#"+that.plotLabelDivId)
                  .html(label);
            }
        }
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

    /* define how to export to tsv */
    this.plotName = "kappa-plot";
    this.getPlotName = function(suffix){
        var filename = that.plotName;
        filename = (filename.indexOf('.') === -1)?filename+suffix:filename;
        return filename;
    }
    this.setPlotName = function(plotName){ that.plotName = plotName; }

    /* render plot */
    this.redraw = function(){
        that.renderPlot();
        that.renderLabel();
        that.renderAxisSelect();
    };

    this.initialContent = function(mainDivId){
	var mainDiv = d3.select("#"+mainDivId);
	mainDiv.append("div").attr("id",this.plotLabelDiv).html("Plot");
	mainDiv.append("div").attr("id",that.plotDivId).attr("class","flex-content");
	var legendForm = mainDiv.append("form").attr("id","plot-legend-div").attr("class","form-inline");
	var showLegendDiv = legendForm.append("div").attr("class","checkbox").append("label");
	showLegendDiv.append("input").attr("type","checkbox")
	    .attr("checked","true").on("change",function () {
		that.setShowLegend(d3.event.currentTarget.checked);
		that.redraw();
	    });
	showLegendDiv.append("span").text(" Legend");
	var logXDiv = legendForm.append("div").attr("class","checkbox").append("label");
	logXDiv.text("Log X ");
	logXDiv.append("input").attr("type","checkbox").on("change",function () {
		that.setXAxisLog(d3.event.currentTarget.checked);
		that.redraw();
	    });;
	var logYDiv = legendForm.append("div").attr("class","checkbox").append("label");
	logYDiv.text("Log Y ");
	logYDiv.append("input").attr("type","checkbox").on("change",function () {
		that.setYAxisLog(d3.event.currentTarget.checked);
		that.redraw();
	    });;
	legendForm.append("div").attr("id",that.plotDivAxisSelectId).attr("class","form-group")
	    .append("select").attr("class","form-control");
    };

    this.initialContent(mainDivId);
}
