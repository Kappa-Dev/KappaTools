"use strict"


function observable_plot(configuration){
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


    /* plotDivId                - div which plot is rendered on
       plotLabelDivId           - div to place plot interval
       plotStyleId              - style sheet for the plot need to export
                                  plot
       plotShowLegendCheckboxId - checkbox to toggle plot legend
       plotXAxisLogCheckboxId   - checkbox to toggle log on x axis
       plotYAxisLogCheckboxId   - checkbox to toggle log on y axis
     */
    this.configuration = configuration;

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
    this.state = [];

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
    /* Get all observables.
     */
    this.getObservables = function(){
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

    this.timeLabel = "Time";


    this.setRawData = function(plot){
        that.rawData = plot;
    }
    this.getRawData = function(){
        return that.rawData;
    }

    /* Update the plot data of the graph.  This is called
       when new data is to be displayed in a graph.  Care
       is taken not to reset the state of the plot when
       data is updated.
     */
    this.setPlot = function(plot){
        that.setRawData(plot);
        var legend = plot.legend;

        /* An update new copy over state preserving settings
         * where possible.
         */
        var new_state = [];
        var time_values = []; // store the times values

        /* Initialize new state element with the corresponding
         * observables.
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
        // Populate observables from data.
        plot.observables.forEach(function(observable){
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
	if (first_time && last_time){
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
        var color = d3.scale.category10();
        color.domain(that.state.map(function(c,i){ return i; }));
        that.state.forEach(function(s,i){ s.color = color(i);
                                          s.tick = that.ticks.index(i);
                                        });
        that.render();

    };
    this.setPlot = wrap(this.setPlot);
    this.formatTime = d3.format(".02f");
    this.getXAxis = function(){
        return this.state.find(function(state){ state.mode = that.modes.XAXIS });
    }
    this.setPlot = wrap(this.setPlot);
    this.renderPlot = function(){
        // set margin
        var margin = {top: 20, right: 80, bottom: 30, left: 80},
            dimensions = that.getDimensions() ,
            width = dimensions.width - margin.left - margin.right,
            height = dimensions.height - margin.top - margin.bottom;

        // setup x-axis
        var x = (that.getXAxisLog()?d3.scale.log().clamp(true):d3.scale.linear()).range([0, width]);
        var xState = that.getStatesByMode(that.modes.XAXIS)[0];
        x.domain(d3.extent(xState.values));
        var xAxis = d3.svg.axis().scale(x).orient("bottom");

        // setup y-axis
        var y = (that.getYAxisLog()?d3.scale.log().clamp(true):d3.scale.linear()).range([height, 0]);
        var yAxis = d3.svg
                      .axis()
                      .scale(y)
                      .orient("left")
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
        d3.select("#"+that.configuration.plotDivId).html("");
        // Add svg element
        var svg = d3.select("#"+that.configuration.plotDivId).append("svg")
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
        var line = d3.svg.line()
            .interpolate("linear")
            .defined(function(d,i) { return !that.getXAxisLog() || xState.values[i] > 0; })
            .defined(function(d,i) { return !that.getYAxisLog() || d > 0; })
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
        var observables =
            svg.selectAll(".observable")
            .data(that.getObservables())
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
                          .style("stroke", function(d) { return d.color; });
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
            .data(that.getObservables())
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

    /* add select for the x-axis */
    this.setupAxisSelect = function(){
        var that = this;
        /* check for the div to add the axis */
        if(that.configuration.plotDivAxisSelectId){
            // Clear div first
            that.divAxisSelect = d3.select("#"+configuration.plotDivAxisSelectId);
            that.divAxisSelect.html("");
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
            that.divAxisSelect = that.divAxisSelect
                .append("select")
                .attr("class","form-control")
                .on("change", changeHandler);
            that.divAxisSelect
                .selectAll('option')
                .data([])
                .enter()
                .append('option');
        }
    }
    this.setupAxisSelect = wrap(this.setupAxisSelect);

    this.updateAxisSelect = function(){
            that.divAxisSelect
                .selectAll('option')
                .remove();

            that.divAxisSelect
                .selectAll('option')
                .data(that.state)
                .enter().append("option")
                .attr("value",function(d,i) { return i; })
                .text(function(d) { return d.label; });
            that.divAxisSelect
                .selectAll('option')
                .filter(function(d){ return d.mode == that.modes.XAXIS; })
                .attr("selected","true");

    }
    this.updateAxisSelect = wrap(this.updateAxisSelect);

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
            if(that.configuration.plotDivAxisSelectId){
		// Clear div first
		var divAxisSelect = d3.select("#"+configuration.plotDivAxisSelectId);
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
        if(that.configuration.plotLabelDivId){
            if (that.start_time){
                var label =
                    "Plot between t = "
                    +that.formatTime(that.start_time)
                    +"s and t = "
                    +that.formatTime(that.end_time)
                    +"s";
                d3.select("#"+configuration.plotLabelDivId)
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

    this.dimensions = { width : 960 , height : 500 };
    this.setDimensions = function(dimensions) {
        that.dimensions = dimensions;
    }
    this.getDimensions = function(){
        return that.dimensions;
    }

    /* define how to export to tsv */
    this.plotName = "kappa-plot";
    this.getPlotName = function(suffix){
        var filename = that.plotName;
        filename = (filename.indexOf('.') === -1)?filename+suffix:filename;
        return filename;
    }
    this.setPlotName = function(plotName){ that.plotName = plotName; }
    this.handlePlotTSV = function(){
        var rawData = that.getRawData();
        var observables = rawData.observables;
        var legend = rawData.legend;
        var header = "'time'\t"+legend.join("\t");
        var body = observables.map(function(d)
                                   { var row = [d["time"]];
                                     row = row.concat(d["values"]);
                                     return row.join("\t") })
                              .join("\n");
        var tsv = header+"\n"+body;
        saveFile(tsv,"text/tab-separated-values",that.getPlotName(".tsv"));
    }
    this.handlePlotTSV = wrap(this.handlePlotTSV);

    /* add checkbox handlers for display options */
    this.addHandlers = function(){
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
    }
    /* render plot */
    this.render = function(){
        that.renderPlot();
        that.renderLabel();
        that.updateAxisSelect();
        that.renderAxisSelect();
    };
    this.addHandlers();
    this.setupAxisSelect();
}
