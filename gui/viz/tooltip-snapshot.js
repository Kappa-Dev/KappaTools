/*jshint esversion: 6*/
class SnapUIManager {
    constructor(renderer) {
        // Define the div for the tooltip
        this.renderer = renderer;
        this.buttonClicked = 0;
        let UI = this;

        this.tip = d3.select(".render-container").append("div")	
            .attr("class", "snap-tooltip")	
            .style("font-size", "2em")			
            .style("padding", "0.5em");
           /* add button functionality */
        
        let timeout = d3.timeout(function() {
            d3.select("input[value=\"sumByMass\"]")
                .property("checked", true)
                .dispatch("change");
        }, 10);
        
        
        d3.selectAll("input")
            .data([sumBySize, sumByCount, sumByMass], function(d) { return d ? d.name : this.value; })
            .on("change", changed);

        function changed(sum) {
            let width = renderer.layout.dimension.width;
            let data = renderer.layout.snapshot.data;
            let height = renderer.layout.dimension.height;
            let treemap = d3.treemap()
                .tile(d3.treemapResquarify)
                .size([width, height - 20])
                .round(true)
                .paddingInner(4);

            let root = d3.hierarchy(data.treeData)
                .eachBefore(d => { d.data.id = (d.parent ? d.parent.data.id + "." : "") + d.data.name; })
                .sum( sum )
                .sort((a, b) => { return b.height - a.height || b.value - a.value; });
            
            /*
            let treemap = renderer.treemap;
            let root = renderer.root
                        .sum( sum )
                        .sort((a, b) => { return b.height - a.height || b.value - a.value; });
            */
            treemap(root);
            timeout.stop();
            
            //renderer.nodeTreemap(renderer.nodeRoot);
            d3.selectAll(".treeSpecies")
                .data(root.leaves())
                .attr("id", d => d.data.id)
            .transition()
                .duration(750)
                .attr("transform", d => { let x = d.x0 + (renderer.layout.margin.left + renderer.layout.margin.right)/2;
                                            let y = d.y0;
                                            return "translate(" + x + "," + y + ")"; })
                
                .select("rect")
                .attr("width", d => { return d.x1 - d.x0; })
                .attr("height", d => { return d.y1 - d.y0; });

            renderer.removeNodes();
            renderer.renderNodes();
            
        }

        function sumByCount(d) {
            return d.count;
        }

        function sumBySize(d) {
            return d.size;
        }

        function sumByMass(d) {
            return d.count * d.size;
        }
    }
    
    renderLegend() {
        let legendRectSize = 25;
        let legendSpacing = 10;
        let padding = 10;
        let renderer = this.renderer;
        let dataArray = Object.keys(renderer.coloring).map( (d) => {
            let tempObj = {};
            tempObj[d] = renderer.coloring[d].brighter(1.5);
            return tempObj;
        } );
        
        let legendHeight = d3.select(".snapshot-legend").node().getBoundingClientRect().height;
        let legendSVG = d3.select(".snapshot-legend")
            .style("height", legendHeight + "px")
            .append("svg")
            .attr("class", "legend-container")   
            .attr("height", (legendRectSize + legendSpacing) * dataArray.length + 2 * padding )
            .attr("width", "100%" )
            .attr("preserveAspectRatio", "xMinYMin meet");        	
        
        window.addEventListener("resize", resizeLegend);
        
        function resizeLegend() {
            let legendHeight = d3.select(".snapshot-legend").node().getBoundingClientRect().height;
            let legendSVG = d3.select(".snapshot-legend")
                .style("height", legendHeight +"px");
        }

        let legend = legendSVG.selectAll(".legend-elements")           
        .data(dataArray)      
          .enter()                                                                        
          .append('g');    
        legend.merge(legend)   
          .attr("class", "legend-elements")                   
          .attr('transform', function(d, i) {                     
            let height = legendRectSize + legendSpacing;            
            let horz = legendRectSize;                        
            let vert = i * height + padding;                        
            return 'translate(' + horz + ',' + vert + ')';         
          });                 

        legend.append('rect')                                      
          .attr('width', legendRectSize)                           
          .attr('height', legendRectSize)                          
          .style('fill', d => d[Object.keys(d)[0]] )                                  
          .style('stroke', d => d[[Object.keys(d)[0]]] );                                 

        legend.append('text')                                      
          .attr('x', legendRectSize + legendSpacing)               
          .attr('y', legendRectSize - legendSpacing)               
          .text( d => Object.keys(d)[0] );                        
    }

    showSpecies(d) { 
        let renderer = this.renderer;
        this.tip
            .style("opacity", 1)
            .style("background", d => {
                let color = d3.rgb("white");
                color.opacity = 0.8;
                return color;
            });

        let speciesTip = this.tip
            .text("count: " + d.data.count + "\nsize: " + d.data.size)
            .style('color', "black");
    }     

    hideSpecies() {
        let renderer = this.renderer;
        this.tip
            .style("opacity", 0)
            .style("background", d => {
                let color = d3.rgb("white");
                color.opacity = 0;
                return color;
            });
        this.tip
            .text("");
    }
}
