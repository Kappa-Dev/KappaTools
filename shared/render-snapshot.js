/*jshint esversion: 6*/
class SnapLayout {
    constructor(snapshot, dimension, margin) {
        this.snapshot = snapshot;
        this.dimension = dimension;
        this.margin = margin ||
            { top: 10, right: 10,
            bottom: 10, left: 10 };
        
    }
}


class Snapshot {
    constructor(id) {
        this.id = "#"+id;
    }

    setData(response) {
        let snapshot = this;
        let root = d3.select(this.id);
        snapshot.data = new DataWareHouse(JSON.parse(response));
            snapshot.clearData();
            let margin = { top: 10, right: 10,
            bottom: 10, left: 10 };
            let w = d3.select("#navcontents").node().getBoundingClientRect().width - margin.left - margin.right;        
            let h = d3.select("#navcontents").node().getBoundingClientRect().height - margin.top - margin.bottom - 34.5 - 34 - 28.5;
            if (snapshot.data) {
                let layout = new SnapLayout(snapshot, new Dimension(w, h), margin);
                let renderer = new SnapRender(root, layout);
                renderer.render();
            }        
    }

    clearData() {
        d3.select(this.id).selectAll("svg").remove();
        d3.select(this.id).selectAll("div").remove();
        d3.selectAll(".snap-tooltip").remove();
        d3.selectAll(".legend").remove();

    }

}

class SnapRender {
    constructor(root, layout) {
        this.root = root;
        let width = layout.dimension.width;
        let height = layout.dimension.height;
        this.layout = layout;

        let svgWidth = width +
                            this.layout.margin.left +
                            this.layout.margin.right;
        let svgHeight = height +
                            this.layout.margin.top +
                            this.layout.margin.bottom;
        let container = this.container = this.root
            .append("div")
            .classed("render-container flex-content", true)
            .style("position", "relative")
            .append("svg")
            .attr("class", "svg-group")
            .attr("id", "map-container")
            .attr("preserveAspectRatio", "xMinYMin meet")
            .attr("viewBox", "0 0 " + svgWidth  + " " + svgHeight );
            
        let svg = this.svg = container.append('g');
        let data = this.layout.snapshot.data;
        data.generateTreeData();
        //console.log(treeData);
        function zoomed() {
            svg.attr('transform', d => d3.event.transform );
        }

        let zoom = d3.zoom().scaleExtent([0.5, 10]).on('zoom', zoomed);
        container.call(zoom);
        container.call(d3.drag().on('drag', () => svg.attr('transform', 'translate(' + d3.event.x + ',' + d3.event.y +')')));
        /* add behavior for reset zoom button */

        d3.select("#resetButton").on("click", reset);

        function reset() {
            container.transition().duration(750)
            .call(zoom.transform, d3.zoomIdentity);
        }

        this.coloring = {};
        this.marking = {};
        this.tooltip = new SnapUIManager(this);
    }

    render() {
        console.log("rendering");
        this.renderTreeMap();
        this.renderNodes();
        this.tooltip.renderLegend();
    }

    renderTreeMap() {
        let renderer = this;
        let data = this.layout.snapshot.data;
        let width = this.layout.dimension.width;
        let height = this.layout.dimension.height;
        let layout = this.layout;
        let svg = this.svg;
        let treemap = this.treemap = d3.treemap()
            .tile(d3.treemapResquarify)
            .size([width, height])
            .round(true)
            .paddingInner(4);
        let root = d3.hierarchy(data.treeData)
                .eachBefore(d => { d.data.id = (d.parent ? d.parent.data.id + "." : "") + d.data.name; })
                .sum( d => d.count * d.size )
                .sort((a, b) => { return b.height - a.height || b.value - a.value; });

        treemap(root);

        let cell = this.cell = this.svg.selectAll(".treeSpecies")
            .data(root.leaves())
            .enter().append("g")
                .attr("class", "treeSpecies")
                .attr("id", d => d.data.id)
                .attr("transform", d => { let x = d.x0 + (layout.margin.left + layout.margin.right)/2;
                                            let y = d.y0 + (layout.margin.top + layout.margin.bottom)/2;
                                            return "translate(" + x + "," + y + ")"; });


        cell.append("rect")
                .attr("width", d => { return d.x1 - d.x0; })
                .attr("height", d => { return d.y1 - d.y0; })
                .attr("fill", d => { return "grey"; })
                .on("mouseover", mouseoverSpecies)
                .on("mouseout", mouseoutSpecies)
                .on("click", markSpecies);
            
        cell.exit().remove();

        function mouseoverSpecies(d) {
            let species = d;
            svg.selectAll(".treeRects").filter(d => d.parent.data.name === species.data.name).attr("fill", d => renderer.coloring[d.data.name].darker());
            renderer.tooltip.showSpecies(d);
        }

        function mouseoutSpecies(d) {
            let species = d;
            svg.selectAll(".treeRects").filter(d => d.parent.data.name === species.data.name && renderer.marking[d.parent.data.name] !== 1 ).attr("fill", d => renderer.coloring[d.data.name]);           
            renderer.tooltip.hideSpecies(d);
        }

        function markSpecies(d) {
            let species = d;
            if (renderer.marking[d.data.name] === undefined) {
                renderer.marking[d.data.name] = 1;
            }
            else if (renderer.marking[d.data.name] === 1) {
                renderer.marking[d.data.name] = 0;
            }
            else {
                renderer.marking[d.data.name] = 1;
            }
            svg.selectAll(".treeRects").filter(d => d.parent.data.name === species.data.name)
                .attr("fill", d => { 
                    if (renderer.marking[d.parent.data.name] === 1 ) {
                        return renderer.coloring[d.data.name].darker();
                    }
                    return renderer.coloring[d.data.name]; 
                });  
            //console.log(renderer.marking);
        }
    }

    renderNodes() {
        let renderer = this;
        let c20 = d3.scaleOrdinal(d3.schemeCategory20);
        let data = this.layout.snapshot.data;
        for (let mixture in data.snapshot) {
            let id = data.snapshot[mixture].id;
            let cell = this.svg.select("#root\\.mixture" + id);
            //console.log(cell.data());

            let width = cell.data()[0].x1 - cell.data()[0].x0;
            let height = cell.data()[0].y1 - cell.data()[0].y0;
            
            // console.log(cell.data()[0], width, height);

            let treemap = this.nodeTreemap = d3.treemap()
                .tile(d3.treemapResquarify)
                .size([width - 4, height - 4])
                .round(true)
                .paddingInner(0);
                
            let tree = data.getSpeciesTree(id);

            //console.log(tree);
            let root = this.nodeRoot = d3.hierarchy(tree)
            .eachBefore(d => { d.data.id = (d.parent ? d.parent.data.id + "." : "") + d.data.name; })
            .sum(d => d.size)
            .sort(function(a, b) { return b.height - a.height || b.value - a.value; });
            
            //console.log(renderer.coloring);
            treemap(root);
            
            let node = this.node = cell.selectAll(".treeNodes")
                .data(root.leaves())
                .enter().append("g")
                    .attr("class", "treeNodes")
                    .attr("id", d => d.data.id)
                    .attr("transform", d => { let x = d.x0 + 2;
                                                     let y = d.y0 + 2;
                                                     return "translate(" + x + "," + y + ")"; });


            node.append("rect")
                    .attr("class", "treeRects")
                    .attr("id", d => d.data.id )
                    .attr("width", d => d.x1 - d.x0 )
                    .attr("height", d => d.y1 - d.y0 )
                    .attr("fill", (d, i) => { 
                        if (renderer.coloring[d.data.name] === undefined) {
                            renderer.coloring[d.data.name] = d3.rgb(c20(Object.keys(renderer.coloring).length));
                        } 
                        if (renderer.marking[d.parent.data.name] === 1)
                            return renderer.coloring[d.data.name].darker();
                        return renderer.coloring[d.data.name]; })
                    .style('pointer-events', 'none');

            node.exit().remove();
        }

      
 
    }
    removeNodes() {
        d3.selectAll(".treeNodes").remove().transition()
                .duration(700);
    }

}