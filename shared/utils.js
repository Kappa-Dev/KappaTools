/*jshint esversion: 6*/

function getTransform(selection) {
    let transform = {};
    transform.translate = selection.attr("transform").substring(selection.attr("transform").indexOf("(")+1, selection.attr("transform").indexOf(")")).split(",");
    transform.rotate = selection.attr("transform").substring(selection.attr("transform").indexOf("rotate(") + 'rotate'.length + 1, selection.attr("transform").indexOf(")", selection.attr("transform").indexOf(")") + 1));
    return transform;
}

function calculateTextWidth(size) {
        let svg = d3.select("svg");
        let text = svg.append("text")
	        .attr("x", 10)
	        .attr("y", 30)
            .style('font-size', size)
	        .text("a");
        let tWidth = svg.select("text").node().getComputedTextLength();
        text.remove();
        return tWidth;

    }