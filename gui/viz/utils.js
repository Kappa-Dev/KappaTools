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
        .attr("id", "calculateText")
        .style('font-size', size)
	.text("a");
    let tWidth = svg.select("#calculateText").node().getComputedTextLength();
    text.remove();
    return tWidth;
}

function dedup(array) {
    return array.filter((elem, pos, arr) => {
	return array.indexOf(elem) == pos;
    });
}

function getBoundingBoxCenterX (selection) {
    let element = selection.node();
    let bbox = element.getBBox();
    return bbox.x + bbox.width/2;
}

function getBoundingBoxCenterY (selection) {
    let element = selection.node();
    let bbox = element.getBBox();
    return bbox.y + bbox.height/2;
}
