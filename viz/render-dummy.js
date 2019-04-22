"use strict"

class DummyRenderer {
    constructor(id) {
	this.id = "#"+id;
	this.data = "null";
    }
    clearData() {
    	d3.select(this.id).selectAll("*").remove();
    }
    redraw() {
	this.clearData();
	var topdiv = d3.select(this.id);
        let margin = { top: 10, right: 10, bottom: 10, left: 10 };
        let w = topdiv.node().getBoundingClientRect().width - margin.left - margin.right;
	let h = topdiv.node().getBoundingClientRect().height - margin.top - margin.bottom;
	topdiv.append("p").text("Box: "+w+"x"+h);
	topdiv.append("p").text(this.data);

    }
    setData(json) {
	this.data = json;
	redraw();
    }
}
