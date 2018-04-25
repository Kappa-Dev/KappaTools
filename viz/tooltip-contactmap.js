/*jshint esversion: 6*/

class UIManager {
    constructor(renderer) {
        // Define the div for the tooltip
        this.renderer = renderer;
        this.buttonClicked = 0;
        let UI = this;

        this.tip = renderer.root.append("div")
            .attr("class", "tooltip")
            .style("font-size", "1.5em")
            .style("opacity", 0)
            .style("padding", "0.5em");

        let toolbox = this.toolbox = renderer.root.append("div")
            .attr("class", "toolbox");

        toolbox.append("input")
            .attr("id", "cycleCheckbox")
            .attr("type", "checkbox")
            .on("change", toggleCycleDetection);
        toolbox.append('label')
            .attr('for', 'cycleCheckbox')
            .text("Interactive Mode");

        toolbox.append("input")
            .attr("id", "toggleLines")
            .attr("type", "checkbox")
            .attr("disabled", "disabled")
            .on("change", toggleHelperLines);
        toolbox.append('label')
            .attr('for', 'toggleLines')
            .text("Hide Guide Lines");

        this.toggles = toolbox.append("div")
            .attr("class", "stateButtonDiv");

        this.toggles.append("input")
            .attr("class", "stateButton")
            .attr("type", "button")
            .attr("value", "Show All States")
            .on("click", showStates);

        this.toggles.append("input")
            .attr("id", "resetZoomButton")
            .attr("class", "stateButton")
            .attr("type", "button")
            .attr("value", "Reset Zoom");

        function toggleCycleDetection() {
            //console.log("checked");
            if (d3.select("#cycleCheckbox").property("checked")) {
                renderer.cycleDetect = true;
                renderer.rerender();
                renderer.root.select("#toggleLines").attr("disabled", null);
                console.log("cycled");
            }
            else
            {
                renderer.cycleDetect = false;
                renderer.rerender();
                renderer.root.select("#toggleLines").attr("disabled", "disabled");
            }
        }

        function toggleHelperLines() {
            if (d3.select("#toggleLines").property("checked")) {
                renderer.toggleLines = true;
                renderer.root.selectAll(".link").filter( d => d.side !== 0 && !d.clicked)
                    .style("stroke-opacity", opacity.line_hidden)
                    .style("stroke-width", 2);

            }
            else
            {
                renderer.toggleLines = false;
                renderer.root.selectAll(".link").filter( d => d.side || d.clicked)
                    .style("stroke-opacity", d => d.clicked ? opacity.line_highlight: opacity.line_side)
                    .style("stroke-width", 8);
            }
        }

        function showStates() {
            UI.buttonClicked = UI.buttonClicked === 0 ? 1: 0;
            let states = renderer.svg.selectAll(".stateLink").
		filter( d => d.states.length > 0 );
            //let siteNodes = renderer.svg.selectAll("")
            if (UI.buttonClicked) {
                states.attr('opacity', 1);
                renderer.svg.selectAll('.outerSite').filter( d => d.states.length > 0 )
                    .style("fill", function(d) {
                    d.clicked = 1;
                    d.currentColor = "white";
                    return d.currentColor;
                });
                renderer.root.select(".stateButton").attr("value", "Hide All States");
            }
            else {
                states.attr('opacity', 0);
                renderer.svg.selectAll('.outerSite')
                    .style("fill", function(d) {
                    d.clicked = 0;
                    d.currentColor = d.agent.color;
                    return d.currentColor;
                });
                renderer.root.select(".stateButton").attr("value", "Show All States");
            }
        }
    }

    showSite(d) {
        let site = d;
        this.tip
            .style("opacity", 1);
        this.tip
            .text( () => {
                if(site.states.length > 0) {
                    let text = "agent: " + site.agent.label + "\n" +
                        "site: " + site.label + "\n" +
                        "states: ";
                    for (let i in site.states) {
                        text = text + site.states[i].name + "\n";
                    }
                    return text;
                }
                else
                    return "agent: " + site.agent.label + "\n" +
                    "site: " + site.label + "\n" +
                    "states: none" ; })
            .style('color', site.agent.color);
    }

    showNode(d) {
        this.tip
            .style("opacity", 1);
        this.tip
            .text("agent: " + d.data.label)
            .style('color', d.data.color);
    }

    hide() {
        this.tip
            .style("opacity", 0);
    }
}
