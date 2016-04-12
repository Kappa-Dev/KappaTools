// http://stackoverflow.com/questions/979975/how-to-get-the-value-from-the-url-parameter
var args = function () {
    var query_string = {};
    var query = window.location.search.substring(1);
    var vars = query.split("&");
    for (var i=0;i<vars.length;i++) {
        var pair = vars[i].split("=");
        if (typeof query_string[pair[0]] === "undefined") {
            query_string[pair[0]] = decodeURIComponent(pair[1]);
        } else if (typeof query_string[pair[0]] === "string") {
            var arr = [ query_string[pair[0]],decodeURIComponent(pair[1]) ];
            query_string[pair[0]] = arr;
        } else {
            query_string[pair[0]].push(decodeURIComponent(pair[1]));
        }
    }
    return query_string;
}();

// add logging functions
// in the url place level=debug for most verbose
(function(){
    var found = false;
    var levels = ["debug","info","notice","warning","error","fatal" ];
    for (i in levels){
        level = levels[i];
        if(args.level && args.level === level) { found = true; }
        if(found) {
            window[level] = function(x){
                console.log(x);
            }
        } else {
            window[level] = function(x){ }
        }
    }
})();

function jqueryOn(selector,event,handler){
    $(selector).on(event, function (e) {
        handler(e); });
}

// http://stackoverflow.com/questions/22395357/how-to-compare-two-arrays-are-equal-using-javascript-or-jquery
function is_same(array1,array2){
    (array1.length == array2.length) && array1.every(function(element, index) {
	return element === array2[index];
    });
}

var cssTextToken = "/* stylesheet : a5f23ffb-e635-435c-ae44-c10779c2a843 */";
function createSVGDefs(svg){
    var svgDefs = svg.append('defs').append("style").attr("type","text/css").text(cssTextToken);
    return svgDefs;
}
function plotPNG(plotDivId,title,plotName,plotStyleId){
	try { var html = d3.select("#"+plotDivId)
		           .select("svg")
	                   .attr("title", title)
	                   .attr("version", 1.1)
	                   .attr("xmlns", "http://www.w3.org/2000/svg")
	                   .node().parentNode.innerHTML;
	      var style = plotStyleId?d3.select("#"+plotStyleId).text():"";
	      style = "<![CDATA["+style+"]]>";
	      html = html.replace(cssTextToken,style);
	      var imgsrc = 'data:image/svg+xml;base64,'+ btoa(html);
	      var canvas = document.createElement("canvas");
	      var width = parseInt(d3.select("#"+plotDivId).select("svg").style("width").replace("px", ""))
	      var height = parseInt(d3.select("#"+plotDivId).select("svg").style("height").replace("px", ""))
	      canvas.width = width; // get original canvas width
	      canvas.height = height; //get original canvas height
	      var context = canvas.getContext("2d");
	      var image = new Image(width, height);
	      image.src = imgsrc;
	      image.onload = function() {
		  context.drawImage(image, 0, 0, width, height);
		  var canvasdata = canvas.toDataURL("image/png");
		  var a = document.createElement("a");
		  a.style = "display: none";
		  document.body.appendChild(a);
		  a.download = plotName;
		  a.href = canvasdata;
		  a.click();
		  document.body.removeChild(a);
	      };
	    } catch (e) {
		alert(e);
	    }
}

function saveFile(data,mime,filename){
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

function plotSVG(plotDivId,title,plotName,plotStyleId){
	try { var html = d3.select("#"+plotDivId)
		           .select("svg")
	                   .attr("title", title)
	                   .attr("version", 1.1)
	                   .attr("xmlns", "http://www.w3.org/2000/svg")
	                   .node().parentNode.innerHTML;
	      var style = plotStyleId?d3.select("#"+plotStyleId).text():"";
	      style = "<![CDATA["+style+"]]>";
	      html = html.replace(cssTextToken,style);
	      saveFile(html,"image/svg+xml",plotName);
	} catch (e) {
	    alert(e);
	}
}
