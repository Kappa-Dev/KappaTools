
// http://stackoverflow.com/questions/326596/how-do-i-wrap-a-function-in-javascript
var wrap = function(fn){
    return function(){
        try { return fn.apply(this, arguments);
            } catch(ex){
                error(ex.stack);
                throw ex;
            }
    };
};

var assert = function(condition, message) {
    if (!condition) {
        message = message || "Assertion failed";
        if (typeof Error !== "undefined") {
            throw new Error(message);
        }
        throw message; // Fallback
    }
}

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
    window.level = {};

    var levels = ["debug","info","notice","warning","error","fatal" ];
    for (var i in levels){
        var level = levels[i];
        // allow other modules to access levels via window.level[level]
        window.level[level] = level;
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
// https://medium.com/@graeme_boy/how-to-optimize-cpu-intensive-work-in-node-js-cdc09099ed41#.2vhd0cp4g
// http://www.codingdefined.com/2014/08/difference-between-fork-spawn-and-exec.html
// param {command,args,onStdout,onStderr,onClose}
var stdsimProcesses = [];

function spawnProcess(param){
    const spawn = require('child_process').spawn;
    const process = spawn(param.command, param.args);
    // log pid
    function failure(param,message){
	if(param.onError){
	    debug(message);
	    param.onError();
	}
	return null;
    }
    try {
	process.stdout.setEncoding('utf8');
	debug(`spawned process ${param.command} ${param.args} pid ${process.pid}`);
	if(param.onStdout) {
	    process.stdout.on('data',
			      function (data) {
				  debug(data);
				  param.onStdout(data); } );
	}
	if(param.onStderr) {
	    process.stderr.on('data',function (data) {
		param.onStderr(`${data}`);
	    } );
	}
	if(param.onClose){
	    process.on('close',param.onClose);
	}
	if(param.onError){
	    process.on('error',param.onError);
	}
	if(process && process.pid) {
	    return { write : function(data){ debug(data);
					     process.stdin.write(data); } ,
		     kill : function(){ process.kill(); }
		   };
	} else {
	    return failure(param,
			  `spawned failed ${param.command} ${param.args} pid ${process.pid}`);
	}
	stdsimProcesses.push(process);
    } catch(err){
	return failure(param,err.message);
    }
}

function jqueryOn(selector,event,handler){
    $(document.body).on(event,
			selector,
			function (e) { handler(e); });

}

// http://stackoverflow.com/questions/22395357/how-to-compare-two-arrays-are-equal-using-javascript-or-jquery
function is_same(array1,array2){
    var same =
    (array1.length == array2.length)
	&&
	array1.every(function(element, index) {
            return element === array2[index];
	});
    return same;
}
/* needed to add the stylesheet to the export */
var cssTextToken = "/* stylesheet : a5f23ffb-e635-435c-ae44-c10779c2a843 */";
function createSVGDefs(svg){
    var svgDefs = svg.append('defs')
	             .append("style")
	             .attr("type","text/css")
	             .text(cssTextToken);
    return svgDefs;
}
function plotPNG(plotDivId,title,plotName,plotStyleId){
    try { var html = d3.select("#"+plotDivId)
                       .select("svg")
                       .attr("title", title)
                       .attr("version", 1.1)
                       .attr("xmlns", "http://www.w3.org/2000/svg")
                       .attr("xmlns:xlink", "http://www.w3.org/1999/xlink")
                       .node()
                       .outerHTML;
          var style = plotStyleId?d3.select("#"+plotStyleId).text():"";
          style = "<![CDATA["+style+"]]>";
          html = html.replace(cssTextToken,style);
          var imgsrc = 'data:image/svg+xml;base64,'+ btoa(html);
          var canvas = document.createElement("canvas");
          var width = parseInt(d3.select("#"+plotDivId)
			         .select("svg")
			         .style("width")
  			         .replace("px", ""));
          var height = parseInt(d3.select("#"+plotDivId)
				  .select("svg")
				  .style("height")
				  .replace("px", ""));
          canvas.width = width; // get original canvas width
          canvas.height = height; //get original canvas height
          var context = canvas.getContext("2d");
          var image = new Image(width, height);
          image.onload = function() {
              context.fillStyle = "white";
              context.fillRect(0, 0, width, height);
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
          image.onerror = function(e){
              debug(e);
          }
          image.src = imgsrc;

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
                           //.attr("title", title)
                           .attr("version", 1.1)
                           .attr("xmlns", "http://www.w3.org/2000/svg")
                           .node()
                           .outerHTML;
              var style = plotStyleId?d3.select("#"+plotStyleId).text():"";
              style = "<![CDATA["+style+"]]>";
              html = html.replace(cssTextToken,style);
	      var header =
		"<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
              + "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n"
              + "\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n";
	      html = header+html;
              saveFile(html,"image/svg+xml",plotName);
        } catch (e) {
            alert(e);
        }
}

// http://werxltd.com/wp/2010/05/13/javascript-implementation-of-javas-string-hashcode-method/
function hashCode(s){
    s = s+s+s+s+s+s+s;
    var hash = 0;
    if (s.length == 0) return hash;
    var i = 0;
    for (i = 0; i < s.length; i++) {
        var char = s.charCodeAt(i);
        hash = ((hash<<5)-hash)+char;
        hash = hash & hash; // Convert to 32bit integer
    }
    return hash;
}

// http://stackoverflow.com/questions/57803/how-to-convert-decimal-to-hex-in-javascript
function hashColor(s){
    var hashInt = hashCode(s);
    if (hashInt < 0)
    { hashInt = 0xFFFFFFFF + hashInt + 1; }
    var hashColor = hashInt.toString(16);
    var hashString = String("000000" + hashColor).slice(-6);
    return "#"+hashString;
}

function ajaxRequest(url,type,data,handler,timeout){
    var parameter = { url : url , type : type };
    if(timeout){
	parameter.timeout = timeout;
    }
    if(data){ parameter.data = data; }
    debug(parameter);
    $.ajax(parameter)
     .done(function( data, textStatus, jqXHR )
	   { var status = jqXHR.status;
	     var response_text = jqXHR.responseText;
	     wrap(handler(status,response_text));
	   })
     .fail(function(jqXHR, textStatus, errorThrown )
	   { var status = jqXHR.status;
	     var response_text = jqXHR.responseText;
	     if(textStatus==="timeout") {
		 wrap(handler(408,"Timeout"));
	     } else {
		 wrap(handler(status,response_text));
	     }
	   });

}
/* Apply an action to a modal window. */
function modal(id,action){
    $(id).modal(action);
}
// http://stackoverflow.com/questions/105034/create-guid-uuid-in-javascript/2117523#2117523
/* Used primarily to create a client id */
function guid(){
    return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
	var r = Math.random()*16|0, v = c == 'x' ? r : (r&0x3|0x8);
	return v.toString(16);
    });
}
/* Throw an exception */
function toss(e){ throw e; }

/* Return the data of an element. */
function elementData(element,label){
    var result = $(element).data(label);
    return result;
}

/* create a jquery ui sort */
function createSort(id,handler){
    $("#"+id).sortable({
        items : "li:not(.ui-sort-disabled)",
	change : function() {
	    var list = $(this).closest('ul');
	    /* used to pin elements to the top */
	    var topAnchor = $(list).find('.ui-sort-top-anchor');
	    /* used to pin elements to the bottom */
	    var bottomAnchor = $(list).find('.ui-sort-bottom-anchor');
	    $(list).prepend($(topAnchor).detach());
	    $(list).append($(bottomAnchor).detach());
	}
    });
    $("#"+id).on("sortupdate",handler);
}
/* apply a map over the child nodes */
function childrenValue(element,selector,map){
    var result = [];
    $(element).children(selector).each(function(index){ result.push(map(this)); })
    return result;
}

function hideCodeMirror(){
    $.each($('.CodeMirror'),(_,cm) => $(cm.CodeMirror.getWrapperElement()).hide());
}

function showCodeMirror(){
    $.each($('.CodeMirror'),(_,cm) => $(cm.CodeMirror.getWrapperElement()).show());
}
