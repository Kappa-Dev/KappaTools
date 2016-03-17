var write_log = false;
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
(function(){
    var found = false;
    var levels = ["debug","info","notice","warning","error","fatal" ];
    for (i in levels){
        level = levels[i];
        if(args.level && args.level === level) { found = true; }
        if(found) {
            window[level] = function(x){ console.log(x); }
        } else {
            window[level] = function(x){ }
        }
    }
})();

function jqueryOn(selector,event,handler){
    //$(selector).on(event,handler);
    //$('#navflux').on('shown.bs.tab', function (e) {
    //console.log(event);
    $(selector).on(event, function (e) {
        console.log(e);
        handler(e); });
}
