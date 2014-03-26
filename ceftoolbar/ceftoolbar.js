function updateTime() {
    $("#time").text(moment().format('MMM Do YYYY, h:mm:ss a'));
};

function callUpdate() {
    external.updateCallback(doUpdate);
};

function doUpdate(obj) {
    $("#stuff").text(JSON.stringify(obj));
    $("#cpu").text(obj["CPU"]);
    coreSet(parseFloat(obj["CPU1"]),
	    parseFloat(obj["CPU2"]),
	    parseFloat(obj["CPU3"]),
	    parseFloat(obj["CPU4"]));
    $("#cpu").css('color', heatmapColour(parseFloat(obj["CPU"])/100));
    $("#ram").text(obj["RAM"]);
    $("#vol").text(obj["VOL"]);
    $("#bat").text(obj["BAT"]);
    var batcol = obj["BATSTATUS"] == "Charging" ? "green" : "grey";
    $("#bat").css("border-bottom-color", batcol);
    $("#bat").css('color', heatmapColour(1-(parseFloat(obj["BAT"])/100)));
    $("#wifi").text(obj["WIFI"]);
    $("#wifiup").text(obj["WIFIUP"]);
    $("#wifidown").text(obj["WIFIDOWN"]);
    netUpdate(parseFloat(obj["WIFIDOWN"]), parseFloat(obj["WIFIUP"]));
    $("#task").text(obj["TASK"]);
    $("#mpdstat").text(obj["MPDSTAT"]);
    $("#mpdalbum").text(obj["MPDALBUM"]);
    $("#mpdartist").text(obj["MPDARTIST"]);
    $("#mpdtitle").text(obj["MPDTITLE"]);
    var elapsed = moment.duration("00:" + obj["MPDELAPSED"]).asMinutes();
    var length = moment.duration("00:" + obj["MPDLENGTH"]).asMinutes();
    $("#progBar").val(elapsed/length);
};

var t = 9001,
    data = d3.range(50).map(function(x) {return({time: x, value: x * 2});});

var colours = ["#00ff00", "#FFFF00", "#FFA500", "#FF0000", "#FF0000"];

var heatmapColour = d3.scale.linear()
  .domain(d3.range(0, 1, 1.0 / (colours.length - 1)))
  .range(colours);

function nextPoint(val) {
    data.shift();
    data.push({
	time: ++t,
	value: val
    });
}

var chart3;
window.onload = function() {
    setInterval(updateTime, 500);

    var w = 1,
	h = 14;

    var x = d3.scale.linear()
	.domain([0, 1])
	.range([0, w]);

    var y = d3.scale.linear()
	.domain([0, 100])
	.rangeRound([0, h]);

    chart3 = d3.select("#cpugraph").append("svg")
	.attr("class", "chart")
	.attr("width", w * data.length - 1)
	.attr("height", h);

    chart3.append("line")
	.attr("x1", 0)
	.attr("x2", w * data.length)
	.attr("y1", h - .5)
	.attr("y2", h - .5)
	.style("stroke", "#000");

    redraw3();

    function redraw3() {

	var rect = chart3.selectAll("rect")
	    .data(data, function(d) { return d.time; });

	rect.enter().insert("rect", "line")
	    .attr("x", function(d, i) { return x(i + 1) - .5; })
	    .attr("y", function(d) { return h - y(d.value) - .5; })
	    .attr("width", w )
	    .attr("height", function(d) { return y(d.value); })
	    .attr("fill", function(d) { return heatmapColour(d.value/100.0); })
	    .attr("stroke", function(d) { return heatmapColour(d.value/100.0); })
	    .transition()
	    .duration(1000)
	    .attr("x", function(d, i) { return x(i) - .5; });

	rect.transition()
	    .duration(1000)
	    .attr("x", function(d, i) { return x(i) - .5; });

	rect.exit().transition()
	    .duration(1000)
	    .attr("x", function(d, i) { return x(i - 1) - .5; })
	    .remove();

    }

    setInterval(function() {
	redraw3();
	d3.timer.flush(); // avoid memory leak when in background tab
    }, 1500);

};

var coredata = d3.range(4).map(function(x) {return(x * 25);});

function coreSet(cpu1, cpu2, cpu3, cpu4) {
    coredata = [cpu1, cpu2, cpu3, cpu4];
}


// CPU cores
var corechart;
window.addEventListener('load', function() {
    var w = 5,
	h = 14;

    var x = d3.scale.linear()
	.domain([0, 1])
	.range([0, w]);

    var y = d3.scale.linear()
	.domain([0, 100])
	.rangeRound([2, h]);

    corechart = d3.select("#coregraph").append("svg")
	.attr("class", "chart")
	.attr("width", w * coredata.length)
	.attr("height", h);

    drawcore();

    function drawcore() {

	var rect = corechart.selectAll("rect")
	    .data(coredata);

	rect.enter().insert("rect", "line")
	    .attr("x", function(d, i) { return x(i) - .5; })
	    .attr("y", function(d) { return h - y(d) - .5; })
	    .attr("width", w - 2 )
	    .attr("height", function(d) { return y(d); })
	    .attr("fill", function(d) { return heatmapColour(d/100.0); })
	    .transition()
	    .duration(1000)
	    .attr("x", function(d, i) { return x(i) - .5; });

	rect.transition()
	    .duration(1000)
	    .attr("fill", function(d) { return heatmapColour(d/100.0); })
	    .attr("y", function(d) { return h - y(d) - .5; })
	    .attr("height", function(d) { return y(d); });

	rect.exit().transition()
	    .duration(1000)
	    .remove();

    }

    setInterval(function() {
	drawcore();
	d3.timer.flush(); // avoid memory leak when in background tab
    }, 1500);

});

var netgraph;

var nt = 0;
var netdata = d3.range(33).map(function(x) {return(x*2);});
var netdataup = d3.range(33).map(function(x) {return(x);});

function netUpdate(down, up) {
    netdata.shift();
    netdataup.shift();
    netdata.push(down);
    netdataup.push(up);
}

window.addEventListener('load', function() {
    //////////////////////////////
    // Line graph
    //////////////////////////////

    var w = 40,
	h = 12;

    var x = d3.scale.linear()
	.domain([0, netdata.length - 1])
	.range([0, w]);

    var y = d3.scale.linear()
	.domain([0, 3000])
	.rangeRound([h, 0]);

    netgraph = d3.select("#netgraph").append("svg")
	.attr("class", "chart")
	.attr("width", w)
	.attr("height", h);

    netgraph.append("defs").append("clipPath")
    	.attr("id", "clip")
    	.append("rect")
    	.attr("width", w)
    	.attr("height", h);

    var line = d3.svg.line()
	.x(function(d, i) { return x(i); })
	.y(function(d, i) { return y(d); });


    // var line = d3.svg.line()
    // 	.interpolate("basis")
    // 	.x(function(d) { return x(d.t); })
    // 	.y(function(d) { return y(d.value); });

    var path = netgraph.append("g")
	.attr("clip-path", "url(#clip)")
	.append("path")
	.datum(netdata)
	.attr("class", "line")
	.attr("d", line);

    var pathup = netgraph.append("g")
	.attr("clip-path", "url(#clip)")
	.append("path")
	.datum(netdataup)
	.attr("class", "line")
	.style("stroke", "red")
	.attr("d", line);


    tick();

    function tick() {
	// redraw the line, and slide it to the left
	y.domain([0, d3.max(netdataup.concat(netdata).concat([10]))]);
	path
	    .attr("d", line)
	    .attr("transform", null)
	    .transition()
	    .duration(0)
	    .ease("linear")
	    .attr("transform", "translate(" + x(-1) + ",0)")
	    .each("end", tick);

	pathup
	    .attr("d", line)
	    .attr("transform", null)
	    .transition()
	    .duration(0)	// TODO: Fix duration for smoother easing
	    .ease("linear")
	    .attr("transform", "translate(" + x(-1) + ",0)")
	    .each("end", tick);
	
    }

});
