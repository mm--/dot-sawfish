function updateTime() {
    $("#time").text(moment().format('ddd MMM Do YYYY, HH:mm:ss'));
};

var taskName = "Nothing";
var taskTime = moment();

//A fake time when the computer "started". Calculated by taking the
//moment now minus the number of seconds the computer has been used
//for
var computerVirtualStartTime = moment();
var pianobar = false;

function secondsToString(totalSec, showSeconds) {
    var hours = parseInt( totalSec / 3600 ) % 24;
    var minutes = parseInt( totalSec / 60 ) % 60;
    var seconds = totalSec % 60;

    var outFormat = (hours < 10 ? "0" + hours : hours) + ":" + 
	    (minutes < 10 ? "0" + minutes : minutes);
    if(showSeconds)
	outFormat += ":" + (seconds  < 10 ? "0" + seconds : seconds);
    return outFormat;
}

function updateTask() {
    var outFormat = secondsToString(parseInt(moment.duration(moment() - taskTime).asSeconds()), true);
    $("#task2").text("[" + taskName + " " + outFormat + "]");
};

function updateComputerTime() {
    var outFormat = secondsToString(parseInt(moment.duration(moment() - computerVirtualStartTime).asSeconds()), false);
    $("#computertime").text("(CT: " + outFormat + ")");
};

function callUpdate() {
    external.updateCallback(doUpdate);
};

function doUpdate(obj) {
    // $("#stuff").text(JSON.stringify(obj));
    $("#cpu").text(obj["CPU"]);
    coreSet(parseFloat(obj["CPU1"]),
	    parseFloat(obj["CPU2"]),
	    parseFloat(obj["CPU3"]),
	    parseFloat(obj["CPU4"]));
    $("#cpu").css('color', heatmapColour(parseFloat(obj["CPU"])/100));
    $("#cputop").text(obj["CPUTOP"]);
    $("#ram").text(obj["RAM"]);
    $("#vol").text(obj["VOL"]);
    $("#bat").text(obj["BAT"]);
    var batweight = obj["BATSTATUS"] == "Discharging" ? "bold" : "normal";
    $("#bat").css("font-weight", batweight);
    $("#bat").css('color', heatmapColour(1-(parseFloat(obj["BAT"])/100)));
    $("#temp0").text(obj["TEMP0"]);
    $("#temp1").text(obj["TEMP1"]);
    $("#wifi").text(obj["WIFI"]);
    $("#wifiup").text(obj["WIFIUP"]);
    $("#wifidown").text(obj["WIFIDOWN"]);
    $("#connections").text(obj["CONNECTIONS"]);
    $("#rcirc").text(obj["RCIRC"]);
    var numupdates = parseInt(obj["NUMUPDATES"]);
    $("#numupdates").text(numupdates);
    if(numupdates == 0)
	$("#numupdates").fadeOut();
    else
	$("#numupdates").fadeIn();
    netUpdate(parseFloat(obj["WIFIDOWN"]), parseFloat(obj["WIFIUP"]));
    pianobar = (obj["PIANOBAR"] == "True");
    if(!pianobar || (obj["MPDSTAT"] == "Playing") ) {
	$("#mpdalbum").text(obj["MPDALBUM"]);
	$("#mpdartist").text(obj["MPDARTIST"]);
	$("#mpdtitle").text(obj["MPDTITLE"]);
    } else {
	$("#mpdalbum").text(obj["PIANOALBUM"]);
	$("#mpdartist").text(obj["PIANOARTIST"]);
	$("#mpdtitle").text(obj["PIANOTITLE"]);
    }
    if(pianobar)
	$("#mpdprog").fadeOut();
    else
	$("#mpdprog").fadeIn();
    if(!pianobar && obj["MPDSTAT"] == "Stopped") {
    	$("#mpdcontainer").fadeOut();
    } else {
    	$("#mpdcontainer").fadeIn();
    }
    var elapsedadd = obj["MPDELAPSED"].match(".*:.*:.*") ? "" : "00:";
    var lengthadd = obj["MPDLENGTH"].match(".*:.*:.*") ? "" : "00:";
    var elapsed = moment.duration(elapsedadd + obj["MPDELAPSED"]).asMilliseconds();
    var length = moment.duration(lengthadd + obj["MPDLENGTH"]).asMilliseconds();
    updateProg(elapsed, length, obj["MPDSTAT"]);
    computerVirtualStartTime = moment(obj["COMPUTERVIRTUALSTART"], 'X');
    taskTime = moment(obj["TASKTIME"], "X");
    taskName = obj["TASKNAME"];
};

function logAlert(line) {
    $("#logalert").text(line);
    $("#logalert").fadeIn();
    $("#logalert").delay(10000).fadeOut();
}

function deskSet(desk) {
    $("#desk").text(desk);
}

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
    setInterval(updateTask, 1000);
    setInterval(updateComputerTime, 1000);

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
    if ((cpu1 > 80) ||
	(cpu2 > 80) ||
	(cpu3 > 80) ||
	(cpu4 > 80))
	$("#cputop").fadeIn();
    else
	$("#cputop").fadeOut();
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
	.rangeRound([1, h]);

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

// MPD progress
var mpdprog;
var updateProg;
window.addEventListener('load', function() {
    var w = 14,
	h = 14,
	tau = 2 * Math.PI;

    var arc = d3.svg.arc()
	.innerRadius(4)
	.outerRadius(7)
	.startAngle(0);

    // Actually just a circle.
    var iarc = d3.svg.arc()
	.innerRadius(0)
	.outerRadius(2)
	.startAngle(0);

    var x = d3.scale.linear()
	.domain([0, 1])
	.range([0, w]);

    var y = d3.scale.linear()
	.domain([0, 100])
	.rangeRound([1, h]);

    mpdprog = d3.select("#mpdprog").append("svg")
	.attr("class", "chart")
	.attr("width", w)
	.attr("height", h)
    	.append("g")
	.attr("transform", "translate(" + w / 2 + "," + h / 2 + ")");

    var background = mpdprog.append("path")
	.datum({endAngle: tau})
	.style("fill", "#ddd")
	.attr("d", arc);

    // Add the foreground arc in orange, currently showing 12.7%.
    var foreground = mpdprog.append("path")
	.datum({endAngle: .127 * tau})
	.style("fill", "blue")
	.attr("d", arc);

    var innerarc = mpdprog.append("circle")
	.style("fill", "#ddd");

    updateProg = function(elapsed, length, status) {
	if(elapsed > 0) {
	var amount = elapsed/length;
	foreground.datum({endAngle: amount * tau});
	foreground.transition()
	    .duration(length - elapsed)
		.call(arcTween, 1 * tau);
	}
	var newcolor = (status == "Playing") ? "black" : "#ddd";
	var newrad = (status == "Stopped") ? 8 : 3;
	innerarc.transition()
	    .duration(1000)
	    .attr("r", newrad)
	    .style("fill", newcolor);
    };

    function arcTween(transition, newAngle) {
	transition.attrTween("d", function(d) {
	    var interpolate = d3.interpolate(d.endAngle, newAngle);
	    return function(t) {
		d.endAngle = interpolate(t);
		return arc(d);
	    };
	});
    }
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
