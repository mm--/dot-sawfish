function updateTime() {
    $("#time").text(moment().format('MMM Do YYYY, h:mm:ss a'));
};

window.onload = function() {
    setInterval(updateTime, 500);
};
