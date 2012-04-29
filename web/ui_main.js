var $tap;

function server_disconnected() {
    $("#server_ok")[0].className = "server_not_ok";
    $("#server_stats_connection_count").text("—");
    $("#server_stats_boot_time").text("—");
    $("#server_stats_uptime").text("— seconds");
}

function refresh_server_stats() {
    $.getJSON("/_/server_stats", function (data) {
	$("#server_ok")[0].className = "server_ok";
	$("#server_stats_connection_count").text(data.connection_count);
	$("#server_stats_boot_time").text(new Date(data.boot_time * 1000));
	$("#server_stats_uptime").text(data.uptime + " seconds");
	switch ($tap.readyState) {
	case 0: // connecting
	case 1: // open
	case 2: // closing
            break;
	case 3: // closed
	    reset_tap_stream();
	}
    }).error(server_disconnected);
}

function reset_tap_stream() {
    $tap = $.stream("/_/tap", {
        type: "http",
        dataType: "json",

        open: function () {
	    refresh_server_stats();
        },
        message: function (event, stream) {
	    $("#debug_container").text(JSON.stringify(event.data));
	    stream.send({ok: true});
        },
        error: server_disconnected,
        close: server_disconnected
    });
}

function ui_main() {
    refresh_server_stats();
    setInterval(refresh_server_stats, 5000);

    $.stream.setup({enableXDR: true});
    reset_tap_stream();
}
