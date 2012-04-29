function refresh_server_stats() {
    $.getJSON("/_/server_stats", function (data) {
	$("#server_ok")[0].className = "server_ok";
	$("#server_stats_connection_count").text(data.connection_count);
	$("#server_stats_boot_time").text(new Date(data.boot_time * 1000));
	$("#server_stats_uptime").text(data.uptime + " seconds");
    }).error(function () {
	$("#server_ok")[0].className = "server_not_ok";
	$("#server_stats_connection_count").text("—");
	$("#server_stats_boot_time").text("—");
	$("#server_stats_uptime").text("— seconds");
    });
}

function ui_main() {
    refresh_server_stats();
    setInterval(refresh_server_stats, 5000);
}
