function ui_main() {
    var uptime = -1;
    var refresh_pending = false;

    function server_disconnected() {
	$("#server_stats_connection_count").text("—");
	$("#server_stats_boot_time").text("—");
	set_uptime(-1);
	$("#server_classes").text("—");
    }

    function set_uptime(new_uptime) {
	uptime = new_uptime;
	if (uptime == -1) {
	    $("#server_stats_uptime").text("—");
	} else {
	    $("#server_stats_uptime").text(uptime);
	}
    }

    function refresh_server_stats() {
	$.getJSON("/_/server_stats", function (data) {
	    $("#server_stats_connection_count").text(data.connection_count);
	    $("#server_stats_boot_time").text(new Date(data.boot_time * 1000));
	    set_uptime(data.uptime);
	    $("#server_classes").text(data.classes.join(", "));
	}).error(server_disconnected);
	refresh_pending = false;
    }

    function bump_uptime() {
	if (uptime != -1) {
	    set_uptime(uptime + 1);
	}
    }

    setInterval(bump_uptime, 1000);

    Hop.$open_hooks.push(function (event, stream) {
	refresh_server_stats();
	Hop.post(stream.id, {"test":true});
	Hop.create("fanout", ["system.log"], "completion1");
	Hop.subscribe("meta", "system.log", "sub_messages", "completion2");
	Hop.subscribe("system.log", "", "log_messages", "completion3");
    });
    Hop.$close_hooks.push(server_disconnected);
    Hop.$message_hooks.push(function (event, stream) {
	if (!refresh_pending) {
	    refresh_pending = true;
	    setTimeout(refresh_server_stats, 1000);
	}
	$("#debug_container").append(JSON.stringify(event.data) + "\n");
    });
}
