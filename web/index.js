function server_disconnected() {
    $("#server_stats_connection_count").text("—");
    $("#server_stats_boot_time").text("—");
    $("#server_stats_uptime").text("—");
    $("#server_classes").text("—");
}

function refresh_server_stats() {
    $.getJSON("/_/server_stats", function (data) {
	$("#server_stats_connection_count").text(data.connection_count);
	$("#server_stats_boot_time").text(new Date(data.boot_time * 1000));
	$("#server_stats_uptime").text(data.uptime);
	$("#server_classes").text(data.classes.join(", "));
	switch (Ocamlmsg.$tap.readyState) {
	case 0: // connecting
	case 1: // open
	case 2: // closing
            break;
	case 3: // closed
	    Ocamlmsg.force_reinstall();
	}
    }).error(server_disconnected);
}

function ui_main() {
    refresh_server_stats();
    setInterval(refresh_server_stats, 5000);

    Ocamlmsg.$open_hooks.push(function (event, stream) {
	refresh_server_stats();
	Ocamlmsg.post(stream.id, {"test":true});
	Ocamlmsg.create("fanout", ["system.log"], "completion1");
	Ocamlmsg.subscribe("meta", "system.log", "sub_messages", "completion2");
	Ocamlmsg.subscribe("system.log", "", "log_messages", "completion3");
    });
    Ocamlmsg.$close_hooks.push(server_disconnected);
    Ocamlmsg.install_tap({
        message: function (event, stream) {
	    $("#debug_container").append(JSON.stringify(event.data) + "\n");
        },
    });
}
