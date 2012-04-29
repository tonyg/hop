var $tap;

function server_disconnected() {
    $("#server_ok")[0].className = "btn-danger";
    $("#server_ok").text("Disconnected");
    $("#server_stats_connection_count").text("—");
    $("#server_stats_boot_time").text("—");
    $("#server_stats_uptime").text("— seconds");
}

function refresh_server_stats() {
    $.getJSON("/_/server_stats", function (data) {
	$("#server_ok")[0].className = "btn-success";
	$("#server_ok").text("OK");
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

function refresh_all_classes() {
    $.getJSON("/_/all_classes", function (data) {
	$("#debug_container").append(JSON.stringify(data));
    });
}

var Ocamlmsg = {
    _send: function (msg) {
	$tap.send({data: JSON.stringify(msg)});
    },

    _post_msg: function (target, datum, token) {
	return ["post", target, datum, token || ""];
    },

    post: function (target, datum, token) {
	Ocamlmsg._send(Ocamlmsg._post_msg(target, datum, token));
    },

    _subscribe_msg: function (filter, sink, name, reply_sink, reply_name) {
	return ["subscribe", filter, sink, name, reply_sink || "", reply_name || ""];
    },

    _subscribe: function (source, filter, name, reply_name) {
	return Ocamlmsg._post_msg(source,
				  Ocamlmsg._subscribe_msg(filter, $tap.id, name,
							  reply_name ? $tap.id : "",
							  reply_name));
    },

    subscribe: function (source, filter, name, reply_name) {
	Ocamlmsg._send(Ocamlmsg._subscribe(source, filter, name, reply_name));
    },

    _unsubscribe_msg: function (token) {
	return ["unsubscribe", token];
    },

    _unsubscribe: function (source, token) {
	return Ocamlmsg._post_msg(source, Ocamlmsg._unsubscribe_msg(token));
    },

    unsubscribe: function (source, token) {
	Ocamlmsg._send(Ocamlmsg._unsubscribe(source, token));
    },

    _create_msg: function (classname, arg, reply_sink, reply_name) {
	return ["create", classname, arg, reply_sink || "", reply_name || ""];
    },

    _create: function (classname, arg, reply_name, factory) {
	return Ocamlmsg._post_msg(factory || "factory",
				  Ocamlmsg._create_msg(classname, arg,
						       reply_name ? $tap.id : "",
						       reply_name));
    },

    create: function (classname, arg, reply_name, factory) {
	Ocamlmsg._send(Ocamlmsg._create(classname, arg, reply_name, factory));
    }
};

function reset_tap_stream() {
    $tap = $.stream("/_/tap", {
        type: "http",
        dataType: "json",

        open: function (event, stream) {
	    refresh_server_stats();
	    refresh_all_classes();
	    Ocamlmsg.post(stream.id, {"test":true});
	    Ocamlmsg.create("fanout", ["system.log"], "completion1");
	    Ocamlmsg.subscribe("meta", "system.log", "sub_messages", "completion2");
	    Ocamlmsg.subscribe("system.log", "", "log_messages", "completion3");
        },
        message: function (event, stream) {
	    $("#debug_container").append(JSON.stringify(event.data) + "\n");
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
