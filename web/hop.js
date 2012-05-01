var Hop = {
    $tap: null,
    $args: null,

    $open_hooks: [],
    $message_hooks: [],
    $close_hooks: [],

    run_open_hooks: function (event, stream) {
	$.each(Hop.$open_hooks, function (i, f) { f(event, stream); });
    },

    run_message_hooks: function (event, stream) {
	$.each(Hop.$message_hooks, function (i, f) { f(event, stream); });
    },

    run_close_hooks: function (event, stream) {
	$.each(Hop.$close_hooks, function (i, f) { f(event, stream); });
    },

    _send: function (msg) {
	Hop.$tap.send({data: JSON.stringify(msg)});
    },

    _post_msg: function (target, datum, token) {
	return ["post", target, datum, token || ""];
    },

    post: function (target, datum, token) {
	Hop._send(Hop._post_msg(target, datum, token));
    },

    _subscribe_msg: function (filter, sink, name, reply_sink, reply_name) {
	return ["subscribe", filter, sink, name, reply_sink || "", reply_name || ""];
    },

    _subscribe: function (source, filter, name, reply_name) {
	return Hop._post_msg(source,
			     Hop._subscribe_msg(filter, Hop.$tap.id, name,
						reply_name ? Hop.$tap.id : "",
						reply_name));
    },

    subscribe: function (source, filter, name, reply_name) {
	Hop._send(Hop._subscribe(source, filter, name, reply_name));
    },

    _unsubscribe_msg: function (token) {
	return ["unsubscribe", token];
    },

    _unsubscribe: function (source, token) {
	return Hop._post_msg(source, Hop._unsubscribe_msg(token));
    },

    unsubscribe: function (source, token) {
	Hop._send(Hop._unsubscribe(source, token));
    },

    _create_msg: function (classname, arg, reply_sink, reply_name) {
	return ["create", classname, arg, reply_sink || "", reply_name || ""];
    },

    _create: function (classname, arg, reply_name, factory) {
	return Hop._post_msg(factory || "factory",
			     Hop._create_msg(classname, arg,
					     reply_name ? Hop.$tap.id : "",
					     reply_name));
    },

    create: function (classname, arg, reply_name, factory) {
	Hop._send(Hop._create(classname, arg, reply_name, factory));
    },

    _install_tap: function () {
	Hop.$tap = $.stream("/_/tap", {
            type: "http",
            dataType: "json",
	    enableXDR: true,

            open: Hop.run_open_hooks,
            message: Hop.run_message_hooks,
            error: Hop.run_close_hooks,
            close: Hop.run_close_hooks
	});
    },

    install_tap: function (args) {
	Hop.$args = args;
	Hop._install_tap();
	setInterval(Hop.check_connectivity, 5000);
    },

    check_connectivity: function () {
	switch (Hop.$tap.readyState) {
	case 0: // connecting
	case 1: // open
	case 2: // closing
            break;
	case 3: // closed
	    Hop._install_tap();
	}
    }
}
