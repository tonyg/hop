var Ocamlmsg = {
    $tap: null,
    $args: null,

    _send: function (msg) {
	Ocamlmsg.$tap.send({data: JSON.stringify(msg)});
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
				  Ocamlmsg._subscribe_msg(filter, Ocamlmsg.$tap.id, name,
							  reply_name ? Ocamlmsg.$tap.id : "",
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
						       reply_name ? Ocamlmsg.$tap.id : "",
						       reply_name));
    },

    create: function (classname, arg, reply_name, factory) {
	Ocamlmsg._send(Ocamlmsg._create(classname, arg, reply_name, factory));
    },

    install_tap: function (args) {
	Ocamlmsg.$args = args;
	Ocamlmsg.$tap = $.stream("/_/tap", {
            type: "http",
            dataType: "json",
	    enableXDR: true,

            open: args.open,
            message: args.message,
            error: args.close,
            close: args.close
	});
    },

    force_reinstall: function () {
	Ocamlmsg.install_tap(Ocamlmsg.$args);
    }
}
