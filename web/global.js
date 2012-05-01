(function () {
    function server_disconnected() {
	$("#server_status_message_container")[0].className = "btn-danger";
	$("#server_status_message").text("Disconnected");
    }

    function server_connected() {
	$("#server_status_message_container")[0].className = "";
	$("#server_status_message").text("Connected");
    }

    Ocamlmsg.$open_hooks.push(server_connected);
    Ocamlmsg.$close_hooks.push(server_disconnected);
})();
