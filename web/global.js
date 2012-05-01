(function () {
    function server_disconnected() {
	$("#server_status_message_container")[0].className = "btn-danger";
	$("#server_status_message").text("Disconnected");
    }

    function server_connected() {
	$("#server_status_message_container")[0].className = "";
	$("#server_status_message").text("Connected");
    }

    Hop.$open_hooks.push(server_connected);
    Hop.$close_hooks.push(server_disconnected);
})();
