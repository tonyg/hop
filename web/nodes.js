function refresh_node_list() {
    $.getJSON("/_/nodes", function (data) {
	var names = data.nodes;
	names.push("bar");
	names.push("foo");
	names.push("qux");
	names.sort();
	var column_count = 4; /* change to match nodes.xml */
	var per_column = Math.ceil(names.length / column_count);
	var column_index, column;
	function set_column(i) {
	    column_index = i;
	    column = $("#nodes" + i);
	    column.html("");
	}
	set_column(0);
	for (var i = 0; i < names.length; i++) {
	    if (i >= (column_index + 1) * per_column) {
		set_column(column_index + 1);
	    }
	    var link = $("<a></a>");
	    link.text(names[i]);
	    link.attr("href", "/_/node/" + names[i]);
	    var li = $("<li></li>");
	    li.append(link);
	    column.append(li);
	}
    });
}

function nodes_main() {
    Ocamlmsg.install_tap({
	open: function (event, stream) {
	    refresh_node_list();
        }
    });
}
