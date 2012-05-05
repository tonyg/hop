function nodes_main() {
    var known_nodes_by_class = {};

    function category_column_template(class_name, n) {
	return '<div class="span3"><ul id="nodes'+class_name+'_'+n+'"></ul></div>';
    }

    function category_template(class_name) {
	return '<div id="node_class_'+class_name+'"><h3>Class <tt>'+class_name+'</tt></h3>' +
	    '<div class="row truncate-overflow">' +
	    category_column_template(class_name, 0) +
	    category_column_template(class_name, 1) +
	    category_column_template(class_name, 2) +
	    category_column_template(class_name, 3) +
	    '</div></div>';
    }

    function refresh_node_class(class_name) {
	var name_set = known_nodes_by_class[class_name] || {};
	var names = [];
	$.each(name_set, function (name) { names.push(name); });
	names.sort();

	var column_count = 4; /* change to match category_template above */
	var per_column = Math.ceil(names.length / column_count);
	var column_index, column;
	function set_column(i) {
	    column_index = i;
	    column = $("#nodes" + class_name + '_' + i);
	    column.children().remove();
	}

	if (!($("#node_class_"+class_name)[0])) {
	    $("#all_node_classes").append($(category_template(class_name)));
	} else {
	    for (var i = 0; i < column_count; i++) {
		set_column(i); // clears out old column contents
	    }
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
    }

    function refresh_node_list() {
	$.getJSON("/_/nodes", function (data) {
	    $("#all_node_classes").children().remove();

	    known_nodes_by_class = {};
	    $.each(data, function (class_name, names) {
		var s = {};
		$.each(names, function (i, name) { s[name] = true; });
		known_nodes_by_class[class_name] = s;
	    });
	    $.each(known_nodes_by_class, refresh_node_class);
	});
    }

    function on_node_added(node_name, node_class) {
	if (!(node_class in known_nodes_by_class)) {
	    known_nodes_by_class[node_class] = {};
	}
	known_nodes_by_class[node_class][node_name] = true;
	refresh_node_class(node_class);
    }

    function on_node_removed(node_name, node_class) {
	if (!(node_class in known_nodes_by_class)) {
	    return;
	}
	delete known_nodes_by_class[node_class][node_name];
	refresh_node_class(node_class);
    }

    function on_message(event) {
	var body = event.data;
	switch (body[0]) {
	case "post":
	    switch (body[1]) {
	    case "log_messages": {
		switch (body[2][0]) {
		case "Node bound":
		    on_node_added(body[2][1], body[2][2]);
		    break;
		case "Node unbound":
		    on_node_removed(body[2][1], body[2][2]);
		    break;
		default: break;
		}
	    }
	    default: break;
	    }
	    break;
	default: break;
	}
    }

    Hop.$open_hooks.push(refresh_node_list);
    Hop.$open_hooks.push(function () {
	Hop.create("fanout", ["system.log"], "");
	Hop.subscribe("system.log", "", "log_messages", "");
    });
    Hop.$message_hooks.push(on_message);
}
