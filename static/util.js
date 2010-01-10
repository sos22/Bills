function toggle_visible(name) {
  $("#" + name).toggle();
}


/* Stuff related to the known users list */
_user_listeners = [];
_known_users = [];

function register_user_listener(fn) {
    fn(_known_users);
    _user_listeners.push(fn);
}

function new_known_user_list(known_users) {
    _known_users = known_users;
    for (var i in _user_listeners) {
	_user_listeners[i](known_users);
    }
}

function get_known_users() {
  jQuery.getJSON("action/get_user_list", {},
    function(data, textStatus) {
      if (data.result == "okay") {
	new_known_user_list(data.data);
      } else {
        $("#known_user_list").text("Error " + data.error + " getting user list");
      }
    }
  );
}

get_known_users();


function do_action(action, params, continuation) {
    params["cookie"] = cookie;
    jQuery.post(action, params, continuation, "json");
}

_next_hidable_id = 0;
function hidable(label, content, id) {
    if (id == null) {
	id = "_hidable_" + _next_hidable_id;
	_next_hidable_id++;
    }
    /* WTF? If we just return this straight off, it comes out as
       undefined on Firefox 3.5, but if we assign to a local variable
       and return that then everything works. */
    var res =
	action(label, "toggle_visible(\"" + id + "\")") +
	"<div id=\"" + id + "\">" + content + "</div>";
    return res;
}

function td(s) {
    return "<td>" + s + "</td>";
}
function escape_quotes(s) {
    return s.replace(/"/g, "&quot;");
}

/* A clickable button */
function action(label, act) {
    return "<a class=\"effaceable\" onclick=\"" + escape_quotes(act) + "\">" + label + "</a>";
}
