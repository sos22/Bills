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
