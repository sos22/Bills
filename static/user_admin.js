function make_user_management() {
    document.write(
	hidable("User management",
		"\
<p>\
Known users: <div id=\"known_user_list\"> Loading... </div>\
</p>\
<div id=\"add_user\">\
   <form>\
     Username: <input type=\"text\" name=\"username\" alt=\"User name\" id=\"add_user_username\">\
     <input type=\"button\" onclick=\"add_user_submit()\" value=\"Add user\">\
     <div id=\"add_user_error\"></div>\
   </form>\
</div>\
"));
    $("#user_management").hide();

    register_user_listener(refresh_known_users);
}

function refresh_known_users(known_users) {
    var newContents = "<table>";
    for (var k in known_users) {
	newContents +="<tr>";
	newContents += td(known_users[k].uname);
	newContents += td(action("Remove", "remove_user(\"" + known_users[k].uname + "\")"));
	newContents +=
	    td("<input type=\"password\" name=\"password\" id=\"change_password_new_" + known_users[k].uname + "\" />" +
	       action("Change password", "change_password(\"" + known_users[k].uname + "\")"));
	newContents += td(action("Grant admin access", "grant_admin(\"" + known_users[k].uname + "\")"));
	newContents += td(action("Revoke admin access", "revoke_admin(\"" + known_users[k].uname + "\")"));
	newContents += "<td id=\"change_user_" + known_users[k].uname + "_error\"></td>";
	newContents += "</tr>";
    }
    newContents += "</table>";
    $("#known_user_list").html(newContents);
}

function add_user_submit() {
    username=$("#add_user_username").get(0).value;
    function err (msg) {
	$("#add_user_error").text(msg);
    };
    if (username == "") {
	err("Need a username for the new user");
	return;
    }
    err("Creating a new user " + username);
    do_action("action/add_user", {"username": username},
	      function(data) {
		  if (data.result == "okay") {
		      err("Created a new user " + username);
		      get_known_users();
		  } else {
		      err("Creating a new user " + username + ": " + data.error);
		  }
	      });
}

function user_error(uname, msg) {
    n = "#change_user_" + uname + "_error";
    $(n).text(msg);
}

function remove_user(name) {
  function err(msg) {
      user_error(name, msg);
  }
  err("Removing...");
  do_action("action/remove_user",
	    {"username": name},
	    function(data) {
	        if (data.result == "okay") {
		    err("removed");
		    get_known_users();
		} else {
		    err("Error: " + data.error);
		}
	    });
}

function change_password(uname) {
    new_pass = $("#change_password_new_" + uname).get(0).value;
    do_action("action/change_password",
	      {"username": uname,
	       "password": new_pass },
	      function(data) {
		  if (data.result == "okay") {
		      user_error(uname, "changed password");
		  } else {
		      user_error(uname, "Error: " + data.error);
		  }
	      });
}

function grant_admin(uname) {
    do_action("action/set_admin",
	      {"username": uname,
	       "is_admin": "1" },
	      function(data) {
		  if (data.result == "okay") {
		      user_error(uname, "gave admin privileges");
		  } else {
		      user_error(uname, "Error: " + data.error);
		  }
	      });
}

function revoke_admin(uname) {
    do_action("action/set_admin",
	      {"username": uname,
	       "is_admin": "0" },
	      function(data) {
		  if (data.result == "okay") {
		      user_error(uname, "revoked admin privileges");
		  } else {
		      user_error(uname, "Error: " + data.error);
		  }
	      });
}
