function make_user_management() {
    document.write("\
<a onclick=\"toggle_visible(&quot;user_management&quot;)\"> User management </a>\
<div id=\"user_management\">\
<p>\
Known users: <div id=\"known_user_list\"> Loading... </div>\
</p>\
<ul>\
  <li>\
    <a onclick=\"script:toggle_visible(&quot;div:add_user&quot;)\"> Add user </a>\
    <div id=\"add_user\">\
       <form>\
	 Username: <input type=\"text\" name=\"username\" alt=\"User name\" id=\"add_user_username\">\
         <input type=\"button\" onclick=\"add_user_submit()\" value=\"Add user\">\
	 <div id=\"add_user_error\"></div>\
       </form>\
    </div>\
  </li>\
</ul>\
</div>\
");
    $("#user_management").hide();

    register_user_listener(refresh_known_users);
}

function refresh_known_users(known_users) {
  var newContents = "<table>"
  for (var k in known_users) {
    newContents += "<tr>";
    newContents += "<td>" + known_users[k].uname + "</td>";
    newContents += "<td><a onclick=\"remove_user(&quot;" + known_users[k].uname + "&quot;)\">Remove</a></td>";
    newContents += "<td id=\"remove_user_" + known_users[k].uname + "_error\"></td>"
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
    jQuery.post("action/add_user", {"username": username},
		function(data) {
		    if (data.result == "okay") {
			err("Created a new user " + username);
			get_known_users();
		    } else {
			err("Creating a new user " + username + ": " + data.error);
		    }
		}, "json"
	);
}

function remove_user(name) {
  function err(msg) {
    n = "#remove_user_" + name + "_error";
    $(n).text(msg);
  }
  err("Removing...");
  jQuery.post("action/remove_user",
              {"username": name},
	      function(data) {
	        if (data.result == "okay") {
		   err("removed");
		   get_known_users();
		} else {
		   err("Error: " + data.error);
		}
              }, "json");
}

