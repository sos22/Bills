function make_balances() {
    document.write("\
<a onclick=\"toggle_visible(&quot;balances&quot;)\">Balances</a>	\
<div id=\"balances\"></div>\
");
}

function refresh_balances(known_users) {
    elem = $("#balances");
    contents = "<table>";
    for (u in known_users) {
	contents += "<tr>";
	contents += "<td>" + known_users[u].uname + "</td>";
	contents += "<td>" + known_users[u].balance + "</td>";
	contents += "</tr>";
    }
    contents += "</table>";
    elem.html(contents);
}
register_user_listener(refresh_balances);

