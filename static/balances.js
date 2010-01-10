function make_balances() {
    document.write(hidable("Balances", "", "balances"));
}

function refresh_balances(known_users) {
    elem = $("#balances");
    contents = "<table>";
    for (u in known_users) {
	contents += "<tr>";
	contents += "<td>" + known_users[u].uname + "</td>";
	contents += "<td>" + known_users[u].balance + "</td>";
	contents += "<td><a href=\"statements.html?show_user=" + known_users[u].uname + "&cookie=" + cookie + "&uname=" + uname + "&is_admin=" + is_admin + "\">Statements</a></td>";
	contents += "</tr>";
    }
    contents += "</table>";
    elem.html(contents);
}
register_user_listener(refresh_balances);

