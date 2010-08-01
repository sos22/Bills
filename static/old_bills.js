function make_old_bills() {
    document.write(hidable("Old bills", "", "old_bills"));
}

_highlighted_bill = null;
function set_bill_highlight(new_highlight) {
    var o = $("#old_bill_" + _highlighted_bill);
    if (o != null) {
	o.removeClass("highlighted_bill");
    }
    var n = $("#old_bill_" + new_highlight);
    if (n != null) {
	n.addClass("highlighted_bill");
    }
    _highlighted_bill = new_highlight;
}


function refresh_old_bills() {
    var new_contents = "<table border=1 cellspacing=0>";
    new_contents += "<h3><thead><tr><td>Date</td><td>Last changed</td><td>Description</td><td>Created by</td><td>Charged user</td><td>Charge amount</td></tr></thead></h3>";
  for (var b in old_bills) {
    var bi = old_bills[b];
    owned_by_me = bi.owner.toLowerCase() == uname.toLowerCase();
    new_contents += "<tbody id=\"old_bill_" + bi.ident + "\">";
    function ident(c) {
      return "old_bill_" + bi.ident + "_" + c;
    }
    function show_charge(c) {
      return "<td id=\"" + ident("uname") +"_" + c.ident + "\" class=\"old_bill_uname\">" + c.uname + "</td><td id=\"" + ident("charge") + "_" + c.ident + "\" class=\"old_bill_charge\">" + c.charge + "</td>";
    }
    new_contents += "<tr>";
    new_contents += "<td id=\"" + ident("date") + "\" class=\"old_bill_date\">" + bi.date + "</td>";
    new_contents += "<td id=\"" + ident("changed") + "\" class=\"old_bill_changed\">" + bi.changed + "</td>";
    new_contents += "<td id=\"" + ident("description") + "\" class=\"old_bill_description\">" + bi.description + "</td>";
    new_contents += "<td>&nbsp;" + bi.owner + "</td>";
    var c = bi.charges;
    if (c.length > 0) {
	new_contents += show_charge(c[0]);
	nr_to_span = c.length;
    } else {
	new_contents += "<td/><td/>";
	nr_to_span = 1;
    }
    if (owned_by_me) {
	new_contents += "<td rowspan=" + nr_to_span + " id=\"" + ident("edit") + "\" class=\"old_bill_edit\">" + action("Edit", "edit_old_bill(\"" + bi.ident + "\")") + "</td>";
	new_contents += "<td rowspan=" + nr_to_span + " id=\"" + ident("remove") + "\" class=\"old_bill_remove\">" + action("Remove", "remove_old_bill(\"" + bi.ident + "\")") + "</td>";
	new_contents += "<td rowspan=" + nr_to_span + " id=\"" + ident("attach") + "\">" + action("Attach file", "attach_file(\"" + bi.ident + "\")") + "</td>";
    }
    new_contents += "<td rowspan=" + nr_to_span + " id=\"" + ident("clone") + "\" class=\"old_bill_clone\">" + action("Clone", "clone_old_bill(\"" + bi.ident + "\")") + "</td></tr>";
    for (index = 1; index < c.length; index++) {
       ch = c[index];
       new_contents += "<tr><td colspan=3 />" + show_charge(ch) + "</tr>";
    }
    for (index = 0; index < bi.attachments.length; index++) {
	a = bi.attachments[index];
	new_contents += "<tr><td colspan=3 /><td colspan=2><a href=\"action/fetch_attachment?id=" + a.attach_id + "\">View attachment " + a.filename + "</a></td>";
	if (owned_by_me) {
	    new_contents +=
		"<td colspan=4>" + action("Remove attachment",
					  "remove_attachment(\"" + a.attach_id + "\")") +
		"</td>";
	}
	new_contents += "</tr>";
    }
    new_contents += "<tr><td colspan=4 id=\"" + ident("msg") + "\"></td></tr>";
    new_contents += "</tbody>";
  }
  new_contents += "</table>";
  $("#old_bills").html(new_contents);
  set_bill_highlight(_highlighted_bill);
}

function get_old_bills() {
  jQuery.getJSON("action/old_bills", {},
                 function(data) {
		   if (data.result == "okay") {
	             old_bills = data.data;
		     refresh_old_bills();
		   } else {
		     $("#old_bills").text("Error fetching old bills: " + data.error);
		   }
                 }, "json");
}
get_old_bills();

function add_new_charge_to_bill(ident) {
    div = $("#old_bill_" + ident).get(0);
    new_entry = document.createElement('tr');
    new_entry.setAttribute("id", "old_bill_" + ident + "_new_charge_tr");
    cell1 = document.createElement('td');
    cell1.setAttribute("colspan", 3);
    new_entry.appendChild(cell1);
    cell2 = document.createElement('td');
    dropbox = document.createElement('select');
    dropbox.setAttribute("id", "old_bill_" + ident + "_new_charge_user");
    for (k in _known_users) {
	i = document.createElement('option');
	i.appendChild(document.createTextNode(_known_users[k].uname));
	dropbox.appendChild(i);
    }
    i = document.createElement('option');
    i.appendChild(document.createTextNode("New charge..."));
    i.setAttribute("selected", "true");
    dropbox.appendChild(i);
    dropbox.setAttribute("onblur", "blur_oldbill_new_charge(" + ident + ")");
    cell2.appendChild(dropbox);
    new_entry.appendChild(cell2);
    cell3 = document.createElement("td");
    charge = document.createElement("input");
    charge.setAttribute("id", "old_bill_"+ ident + "_new_charge_charge");
    cell3.appendChild(charge);
    new_entry.appendChild(cell3);
    div.appendChild(new_entry);
}

function edit_old_bill(ident) {
    var items = $("*[id^='old_bill_" + ident +"']");
    for (index = 0; index < items.length; index++) {
	item = items[index];
	cl = item.getAttribute("class");
	id = item.getAttribute("id");
	val = item.innerHTML;
	if (cl == "old_bill_date") {
	    item.innerHTML = "<input class=\"date-pick\" id=\"" + id + "_edit\" + value=\"" + val + "\">";
	    $('#' + id + "_edit").datePicker({startDate: '1996-01-01'});
	} else if (cl == "old_bill_description") {
	    item.innerHTML = "<input id=\"" + id + "_edit\" value=\"" + val + "\">";
	} else if (cl == "old_bill_uname") {
	    content = "<select id=\"" + id + "_edit\">";
	    for (k in _known_users) {
		if (_known_users[k].uname == val) {
		    sel = " selected";
		} else {
		    sel = "";
		}
		content += "<option" + sel + ">" + _known_users[k].uname + "</option><br>";
	    }
	    content += "</select>";
	    item.innerHTML = content;
	} else if (cl == "old_bill_edit") {
	    item.innerHTML = action("Done", "done_old_bill(\"" + ident + "\")");
	} else if (cl == "old_bill_charge") {
	    item.innerHTML = "<input id=\"" + id + "_edit\" value=\"" + val + "\">";
	}
    }

    add_new_charge_to_bill(ident);
    set_bill_highlight(ident);
}

function done_old_bill(ident) {
    function id(l) {
	return "old_bill_" + ident + "_" + l + "_edit";
    }
    function err(msg) {
	$("#old_bill_" + ident + "_msg").text(msg);
    }
    err("Preparing bill...");
    date = $("#" + id("date")).get(0).value;
    description = $("#" + id("description")).get(0).value;
    unames = $("select[id^='old_bill_" + ident + "_uname']");
    prices = $("input[id^='old_bill_" + ident + "_charge']");
    tot_price = 0;
    charges = "[";
    function do_pair(u) {
	p = prices.get(u).value;
	if (p == "" || p == "0")
	    return "";
	else
	    return "{ \"user\": \"" + encodeURIComponent(unames.get(u).value) + "\", \"charge\": \"" + encodeURIComponent(p) + "\"}";
    }
    pairs = []
    for (u = 0; u < unames.length; u++) {
	p = do_pair(u);
	if (p != "")
	    pairs.push(p);
	tot_price += parseFloat(prices.get(u).value);
    }
    charges = "[";
    for (c = 0; c < pairs.length - 1; c++) {
	charges += pairs[c] + ", ";
    }
    if (pairs.length != 0) {
	charges += pairs[c];
    }
    charges += "]";

    if (tot_price < -0.01 || tot_price > 0.01) {
	err("Expected total to be zero; was " + tot_price);
	return;
    }
    err("Submitting new bill...");
    set_bill_highlight(ident);
    do_action("action/change_bill",
	      {"description": description,
	       "id": parseInt(ident),
	       "date": date,
	       "charges": charges },
	      function(data) {
		  if (data.result == "okay") {
		      err("Done.");
		      get_old_bills();
		      get_known_users();
		  } else {
		      err("Saving bill: " + data.error);
		  }
	      });
}

function clone_old_bill(ident) {
    do_action("action/clone_bill",
	      {"id": parseInt(ident) },
	      function(data) {
		  if (data.result == "okay") {
		      set_bill_highlight(data.data);
		      get_old_bills();
		      get_known_users();
		  }
	      });
}

function attach_file(ident) {
    var items = $("#old_bill_" + ident + "_attach");
    var item = items[0];
    item.innerHTML = "<iframe height=\"70\" frameborder=0 src=\"attach_file.html?cookie=" + cookie + "&bill=" + ident + "\">No iframe support?</iframe>";
}

function remove_old_bill(ident) {
    do_action("action/remove_bill",
	      {"id": parseInt(ident) },
	      function(data) {
		  if (data.result == "okay") {
		      get_old_bills();
		      get_known_users();
		  }
	      });
}

new_charge_ident = 1;
function blur_oldbill_new_charge(ident) {
    tr = $("#old_bill_" + ident + "_new_charge_tr").get(0);
    user = $("#old_bill_" + ident + "_new_charge_user").get(0).value;
    charge = $("#old_bill_" + ident + "_new_charge_charge").get(0).value;
    if (user == "New charge...")
	return;
    content = "<td colspan=\"3\">";
    content += "<td id=\"old_bill_" + ident + "_A" + new_charge_ident + "\" class=\"old_bill_uname\">";
    content += "<select id=\"old_bill_" + ident + "_uname_A" + new_charge_ident + "_edit\">";
    for (u in _known_users) {
	uname = _known_users[u].uname;
	if (uname == user)
	    sel = " selected";
	else
	    sel = "";
	content += "<option" + sel + ">" + uname + "</option>";
    }
    content += "</select>";
    content += "</td>";
    content += "<td id=\"old_bill_" + ident + "_charge_A" + new_charge_ident + "\">";
    content += "<input id=\"old_bill_" + ident + "_charge_A" + new_charge_ident + "_edit\" value=\"" + charge + "\">";
    content += "</td>";
    tr.removeAttribute("id");
    tr.innerHTML = content;

    new_charge_ident += 1;
    add_new_charge_to_bill(ident);
}

function remove_attachment(ident) {
    do_action("action/remove_bill_attachment",
	      {"id": parseInt(ident) },
	      function(data) {
		  if (data.result == "okay") {
		      get_old_bills();
		      get_known_users();
		  } else {
		      alert("Error removing attachment:" + data.error);
		  }
	      });
}