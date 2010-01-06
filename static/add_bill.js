function make_add_bill() {
    document.write("\
<a onclick=\"toggle_visible(&quot;bills&quot;)\"> Add bill </a>\
<div class=\"visible\" id=\"bills\">\
<form id=\"add_bill_form\">\
<ul>\
  <li> Description: <input type=\"text\" id=\"add_bill_description\"> </li>\
  <li> Amount: <input type=\"text\" id=\"add_bill_amount\"> </li>\
  <li> Date: <input id=\"add_bill_date\" class=\"date-pick\"/> </li>\
\
  <li> Who pays:<br>\
    <input type=\"radio\" id=\"add_bill_who_everyone\" name=\"add_bill_who\" onclick=\"add_bill_who_everyone_sel()\" checked value=\"everyone\"> Everyone equally<br>\
    <input type=\"radio\" id=\"add_bill_who_eqlist\" name=\"add_bill_who\" onclick=\"add_bill_who_eqlist_sel()\" value=\"eqlist\"> Split evenly between some people<br>\
    <div id=\"add_bill_who_eqlist_split\"></div>\
    <input type=\"radio\" id=\"add_bill_who_custom\" name=\"add_bill_who\" onclick=\"add_bill_who_custom_sel()\" value=\"custom\"> Manually specify split<br>\
    <div id=\"add_bill_who_custom_split\"></div></li>\
\
  <li> Who gets reimbursed:<br>\
    <input type=\"radio\" id=\"add_bill_whom_one\" name=\"add_bill_whom\" checked onclick=\"add_bill_whom_one_sel()\" value=\"one\"> One person <br>\
    <div id=\"add_bill_whom_one_split\"></div>\
    <input type=\"radio\" id=\"add_bill_whom_custom\" name=\"add_bill_whom\" onclick=\"add_bill_whom_custom_sel()\" value=\"custom\"> Manually specify split<br>\
    <div id=\"add_bill_whom_custom_split\"></div>\
  </li>\
</ul>\
<input type=\"button\" onclick=\"add_bill_submit()\" value=\"Add bill\">\
<div id=\"add_bill_error\"></div>\
</form>\
</div>\
");
    $("#add_bill_who_eqlist_split").hide();
    $("#add_bill_who_custom_split").hide();
    $("#add_bill_whom_custom_split").hide();

    register_user_listener(refresh_bill_split);
}

function add_bill_submit() {
  var description = $("#add_bill_description").get(0).value;
  var amount = $("#add_bill_amount").get(0).value;
  var date = $("#add_bill_date").get(0).value;
  var who_pays = $("input[name='add_bill_who']:checked").val();
  var pay_whom = $("input[name='add_bill_whom']:checked").val();

  var eqlist_users;
  var to_pay;

  function err(msg) {
    $("#add_bill_error").text(msg);
  }
  err("");

  amount = parseFloat(amount);
  if (isNaN(amount) || amount < 0 || !isFinite(amount)) {
    err("Don't understand the given amount");
    return;
  }
  if (who_pays == "eqlist") {
    eqlist_users = []
    var q = $("input[class='add_bill_split_eq_item']:checked").get();
    for (k in q)
      eqlist_users.push(q[k].value);
    if (eqlist_users == []) {
      err("Need to select at least one user to split between");
      return;
    }
  }
  if (who_pays == "everyone") {
    if (typeof(known_users) == "undefined") {
      err("still waiting for list of users...");
      return;
    }
    who_pays = "eqlist";
    eqlist_users = []
    for (k in _known_users)
      eqlist_users.push([_known_users[k].uname]);
  }
  to_pay = []
  if (who_pays == "custom") {
    var q = $("input[class='add_bill_split_custom_item']").get();
    var tot = 0;
    for (k in q) {
      k2 = q[k];
      uname = k2.id.substring(22);
      cost = k2.value;
      if (cost == "")
        continue;
      cost2 = parseFloat(cost);
      if (isNaN(cost2) || cost2 < 0 || !isFinite(cost2)) {
        err("Don't understand " + cost + " as a cost");
	return;
      }
      tot += cost2;
      to_pay.push( {"user": uname, "charge": cost2 });
    }
    if (tot <= amount - 0.01 || tot >= amount + 0.01) {
      err("Costs don't add up (got " + tot + ", expected " + amount + ")");
      return;
    }
  }
  if (who_pays == "eqlist") {
    var amt_to_pay = amount / eqlist_users.length;
    for (u in eqlist_users) {
      to_pay.push( {"user": eqlist_users[u],
                    "charge": amt_to_pay} );
    }
    who_pays = "custom";
  }

  if (who_pays != "custom") {
    err("got very confused...");
    return;
  }

  var to_receive;
  if (pay_whom == "one") {
    var uname = $("input[name='add_bill_split_whom_one']:checked").val();
    to_receive = [ {"user": uname, "charge": amount } ]
  } else {
    var q = $("input[class='add_bill_split_whom_custom_item']").get();    
    var tot = 0;
    to_receive = []
    for (k in q) {
      var k2 = q[k];
      uname = k2.id.substring(27);
      cost = k2.value;
      if (cost == "")
        continue;
      cost2 = parseFloat(cost);
      if (isNaN(cost2) || cost2 < 0 || !isFinite(cost2)) {
        err("Don't understand " + cost + " as a receipt");
	return;
      }
      tot += cost2;
      to_receive.push( {"user": uname, "charge": cost2 });
    }
    if (tot <= amount - 0.01 || tot >= amount + 0.01) {
      err("Receipts don't add up (got " + tot + ", expected " + amount + ")");
      return;
    }  
  }

  function encode_json(what) {
    function encode_pair(w) {
      return "{ \"user\": \"" + encodeURIComponent(w["user"]) + "\", \"charge\": \"" + encodeURIComponent(w["charge"]) + "\"}";
    }
    var r = "[";
    for (i = 0; i < what.length - 1; i++) {
      r += encode_pair(what[i]);
      r += ",";
    }
    r += encode_pair(what[i]);
    r += "]";
    return r;
  }

  to_pay = encode_json(to_pay);
  to_receive = encode_json(to_receive);

  err("Adding bill");
  do_action("action/add_bill",
            {"description": description,
	     "date": date,
	     "to_pay": to_pay,
	     "to_receive": to_receive},
	    function(data) {
	        if (data.result == "okay") {
		    err("Added bill " + description);
		    reset_add_bill();
		    get_old_bills();
		    get_known_users();
		} else {
		    err("Adding bill " + description + ": " + data.error);
		}
	    });
}

function refresh_bill_split(known_users) {
  var newContents = "<ul>";
  for (var k in known_users) {
    var uname = known_users[k].uname;
    newContents += "<li><input type=\"checkbox\" class=\"add_bill_split_eq_item\" id=\"add_bill_split_eq_" + uname + "\" value=\"" + uname + "\">" + uname + "</li>";
  }
  newContents += "</ul>";
  $("#add_bill_who_eqlist_split").html(newContents);

  newContents = "<ul>";
  for (var k in known_users) {
    var uname = known_users[k].uname;
    newContents += "<li>" + uname + "<input type=\"text\" id=\"add_bill_split_custom_" + uname + "\" class=\"add_bill_split_custom_item\"></li>";
  }
  newContents += "</ul>";
  $("#add_bill_who_custom_split").html(newContents);

  newContents = "<ul>";
  for (var k in known_users) {
    var uname = known_users[k].uname;
    newContents += "<li><input type=\"radio\" name=\"add_bill_split_whom_one\" id=\"add_bill_split_whom_one_" + uname + "\" value=\"" + uname + "\">" + uname + "</li>";
  }
  newContents += "</ul>";
  $("#add_bill_whom_one_split").html(newContents);

  newContents = "<ul>";
  for (var k in known_users) {
    var uname = known_users[k].uname;
    newContents += "<li>" + uname + "<input type=\"text\" id=\"add_bill_split_whom_custom_" + uname + "\" class=\"add_bill_split_whom_custom_item\"></li>";
  }
  newContents += "</ul>";
  $("#add_bill_whom_custom_split").html(newContents);
}

function add_bill_who_eqlist_sel() {
$("#add_bill_who_eqlist_split").show();
$("#add_bill_who_custom_split").hide();
};
function add_bill_who_everyone_sel() {
$("#add_bill_who_eqlist_split").hide();
$("#add_bill_who_custom_split").hide();
};
function add_bill_who_custom_sel() {
$("#add_bill_who_eqlist_split").hide();
$("#add_bill_who_custom_split").show();
};

function add_bill_whom_one_sel() {
$("#add_bill_whom_one_split").show();
$("#add_bill_whom_custom_split").hide();
};
function add_bill_whom_custom_sel() {
$("#add_bill_whom_one_split").hide();
$("#add_bill_whom_custom_split").show();
};
function reset_add_bill() {
$("#add_bill_form")[0].reset();
};
