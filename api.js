String.prototype.replaceAll = function (search, replacement) {
    var target = this;
    var esc = escapeRegExp(search);// search.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
    var reg = new RegExp(esc, 'ig');
    return this.replace(reg, replacement);
};

function isUndefined(variable) {
	if(typeof variable == "number" && isNaN(variable)){return true;}
    return typeof variable === 'undefined' || variable == null;
}

function isArray(variable) {
    return Array.isArray(variable);
}

var escapeRegExp = function(strToEscape) {
	// Escape special characters for use in a regular expression
	return strToEscape.replace(/[\-\[\]\/\{\}\(\)\*\+\?\.\\\^\$\|]/g, "\\$&");
};

var anames = document.querySelectorAll(".header");
if(anames.length > 0){
  document.getElementById('body').insertAdjacentHTML('afterbegin', '<H1>Table of Contents</H1><UL ID="tos"></UL>');
  var tos = document.getElementById("tos");
  for(var i = 0; i < anames.length; i++){
    var element = anames[i];
    var name = element.textContent.toLowerCase().trim().replaceAll(" ", "-");
    element.innerHTML += '<A NAME="' + name + '">';
    tos.innerHTML += '<LI><A HREF="#' + name + '">' + element.textContent + '</A></LI>';
  }
}

function enum_consoles(){
    var ret = [];
    for(var i = 0; i < controllers.length; i++){
		if(controllers[i].hasOwnProperty("systems")){
			var systems = controllers[i].systems.split("/");
			for(var z = 0; z < systems.length; z++){
				if(ret.indexOf(systems[z]) == -1){
				ret.push(systems[z]);
			  }
			}
		}
    }
    return ret;
}

function enum_controllers_by_console(consoles = false, include = true){
	var ret = [];
	if(!Array.isArray(consoles)){
		consoles = consoles.split(",");
	}
	for(var i = 0; i < controllers.length; i++){
		if(controllers[i].hasOwnProperty("systems") && Array.isArray(consoles)){
			var systems = controllers[i].systems.split("/");
			var included = false;
			for(var z = 0; z < consoles.length; z++){
				var index = systems.indexOf(consoles[z]);
				if(index > -1){
					included = true;
					break;
				}
			}
			if(included == include){
				ret.push(controllers[i]);
			}
		} else if(consoles === false && include) {
			ret.push(controllers[i]);
		}
	}
	return ret;
}

function make_controller(controller = false, stat = false, name = false){
	var HTML = "";
	if(stat !== false){
		if(controller.hasOwnProperty(stat)){
			if(name){
				HTML = "<B>" + name + "</B>: "
				switch(stat){
					case "cost": HTML += "~$"; break;
				}
				HTML += controller[stat] + "<BR>";
			} else {
				HTML = nl2br(controller[stat]);
			}
		}
	} else if(controller === false){
		for(var i = 0; i < controllers.length; i++){
			HTML += make_controller(controllers[i]);
		}
	} else {
		var style = "normal";
		if(controller.hasOwnProperty("style")){
			style = controller.style;
		}
		switch(style){
			case "normal":
				HTML = '<TABLE CLASS="table"><TBODY><TR><TH COLSPAN="2" CLASS="header">' + controller.peripheralName + '</TH></TR><TR><TD ROWSPAN="2" CLASS="image">';
				HTML += 'INSERT IMAGE';
				HTML += '</TD><TD>';
				HTML += make_controller(controller, "peripheral", 	"Peripheral ID");
				HTML += make_controller(controller, "games", 		"Games Supported");
				HTML += make_controller(controller, "cost", 		"Can be found for");
				HTML += make_controller(controller, "company", 		"Made by");
				HTML += make_controller(controller, "specificVersion", 	"Specific Version");
				HTML += make_controller(controller, "otherVersions", 	"Other Version(s)");
				HTML += '</TD></TR><TR><TD>';
				HTML += make_controller(controller, "description");
				HTML += '</TD></TR></TBODY></TABLE>';
				break;
		}
	}
	return HTML;
}

function nl2br(str) {
    return (str + '').replace(/([^>\r\n]?)(\r\n|\n\r|\r|\n)/g, '$1<BR>$2');
}
