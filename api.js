var is_firefox = navigator.userAgent.toLowerCase().indexOf('firefox') > -1;
var is_android = navigator.userAgent.toLowerCase().indexOf('android') > -1;
var is_chromiumedge = window.navigator.userAgent.indexOf("Edg/") > -1;
var is_chrome = is_chromiumedge || (/Chrome/.test(navigator.userAgent) && /Google Inc/.test(navigator.vendor));
var is_edge = window.navigator.userAgent.indexOf("Edge/") > -1;
var is_firefox_for_android = is_firefox && is_android;
var is_IOS = (/iPad|iPhone|iPod/.test(navigator.platform) || (navigator.platform === 'MacIntel' && navigator.maxTouchPoints > 1)) && !window.MSStream;
var newline = "\r\n";
function isMobileOrTablet() {
	var check = false;
	(function(a){if(/(android|bb\d+|meego).+mobile|avantgo|bada\/|blackberry|blazer|compal|elaine|fennec|hiptop|iemobile|ip(hone|od)|iris|kindle|lge |maemo|midp|mmp|mobile.+firefox|netfront|opera m(ob|in)i|palm( os)?|phone|p(ixi|re)\/|plucker|pocket|psp|series(4|6)0|symbian|treo|up\.(browser|link)|vodafone|wap|windows ce|xda|xiino|android|ipad|playbook|silk/i.test(a)||/1207|6310|6590|3gso|4thp|50[1-6]i|770s|802s|a wa|abac|ac(er|oo|s\-)|ai(ko|rn)|al(av|ca|co)|amoi|an(ex|ny|yw)|aptu|ar(ch|go)|as(te|us)|attw|au(di|\-m|r |s )|avan|be(ck|ll|nq)|bi(lb|rd)|bl(ac|az)|br(e|v)w|bumb|bw\-(n|u)|c55\/|capi|ccwa|cdm\-|cell|chtm|cldc|cmd\-|co(mp|nd)|craw|da(it|ll|ng)|dbte|dc\-s|devi|dica|dmob|do(c|p)o|ds(12|\-d)|el(49|ai)|em(l2|ul)|er(ic|k0)|esl8|ez([4-7]0|os|wa|ze)|fetc|fly(\-|_)|g1 u|g560|gene|gf\-5|g\-mo|go(\.w|od)|gr(ad|un)|haie|hcit|hd\-(m|p|t)|hei\-|hi(pt|ta)|hp( i|ip)|hs\-c|ht(c(\-| |_|a|g|p|s|t)|tp)|hu(aw|tc)|i\-(20|go|ma)|i230|iac( |\-|\/)|ibro|idea|ig01|ikom|im1k|inno|ipaq|iris|ja(t|v)a|jbro|jemu|jigs|kddi|keji|kgt( |\/)|klon|kpt |kwc\-|kyo(c|k)|le(no|xi)|lg( g|\/(k|l|u)|50|54|\-[a-w])|libw|lynx|m1\-w|m3ga|m50\/|ma(te|ui|xo)|mc(01|21|ca)|m\-cr|me(rc|ri)|mi(o8|oa|ts)|mmef|mo(01|02|bi|de|do|t(\-| |o|v)|zz)|mt(50|p1|v )|mwbp|mywa|n10[0-2]|n20[2-3]|n30(0|2)|n50(0|2|5)|n7(0(0|1)|10)|ne((c|m)\-|on|tf|wf|wg|wt)|nok(6|i)|nzph|o2im|op(ti|wv)|oran|owg1|p800|pan(a|d|t)|pdxg|pg(13|\-([1-8]|c))|phil|pire|pl(ay|uc)|pn\-2|po(ck|rt|se)|prox|psio|pt\-g|qa\-a|qc(07|12|21|32|60|\-[2-7]|i\-)|qtek|r380|r600|raks|rim9|ro(ve|zo)|s55\/|sa(ge|ma|mm|ms|ny|va)|sc(01|h\-|oo|p\-)|sdk\/|se(c(\-|0|1)|47|mc|nd|ri)|sgh\-|shar|sie(\-|m)|sk\-0|sl(45|id)|sm(al|ar|b3|it|t5)|so(ft|ny)|sp(01|h\-|v\-|v )|sy(01|mb)|t2(18|50)|t6(00|10|18)|ta(gt|lk)|tcl\-|tdg\-|tel(i|m)|tim\-|t\-mo|to(pl|sh)|ts(70|m\-|m3|m5)|tx\-9|up(\.b|g1|si)|utst|v400|v750|veri|vi(rg|te)|vk(40|5[0-3]|\-v)|vm40|voda|vulc|vx(52|53|60|61|70|80|81|83|85|98)|w3c(\-| )|webc|whit|wi(g |nc|nw)|wmlb|wonu|x700|yas\-|your|zeto|zte\-/i.test(a.substr(0,4))) check = true;})(navigator.userAgent||navigator.vendor||window.opera);
	return check;
}
window.isMobileOrTablet = window.isMobileOrTablet || isMobileOrTablet;

if(is_chrome){
	var log = console.log;
}

function iif(value, iftrue, iffalse) {
    if (value) {return iftrue;}
    if (isUndefined(iffalse)) {return "";}
    return iffalse;
}

String.prototype.replaceAll = function (search, replacement) {
    var target = this;
    var esc = escapeRegExp(search);// search.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
    var reg = new RegExp(esc, 'ig');
    return this.replace(reg, replacement);
};

function dynamicSortMultiple() {
    /*
     save the arguments object as it will be overwritten
     note that arguments object is an array-like object consisting of the names of the properties to sort by
     ie: People.sort(dynamicSortMultiple("Name", "-Surname"));
     */
    var props = arguments;
    return function (obj1, obj2) {
        var i = 0, result = 0, numberOfProperties = props.length;
        /* try getting a different result from 0 (equal)
         * as long as we have extra properties to compare
         */
        while(result === 0 && i < numberOfProperties) {
            result = dynamicSort(props[i])(obj1, obj2);
            i++;
        }
        return result;
    }
}

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


function die(value = "Halted execution"){
	if(isObject(value)){
		log(value);
		value = "Halted execution";
	}
	toast("[DIE: " + value + "]");
	throw new Error(value);
}

function isAssoc(value){
	return !isUndefined(value) && isObject(value) && !isArray(value);
}

function collectdates(quantity = 5){
	var ret = [];
	for(var i = controllers.length - 1; i > 0; i--){
		var controller = controllers[i];
		if(controller.hasOwnProperty("obtained") && ret.indexOf(controller.obtained) == -1){
			ret.push(controller.obtained);
			if(ret.length == quantity){
				break;
			}
		}
	}
	return ret;
}

function enum_controllers_by_dates(dates = 5){
	var ret = {};
	if(isNumeric(dates)){
		dates = collectdates(dates);
	}
	for(var i = 0; i < dates.length; i++){
		ret[dates[i]] = [];
	}
	for(var i = controllers.length - 1; i > 0; i--){
		var controller = controllers[i];
		if(controller.hasOwnProperty("obtained") && dates.indexOf(controller.obtained) > -1){
			ret[controller.obtained].push(controller);
		}
	}
	return ret;
}

Number.prototype.isEqual = function (str) {
	return this.toString().isEqual(str);
};

String.prototype.isEqual = function (str) {
    if (isUndefined(str)) {
        return false;
    }
    if (isNumeric(str) || isNumeric(this)) {
        return parseFloat(this) == parseFloat(str);
    }
    return this.toUpperCase().trim() == str.toUpperCase().trim();
};

//returns true if $variable appears to be a valid number
function isNumeric(variable) {
	if(!isUndefined(variable)){
		if(filternonnumeric(variable.toString()).length > 0){
			return !isNaN(Number(variable));
		}
	}
	return false;
}

function daysInMonth(month,year) {
  return new Date(year, month, 0).getDate();
}

function currentdate(){
	var timestamp = Date.now();
	if( parseInt(formattednow(timestamp, "H")) < 8 ){
		timestamp -= 864e5;
	}
	return formattednow(timestamp, "Y-m-d");
}

var days_of_week = ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"];
var month_names = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
function formattednow(timestamp, formatstring, businessdate = false){
	if(formatstring === true){
		businessdate = true;
		formatstring = null;
	}
	if(isUndefined(formatstring)){
		if(!isUndefined(timestamp) && isNaN(timestamp)){
			formatstring = timestamp;
			timestamp = null;
		} else {
			formatstring = "l F j, Y @ g:i A";//Wednesday August 22, 2018 @ 1:34 PM
		}
	}
	if(isUndefined(timestamp)){
		timestamp = Date.now();
	} else if(typeof timestamp == "object"){
		if(typeof timestamp.getTime === 'undefined'){
			log("UNRECOGNIZED OBJECT (formattednow)");
			log(timestamp);
			return timestamp.toString();
		} else {
			timestamp = timestamp.getTime();
		}
	} else if(isNaN(timestamp)) {
		if(timestamp.contains(":")){
			
		} else {
			timestamp = (timestamp + 'T00:00:00').replace(/-/g, '\/').replace(/T.+/, '');
		}
		timestamp = Date.parse(timestamp);
	}
    var the_date = new Date(timestamp);
	var hours24 = the_date.getHours();//0-23
	var minutes = the_date.getMinutes();//0-59
	if(businessdate && (hours24 < 6 || (hours24 == 6 && minutes == 0))){
		the_date.setDate(the_date.getDate()-1);
	}
    var day_of_month = the_date.getDate();//1-31
    var day_of_week = the_date.getDay();//0-6
    var the_month = the_date.getMonth();//0-11
    var the_year = the_date.getFullYear();//2017
    var hours12 = hours24;
    if(hours12 == 0){hours12 = 12;} else if(hours12 > 12){hours12 = hours12 - 12;}
	var seconds = the_date.getSeconds();//0-59
    var antepost = iif(hours24 < 12, "AM", "PM");	
	var returnvalue = "";
	if(formatstring == "debug"){
		return "Year: " + the_year + " Month: " + (the_month+1) + " Day: " + day_of_month;
	}
	for(var i = 0; i < formatstring.length; i++){
		var digit = formatstring.mid(i, 1);
		switch(digit){
			case "l": returnvalue += isNaN(day_of_week) ? "Unknown" : days_of_week[day_of_week]; break; //day of week (Wednesday)
			case "D": returnvalue += isNaN(day_of_week) ? "???" : days_of_week[day_of_week].left(3); break; //day of week (Wed)
			case "w": returnvalue += day_of_week; break; //day of week (3)
			case "F": returnvalue += isNaN(the_month) ? "Unknown" : month_names[the_month]; break;//month (August)
			case "M": returnvalue += isNaN(the_month) ? "???" : month_names[the_month].left(3); break;//month (Aug)
			case "n": returnvalue += the_month+1; break;//month (8)
			case "j": returnvalue += day_of_month; break;//day of month (22)
			case "Y": returnvalue += the_year; break;//year (2018)
			case "g": returnvalue += hours12; break;//12hour (1)
			case "G": returnvalue += hours24; break;//24hour (13)
			case "P": //pizza hour
				if(hours24 <= 6){
					returnvalue += (hours24 + 24);
				} else {
					returnvalue += hours24;
				}
				break;
			case "i": returnvalue += minutes.pad(2); break;//minutes (34)
			case "s": returnvalue += seconds.pad(2); break;//seconds (00)
			case "A": returnvalue += antepost; break;//antepost (AM)
			case "a": returnvalue += antepost.toLowerCase(); break;//antepost (AM)
			case "t": returnvalue += daysInMonth(the_month+1, the_year); break;//days in month (31)
			case "y": returnvalue += the_year % 100; break;//year (18)
			case "h": returnvalue += hours12.pad(2); break;//12hour (01)
			case "H": returnvalue += hours24.pad(2); break;//24hour (13)
			case "d": returnvalue += day_of_month.pad(2); break;//day of month (22)
			case "m": returnvalue += (the_month+1).pad(2); break;//month (08)
			default: returnvalue += digit;
		}
	}
	return returnvalue;
    //return days_of_week[day_of_week] + " " + month_names[the_month] + " " + day_of_month + ", " + the_year + " @ " + hours12 + ":" + minutes + " " + antepost;
}

function filternonalphanumeric(text, replacewith = ""){
	if(isUndefined(text)){
		return "";
	}
	return text.replace(/[^a-zA-Z0-9 ]+/g, replacewith);
}
function filternumeric(text){
    if(isUndefined(text)){
		return "";
	}
	return text.replace(/[0-9]/g, '');
}
function filternonnumeric(text, allowmore = false){
	if(isUndefined(text)){
		return "";
	}
	if(allowmore){
		return parseFloat(text);
	}
    return text.replace(/\D/g,'');
}


//returns true if $variable appears to be a valid object
//typename (optional): the $variable would also need to be of the same object type (case-sensitive)
function isObject(variable, typename) {
    if (typeof variable == "object") {
        if (isUndefined(typename)) {
            return true;
        }
        return variable.getName().toLowerCase() == typename.toLowerCase();
    }
    return false;
}

String.prototype.contains = function (str) {
	if(isUndefined(str)){return false;}
    return this.toLowerCase().indexOf(str.toLowerCase()) > -1;
};

String.prototype.ucfirst = function () {
    return ucfirst(this);
};

function trim(text, trimoff = " "){
	if(isUndefined(text)){
		return "";
	}
	if(trimoff != " "){
		while(text.startswith(trimoff)){
			text = text.right(text.length - trimoff.length);
		}
		while(text.endswith(trimoff)){
			text = text.left(text.length - trimoff.length);
		}
	}
	return text.trim();
}

//returns true if the string starts with str
String.prototype.startswith = function (str) {
    return this.substring(0, str.length).isEqual(str);
};
String.prototype.endswith = function (str) {
    return this.right(str.length).isEqual(str);
};

//returns the left $n characters of a string
String.prototype.left = function (n) {
    return this.substring(0, n);
};

//returns the right $n characters of a string
String.prototype.right = function (n) {
    return this.substring(this.length - n);
};


String.prototype.mid = function (start, length) {
    return this.substring(start, start + length);
};

String.prototype.GetBetween = function (startingtext, endingtext, inclusive) {
    var target = this;
    if(target.indexOf(startingtext) < 0 || target.indexOf(endingtext) < 0) return false;
    var SP = target.indexOf(startingtext)+startingtext.length;
    var string1 = target.substr(0,SP);
    var string2 = target.substr(SP);
    var TP = string1.length + string2.indexOf(endingtext);
	if(!isUndefined(inclusive)){
		return startingtext + target.substring(SP,TP) + endingtext;
	}
    return target.substring(SP,TP);
};

String.prototype.SetSlice = function (Start, End, ReplaceText) {
    var target = this;
    return target.left(Start) + ReplaceText + target.right(target.length - End);
};

String.prototype.pad = function (size = 2, rightside = false, character = "0") {
	var s = this;
	while (s.length < (size || 2)) {
        if (rightside) {
            s = s + character;
        } else {
            s = character + s;
        }
    }
    return s;
}

Number.prototype.pad = function (size = 2, rightside = false, character = "0") {
    var s = String(this);
    return s.pad(size, rightside, character);
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

function set_HTML(elementID, HTML){
	document.getElementById(elementID).innerHTML = HTML;
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
	ret.sort(dynamicSortMultiple("peripheralName"));
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
