var is_firefox = navigator.userAgent.toLowerCase().indexOf('firefox') > -1;
var is_android = navigator.userAgent.toLowerCase().indexOf('android') > -1;
var is_chromiumedge = window.navigator.userAgent.indexOf("Edg/") > -1;
var is_chrome = is_chromiumedge || (/Chrome/.test(navigator.userAgent) && /Google Inc/.test(navigator.vendor));
var is_edge = window.navigator.userAgent.indexOf("Edge/") > -1;
var is_firefox_for_android = is_firefox && is_android;
var is_IOS = (/iPad|iPhone|iPod/.test(navigator.platform) || (navigator.platform === 'MacIntel' && navigator.maxTouchPoints > 1)) && !window.MSStream;
var newline = "\r\n";
var missingimages = [];
const urlSearchParams = new URLSearchParams(window.location.search);
var params = Object.fromEntries(urlSearchParams.entries());
params.hash = false;
if(window.location.hash) {
      params.hash = window.location.hash.substring(1);
}

function get(key, thedefault = ""){
	if(params.hasOwnProperty(key)){
		return params[key];
	}
	return thedefault;
}

function q(selector, action, value){//mini query
	var elements = document.querySelectorAll(selector);
	var mode = 2;//set
	if(typeof action == "function"){
		mode = 1;//callback
	} else if(value === null){
		mode = 0;//get
	}
	for(var i = 0; i < elements.length; i++){
		if(mode == 0){//get
			switch(action){
				case "val": case "value": 	return elements[i].value;
				default:			return elements[i].getAttribute(action);
			}
		} else if (mode == 1) {
			action(elements[i]);
		} else {
			switch(action){
				case "val": case "value":
					elements[i].value = value;
					break;
				default:
					elements[i].setAttribute(action, value);
			}
		}
	}
	return elements;
}

function isMobileOrTablet() {
	var check = false;
	(function(a){if(/(android|bb\d+|meego).+mobile|avantgo|bada\/|blackberry|blazer|compal|elaine|fennec|hiptop|iemobile|ip(hone|od)|iris|kindle|lge |maemo|midp|mmp|mobile.+firefox|netfront|opera m(ob|in)i|palm( os)?|phone|p(ixi|re)\/|plucker|pocket|psp|series(4|6)0|symbian|treo|up\.(browser|link)|vodafone|wap|windows ce|xda|xiino|android|ipad|playbook|silk/i.test(a)||/1207|6310|6590|3gso|4thp|50[1-6]i|770s|802s|a wa|abac|ac(er|oo|s\-)|ai(ko|rn)|al(av|ca|co)|amoi|an(ex|ny|yw)|aptu|ar(ch|go)|as(te|us)|attw|au(di|\-m|r |s )|avan|be(ck|ll|nq)|bi(lb|rd)|bl(ac|az)|br(e|v)w|bumb|bw\-(n|u)|c55\/|capi|ccwa|cdm\-|cell|chtm|cldc|cmd\-|co(mp|nd)|craw|da(it|ll|ng)|dbte|dc\-s|devi|dica|dmob|do(c|p)o|ds(12|\-d)|el(49|ai)|em(l2|ul)|er(ic|k0)|esl8|ez([4-7]0|os|wa|ze)|fetc|fly(\-|_)|g1 u|g560|gene|gf\-5|g\-mo|go(\.w|od)|gr(ad|un)|haie|hcit|hd\-(m|p|t)|hei\-|hi(pt|ta)|hp( i|ip)|hs\-c|ht(c(\-| |_|a|g|p|s|t)|tp)|hu(aw|tc)|i\-(20|go|ma)|i230|iac( |\-|\/)|ibro|idea|ig01|ikom|im1k|inno|ipaq|iris|ja(t|v)a|jbro|jemu|jigs|kddi|keji|kgt( |\/)|klon|kpt |kwc\-|kyo(c|k)|le(no|xi)|lg( g|\/(k|l|u)|50|54|\-[a-w])|libw|lynx|m1\-w|m3ga|m50\/|ma(te|ui|xo)|mc(01|21|ca)|m\-cr|me(rc|ri)|mi(o8|oa|ts)|mmef|mo(01|02|bi|de|do|t(\-| |o|v)|zz)|mt(50|p1|v )|mwbp|mywa|n10[0-2]|n20[2-3]|n30(0|2)|n50(0|2|5)|n7(0(0|1)|10)|ne((c|m)\-|on|tf|wf|wg|wt)|nok(6|i)|nzph|o2im|op(ti|wv)|oran|owg1|p800|pan(a|d|t)|pdxg|pg(13|\-([1-8]|c))|phil|pire|pl(ay|uc)|pn\-2|po(ck|rt|se)|prox|psio|pt\-g|qa\-a|qc(07|12|21|32|60|\-[2-7]|i\-)|qtek|r380|r600|raks|rim9|ro(ve|zo)|s55\/|sa(ge|ma|mm|ms|ny|va)|sc(01|h\-|oo|p\-)|sdk\/|se(c(\-|0|1)|47|mc|nd|ri)|sgh\-|shar|sie(\-|m)|sk\-0|sl(45|id)|sm(al|ar|b3|it|t5)|so(ft|ny)|sp(01|h\-|v\-|v )|sy(01|mb)|t2(18|50)|t6(00|10|18)|ta(gt|lk)|tcl\-|tdg\-|tel(i|m)|tim\-|t\-mo|to(pl|sh)|ts(70|m\-|m3|m5)|tx\-9|up(\.b|g1|si)|utst|v400|v750|veri|vi(rg|te)|vk(40|5[0-3]|\-v)|vm40|voda|vulc|vx(52|53|60|61|70|80|81|83|85|98)|w3c(\-| )|webc|whit|wi(g |nc|nw)|wmlb|wonu|x700|yas\-|your|zeto|zte\-/i.test(a.substr(0,4))) check = true;})(navigator.userAgent||navigator.vendor||window.opera);
	return check;
}
window.isMobileOrTablet = window.isMobileOrTablet || isMobileOrTablet;

var sections = {
	pc: "PC,AND,iOS,Android,iPad,WIN,Pi,Pi 0,Pi 0 W,MAC",
	playstation: "PS1,PS2,PS3,PS4,PS5,PSP,Vita,PSP Go,PSP (2K+3K only),PSP go,PSP2,VitaTV,PocketStation,PSP (1K+2K+3K only),slim,PSone,PSP2 2K,PS3 (slim),PSP2 1000,PSTV",
	nintendo: "GB,GBA,GBC,GCN,Virtual Boy,N64,NES,SNES,DS,2DS,3DS,Wii,Wii U,WiiU,SNES Classic,NES Classic,DS lite,3DS XL,FAM,Switch,n3DS",
	sega: "GEN,SAT,DC,Saturn,Genesis,32X,SegaCD",
	xbox: "XBOX,XBOX360,X1,XSS,XSX",
	systems: "SYS",
	misc: "Zodiac,Arcade,WSC,WS,Atari,Sinclair,Other,Playdia,Pippin,Pair Match,Misc,misc,Sharp X68000,MISC,GS,COL,Multi,VCS,PCE,MSX,NEC PC-88,Sharp 68000,FM Towns",
	sub: "Astro City Mini,Neo Geo Mini,Pokitto,1942,1943",
};

function toast(text){
	var x = document.getElementById("snackbar");
	x.innerHTML = text;
  	x.className = "show";
  	setTimeout(function(){ x.className = x.className.replace("show", ""); }, 3000);
}

function msgbox(title, text){
	set_HTML("modalhead", title);
	set_HTML("modalbody", text);
	var modal = document.getElementById("modal");
	modal.style.display = "block";
	document.getElementsByClassName("close")[0].onclick = function() {
  		modal.style.display = "none";
	};
}

window.onclick = function(event) {
	var modal = document.getElementById("modal");
  if (event.target == modal) {
    modal.style.display = "none";
  }
} 

function console_section(console){
	for(var section in sections){
		if(sections.hasOwnProperty(section)){
			if(sections[section].indexOf(console) > -1){
				return section;
			}
		}
	}
	return false;
}

function _GET(name, def = "", url = window.location.href) {
    name = name.replace(/[\[\]]/g, '\\$&');
    var regex = new RegExp('[?&]' + name + '(=([^&#]*)|&|#|$)'),
        results = regex.exec(url);
    if (!results) return def;
    if (!results[2]) return def;
    return decodeURIComponent(results[2].replace(/\+/g, ' '));
}

function controller_section(controller, normal = false){
	if(isNumeric(controller)){
		controller = controllers[controller];
		controller.source = "number";
	} else if(typeof controller == "string"){
		controller = find(controllers, ["peripheral", "peripheralName"], controller, true);
		controller.source = "name";
	} else {
		controller.source = "object";
	}
	var section = false;
	if(!controller){
		return null;
	} else if(controller.hasOwnProperty("section")){
		section = controller.section;
	} else if(controller.hasOwnProperty("systems")){
		var systems = controller.systems.replaceAll(",", "/").split("/");
		for(var i = 0; i < systems.length; i++){
			var section2 = console_section(systems[i]);
			if(section){
				if(section != section2){
					return "misc";// iif(normal, "misc", "multiple");
				}
			} else {
				section = section2;
			}
		}
	}
	if(normal && section == "sub"){
		section = "systems";
	}
	if(typeof section != "string"){
		console.log("Invalid section: " + section);
	}
	return section.toString().toLowerCase();
}
	
function isArray(variable) {
    return Array.isArray(variable);
}
	
function find(data, keys, value, asvalue = false){
	if(!isArray(keys)){
		keys = [keys];
	}
	for(var i = 0; i < data.length; i++){
		for(var z = 0; z < keys.length; z++){
			var key = keys[z];
			if(data[i].hasOwnProperty(key) && data[i][key].isEqual(value)){
				if(asvalue){
					return data[i];
				}
				return i;
			}
		}
	}
	return -1;
}

if(is_chrome){
	var log = console.log;
}

function iif(value, iftrue, iffalse = "") {
    if (value) {return iftrue;}
    return iffalse;
}

Number.prototype.isEqual = function (str) {
	return this.toString().isEqual(str);
};

String.prototype.replaceAll = function (search, replacement, insensitive) {
    var target = this;
    if (isArray(search)) {
        for (var i = 0; i < search.length; i++) {
            if (isArray(replacement)) {
                target = target.replaceAll(search[i], replacement[i], insensitive);
            } else {
                target = target.replaceAll(search[i], replacement, insensitive);
            }
        }
        return target;
    }
	if(isUndefined(insensitive)){
		return target.replace(new RegExp(search, 'g'), replacement);
	} 
	var esc = escapeRegExp(search);// search.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
    var reg = new RegExp(esc, 'ig');
    return this.replace(reg, replacement);
};

String.prototype.between = function (leftside, rightside) {
    var target = this;
    var start = target.indexOf(leftside);
    if (start > -1) {
        var finish = target.indexOf(rightside, start);
        if (finish > -1) {
            return target.substring(start + leftside.length, finish);
        }
    }
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
	
String.prototype.startswith = function (str) {
    return this.substring(0, str.length).isEqual(str);
};
String.prototype.endswith = function (str) {
    return this.right(str.length).isEqual(str);
};
	
String.prototype.replaceAll = function (search, replacement) {
    var target = this;
    var esc = escapeRegExp(search);// search.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
    var reg = new RegExp(esc, 'ig');
    return this.replace(reg, replacement);
};

function dynamicSort(property) { 
    return function (obj1,obj2) {
        return obj1[property] > obj2[property] ? 1
            : obj1[property] < obj2[property] ? -1 : 0;
    }
}

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

function enum_controllers_by_console(consoles = false, include = true){
	var ret = [];
	if(!Array.isArray(consoles)){
		consoles = consoles.split(",");
	}
	for(var i = 0; i < controllers.length; i++){
		if(!controllers[i].hasOwnProperty("attached")){
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
	}
	return enum_attached_controllers(ret);
}

function enum_controllers_by_section(section = false, include = true){
	var ret = [];
	for(var i = 0; i < controllers.length; i++){
		if(!controllers[i].hasOwnProperty("attached")){
			if(controllers[i].hasOwnProperty("systems") && section){
				var systems = controller_section(controllers[i], true);
				var included = systems == section;
				if(included == include){
					ret.push(controllers[i]);
				}
			} else if(section === false && include) {
				ret.push(controllers[i]);
			}
		}
	}
	
	return enum_attached_controllers(ret);
}

function enum_attached_controllers(ret){
	var IDs = {};
	for(var i = 0; i < ret.length; i++){
		var controller = ret[i];
		if(controller.hasOwnProperty("peripheral")){
			IDs[ controller.peripheral ]  = i;
		}
	}
	
	for(var i = 0; i < controllers.length; i++){
		var controller = controllers[i];
		if(controller.hasOwnProperty("attached") && IDs.hasOwnProperty( controller.attached ) > -1){
			var index = IDs[ controller.attached ];
			if(!isUndefined(ret[index])){
				if(!ret[index].hasOwnProperty("attachments")){
					ret[index].attachments = [];
				}
				ret[index].attachments.push(controller);
			}
		} 
		//attempt to fix sorting
		if(controller.hasOwnProperty("title")){
			controllers[i].Name = controller.title;
		} else if(controller.hasOwnProperty("peripheralName")){
			controllers[i].Name = controller.peripheralName;
		} else {
			controllers[i].Name = controller.peripheral;
		}
	}

	ret.sort(dynamicSortMultiple("Name"));
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
if(anames.length > 1){
  document.getElementById('body').insertAdjacentHTML('afterbegin', '<H1 CLASS="tos">Table of Contents</H1><UL ID="tos" CLASS="tos"></UL>');
  var tos = document.getElementById("tos");
  for(var i = 0; i < anames.length; i++){
    var element = anames[i];
    var name = element.textContent.toLowerCase().trim().replaceAll(" ", "-");
    element.innerHTML += '<A NAME="' + name + '">';
    tos.innerHTML += '<LI><A HREF="#' + name + '">' + element.textContent + '</A></LI>';
  }
}

function set_HTML(elementID, HTML, param = "html"){
	var element = document.getElementById(elementID);
	if(isUndefined(HTML)){
		switch(param){
			case "html": return element.innerHTML;
			case "val": return element.value;
		}
	} 
	switch(param){
		case "html": element.innerHTML = HTML; break;
		case "val": element.value = HTML; break;
	}
	//if(window.location.hash) {
      	//	var hash = window.location.hash.substring(1);
	//}
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

function showgames(section, asAlert = true){
	var HTML = '<TABLE CLASS="table"><TBODY>';
	if(!asAlert){
		HTML += '<TR><TH COLSPAN="2" CLASS="header">' + section + '</TH></TR>';
	}
	if(games[section].hasOwnProperty("URL")){
		HTML += '<TR><TH WIDTH="20%">URL</TH><TD><A HREF="' + games[section].URL + '" TARGET="_new">' + games[section].URL + '</A></TD></TR>';
	}
	if(games[section].hasOwnProperty("games")){
		if(games[section].games.contains("<TABLE")){
			HTML += '<TR><TH WIDTH="20%">Games</TH><TD>' + games[section].games + '</TD></TR>';
		} else {
			HTML += '<TR><TH WIDTH="20%">Games</TH><TD>' + nl2br(games[section].games) + '</TD></TR>';
		}
	}
	if(games[section].hasOwnProperty("info")){
		HTML += '<TR><TD COLSPAN="2">' + nl2br(games[section].info) + '</TD></TR>';
	}
	HTML += '</TBODY></TABLE>';
	if(asAlert){
		msgbox(section, HTML);
	}
	return HTML;
}

function make_controller(controller = false, stat = false, name = false){
	var HTML = "";
	if(stat === "images"){//just the images
		if(!controller.hasOwnProperty("noimage")){
			name = controller.peripheralName;
			if(controller.hasOwnProperty("title")){
				name = controller.title;
			}
			if(controller.hasOwnProperty("image")){
				HTML += makeimg(controller.image, name);
			} else if(controller.hasOwnProperty("images")){
				for(var i = 0; i < controller.images.length; i++){
					HTML += makeimg(controller.images[i], name);
				}
			} else {
				HTML += makeimg(controller.peripheral + '.jpg', name);
			}
		}
	} else if(stat === true){//do all stats in normal style
		var classname = toclassname(controller.peripheralName);//
		switch(name){
			case "normal": case "noimage":
				if(name == "noimage" && controller.hasOwnProperty("title")){
					HTML += '<TR><TH COLSPAN="2" CLASS="header">' + controller.title + '</TH></TR><TR>';
				}
				HTML += '<TR><TH COLSPAN="2" CLASS="header">' + controller.peripheralName + '</TH></TR><TR>';
				if(name == "normal"){
					HTML += '<TD ROWSPAN="2" CLASS="image top">' + make_controller(controller, "images") + '</TD><TD CLASS="controllerinfo top">';
				} else {
					HTML += '<TD CLASS="controllerinfo top" COLSPAN="2"><A NAME="' + classname + '"></A>';
				}
				HTML += make_controller(controller, "peripheral", 		"Peripheral ID");
				
				if(controller.hasOwnProperty("games") && typeof controller.games == "string" && games.hasOwnProperty(controller.games)){
					HTML += '<B>Games: <SPAN CLASS="hyperlink" ONCLICK="showgames(' + "'" + controller.games + "'" + ');">List</SPAN><BR>';
				} else {
					HTML += make_controller(controller, "games", 			"Games Supported");
				}
				if(controller.hasOwnProperty("moreinfo")){
					HTML += '<B>More Info: <SPAN CLASS="hyperlink" ONCLICK="showgames(' + "'" + controller.moreinfo + "'" + ');">Click Here</SPAN><BR>';
				}
				
				HTML += make_controller(controller, "systems", 			"Systems Supported");
				HTML += make_controller(controller, "obtained", 		"Obtained");
				HTML += make_controller(controller, "cost", 			"Can be found for");
				HTML += make_controller(controller, "company", 			"Made by");
				HTML += make_controller(controller, "location",			"Location");
				HTML += make_controller(controller, "specificVersion", 		"Specific Version");
				HTML += make_controller(controller, "otherVersions", 		"Other Version(s)");
				HTML += make_controller(controller, "files", 			"Attachments");
				HTML += make_controller(controller, "urls",			"URLs");
				HTML += '</TD></TR><TR><TD CLASS="top">';
				HTML += make_controller(controller, "description");
				HTML += '</TD></TR>';
				break;
			case "list":
				HTML += '<TR><TD><A NAME="' + classname + '"></A>';
				if(controller.hasOwnProperty("peripheral")){
					HTML += controller.peripheral;
				}
				HTML += '</TD><TD>' + controller.peripheralName + '</TD><TD>';
				if(controller.hasOwnProperty("obtained")){
					HTML += controller.obtained;
				}
				HTML += '</TD></TR>';
				break;
		}
	} else if(stat !== false){//do a specific stat
		if(controller.hasOwnProperty(stat) && controller[stat]){
			if(name){
				HTML += "<B>" + name + "</B>: ";
				switch(stat){
					case "urls":
						var del = "";
						for(var att in controller[stat]){
							if(controller[stat].hasOwnProperty(att)){
								HTML += del + '<A HREF="' + att + '">' + controller[stat][att] + '</A>';
								del = ", ";
							}
						}
						break;
					case "files":
						var del = "";
						for(var att in controller[stat]){
							if(controller[stat].hasOwnProperty(att)){
								HTML += del + '<A HREF="/files/' + controller[stat][att] + '">' + att + '</A>';
								del = ", ";
							}
						}
						break;
					case "cost": 
						HTML += "~$" + controller[stat]; 
						if(controller.hasOwnProperty("importOnly")){
							HTML += " (Import Only)";
						}
						break;
					case "games": 
						if(Array.isArray(controller[stat])){
							HTML += controller[stat].join(", ");
						} else {
							HTML += controller[stat];
						}
						break;
					default:
						HTML += controller[stat];
				}
				HTML += "<BR>";
			} else {
				HTML = nl2br(controller[stat]);
			}
		}
	} else if(controller === false){//do all controllers
		HTML += '<UL CLASS="cols-4">';
		for(var i = 0; i < controllers.length; i++){
			var controller = controllers[i];
			HTML += '<LI><A HREF="#' + toclassname(controller.peripheralName) + '">';
			if(controller.hasOwnProperty("title")){
				HTML += controller.title;
			} else {
				HTML += controller.peripheralName;
			}
			HTML += '</A></LI>';
		}
		HTML += '</UL>';
		for(var i = 0; i < controllers.length; i++){
			HTML += make_controller(controllers[i]);
		}
	} else {//do a specific controller
		var classname = toclassname(controller.peripheralName);
		HTML += '<A NAME="' + classname + '"></A>';
		var style = "normal";
		if(controller.hasOwnProperty("style")){
			style = controller.style;
		}
		switch(style){
			case "normal": case "list":
				HTML += '<TABLE ID="' + classname + '" NAME="' + controller.peripheralName.toLowerCase() + '" CLASS="table searchable">' + make_controller(controller, true, iif(style == "list", "noimage", style));
				if(style == "list"){
					HTML += '<TR><TD COLSPAN="2" CLASS="listtable"><TABLE CLASS="listtable"><THEAD><TR><TH>ID</TH><TH>Name</TH><TH>Obtained</TH></TR></THEAD><TBODY>' + make_controller(controller, true, style);
				}				
				if(controller.hasOwnProperty("attachments")){
					for(var i = 0; i < controller.attachments.length; i++){
						HTML += make_controller(controller.attachments[i], true, style);
					}
				}
				if(style == "list"){
					HTML += '</TBODY><TFOOT><TR><TD COLSPAN="3" CLASS="imagelist">' + make_controller(controller, "images");
					if(controller.hasOwnProperty("attachments")){
						for(var i = 0; i < controller.attachments.length; i++){
							HTML += make_controller(controller.attachments[i], "images");
						}
					}
					HTML += '</TD></TR></TFOOT></TABLE></TD></TR>';
				}
				HTML += '</TABLE>';
				break;
		}
	}
	return HTML;
}

function unisearch(text){
	if(typeof query == "function"){
		window["query"](text);
	} else {
		visible(text);
	}
}

function visible(text, selector = ".searchable"){
	text = text.trim().toLowerCase();
	var elements = document.querySelectorAll(selector);
	for(var i = 0; i < elements.length; i++){
		var found = text.length == 0;
		if(!found){
			found = elements[i].getAttribute("name").contains(text);
		}
		if(found){
			RemoveClass(elements[i], "dont-show");
		} else {
			AddClass(elements[i], "dont-show");
		}
	}
}

function setVisible(selector, visible = true){
	var elements = document.querySelectorAll(selector);
	for(var i = 0; i < elements.length; i++){
		if(visible){//show
			RemoveClass(elements[i], "dont-show");
		} else {//hide
			AddClass(elements[i], "dont-show");
		}
	}
}

function AddClass(element, classname){
	element.classList.add(classname);
}

function RemoveClass(element, classname){
	element.classList.remove(classname);
}

function makeimg(filename, description, folder = 'images'){
	return '<IMG SRC="' + iif(filename.contains('/'), "", '/' + folder + '/') + filename + '" CLASS="controllerimage" ONERROR="imgError(this);" ONCLICK="expandimage(this);" ALT="' + description + '">';
}

function make_photos(titles = true, folder = 'photos'){
	var level = 0;
	var LIST = '<UL' + iif(!titles, ' CLASS="cols-4"') + '>';
	var HTML = '';
	for(var key in photos){
		if(photos.hasOwnProperty(key)){
			if(photos[key].hasOwnProperty("text")){
				var classname = toclassname(key);
				HTML += '<A NAME="' + classname + '"></A><TABLE ID="photo-' + classname + '" NAME="' + key.toLowerCase() + '" CLASS="table searchable">';
				if(titles){
					HTML += '<TR><TH COLSPAN="2" CLASS="header">' + key + '</TH></TR><TR>';
					LIST += '<LI><A HREF="#' + toclassname(key) + '">' + key + '</A></LI>';
				}
				if(!photos[key].hasOwnProperty("images")){
					photos[key].images = [key + ".jpg"];
				} else if(!isArray(photos[key].images)){
					photos[key].images = [photos[key].images];
				}
				if(photos[key].hasOwnProperty("horizontal")){
					HTML += '<TR><TD CLASS="image top imagelist" COLSPAN="2">';
				} else {
					HTML += '<TR><TD CLASS="image top">';
				}
				for(var i = 0; i < photos[key].images.length; i++){
					HTML += makeimg(photos[key].images[i], iif(titles, key, 'Image'), folder);
				}
				if(photos[key].hasOwnProperty("horizontal")){
					HTML += '</TD></TR><TR><TD COLSPAN="2">';	
				} else {
					HTML += '</TD><TD>';
				}
				if(photos[key].hasOwnProperty("url")){
					HTML += '<A HREF="' + photos[key].url + '">';
				}
				HTML += nl2br(photos[key].text);
				if(photos[key].hasOwnProperty("url")){
					HTML += '</A>';	
				}
				if(photos[key].hasOwnProperty("cost")){
					HTML += '<BR> Cost: $' + photos[key].cost;
					if(photos[key].hasOwnProperty("unit")){
						HTML += ' (' + photos[key].unit + ')';
					}
				}
				if(photos[key].hasOwnProperty("releases")){
					HTML += '<BR> Releases: ' + photos[key].releases;
				}				
				HTML += '</TD></TR></TABLE>';
			} else {
				var name = toclassname(key);
				LIST += iif(level > 0, iif(titles, '</UL>') + '</LI>') + '<LI><A HREF="#' + name + '">' + key + '</A>' + iif(titles, '<UL>', '</LI>');
				HTML += '<A NAME="' + name + '"></A><H2>' + key + '</H2>';
				level = 1;
			}
		}
	}
	if(level > 0 && titles){
		LIST += '</UL></LI>';
	}
	return LIST + "</UL>" + HTML;
}

function imgError(image) {
    image.onerror = "";
    image.onclick = "";
    image.setAttribute("original", image.src);
    missingimages.push(image.src);
    image.src = "/images/noimage.png";
    return true;
}

function expandimage(element){
    msgbox(element.getAttribute("alt"), '<CENTER><IMG ID="expandimage" SRC="' + element.src + '" STYLE="max-width: 100% !important;" USEMAP="#' + getfilename(element.src, false) + '"></CENTER>');
}

function getfilename(url, includeextension = true){
 	url = url.split("/");
	url = url[ url.length - 1];
	if(!includeextension){
		url = url.split(".");
		url.splice(url.length - 1, 1); 
		url = url.join(".");
	}
	return url;
}

function nl2br(str) {
    return (str + '').replace(/([^>\r\n]?)(\r\n|\n\r|\r|\n)/g, '$1<BR>$2');
}

function toclassname(text) {
	if(isUndefined(text)){return "";}
	text = text + "";
	text = text.toLowerCase().replaceAll("-", "MINUS");
	text = text.replaceAllbrute("#", "").replaceAllbrute("/", "");
    text = text.replace(/[\W]+/g,"_").replaceAll("__", "_").replaceAll("MINUS", "-");
	if(isNumeric(text.left(1))){
		text = "n" + text;
	}
	return trimChar(text, "_").left(64);
}

String.prototype.replaceAllbrute = function (search, replacement) {
	var target = this;
	while(target.indexOf(search) > -1){
	//while(target.contains(search)){
		target = target.replace(search, replacement);
	}
	return target;
}; 

var trimChar = function(origString, charToTrim) {
	charToTrim = escapeRegExp(charToTrim);
	var regEx = new RegExp("^[" + charToTrim + "]+|[" + charToTrim + "]+$", "g");
	return origString.replace(regEx, "");
};
