var current_region 	= getCookie("region", "en-us");
	var table_data 		= {};
	var regions 		= {
		"USA"			: "en-us",
		"Canada"		: "en-ca",
	};
	
	//console.log("STARTING REGION: " + current_region);
	
	String.prototype.contains = function (str) {
		if(isUndefined(str)){return false;}
		return 			this.toLowerCase().indexOf(str.toLowerCase()) > -1;
	};

	String.prototype.ucfirst = function () {
		return 			ucfirst(this);
	};
	
	//returns true if the string starts with str
	String.prototype.startswith = function (str) {
		return 			this.substring(0, str.length).isEqual(str);
	};
	String.prototype.endswith = function (str) {
		return 			this.right(str.length).isEqual(str);
	};

	//returns the left $n characters of a string
	String.prototype.left = function (n) {
		return 			this.substring(0, n);
	};

	//returns the right $n characters of a string
	String.prototype.right = function (n) {
		return 			this.substring(this.length - n);
	};


	String.prototype.mid = function (start, length) {
		return 			this.substring(start, start + length);
	};
	
	function isUndefined(variable) {
		if(typeof variable == "number" && isNaN(variable)){return true;}
		return 			typeof variable === 'undefined' || variable == null;
	}
		
	function storageAvailable(type) {
		try {//types: sessionStorage, localStorage
			var storage = window[type], x = '__storage_test__';
			storage.setItem(x, x);
			storage.removeItem(x);
			return 		true;
		} catch(e) {
			return 		false;
		}
	}
	
	var uselocalstorage = storageAvailable('localStorage');
	function hasItem(c_name){
		if(uselocalstorage){
			return 		window['localStorage'].getItem(c_name) !== null;
		} else {
			var i, x, y, ARRcookies = document.cookie.split(";");
			for (i = 0; i < ARRcookies.length; i++) {
				x 		= ARRcookies[i].substr(0, ARRcookies[i].indexOf("="));
				y 		= ARRcookies[i].substr(ARRcookies[i].indexOf("=") + 1);
				x 		= x.replace(/^\s+|\s+$/g, "");
				if (x == c_name) {
					return true;
				}
			}
		}
		return 			false;
	}

	function setCookie(c_name, value, exdays = 365) {
		if(uselocalstorage){
			window['localStorage'].setItem(c_name, value);
		} else {
			var exdate 	= new Date();
			exdate.setDate(exdate.getDate() + exdays);
			var c_value = value + ((exdays == null) ? "" : "; expires=" + exdate.toUTCString());
			c_value 	= c_name + "=" + c_value + ";path=/;";
			document.cookie = c_value;
		}
	}

	//gets a cookie value
	function getCookie(c_name, defaultvalue = false) {
		if(uselocalstorage && hasItem(c_name)){
			return 		window['localStorage'].getItem(c_name);
		}
		var i, x, y, ARRcookies = document.cookie.split(";");
		for (i = 0; i < ARRcookies.length; i++) {
			x 			= ARRcookies[i].substr(0, ARRcookies[i].indexOf("="));
			y 			= ARRcookies[i].substr(ARRcookies[i].indexOf("=") + 1);
			x 			= x.replace(/^\s+|\s+$/g, "");
			if (x == c_name) {
				return unescape(y);
			}
		}
		return 			defaultvalue;
	}

	//deletes a cookie value
	function removeCookie(cname, forcecookie) {
		if(isUndefined(forcecookie)){forcecookie = false;}
		if (isUndefined(cname)) {//erase all cookies
			var cookies = document.cookie.split(";");
			for (var i = 0; i < cookies.length; i++) {
				var cookie = cookies[i];
				var eqPos = cookie.indexOf("=");
				var name = eqPos > -1 ? cookie.substr(0, eqPos) : cookie;
				removeCookie(name, true);
			}
			if(uselocalstorage) {
				cookies = Object.keys(window['localStorage']);
				for (var i = 0; i < cookies.length; i++) {
					if(!cookies[i].startswith("sys_")){
						removeCookie(cookies[i]);
					}
				}
			}
		} else if(hasItem(cname) && !forcecookie){
			window['localStorage'].removeItem(cname);
		} else {
			document.cookie = cname + "=;expires=Thu, 01 Jan 1970 00:00:00 GMT;path=/;";
		}
	}

	
	
	
	
	
	
	
	
	
	function getset(node, attribute = null, value = null){
		if(Array.isArray(node)){
			//console.log("ARRAY FOUND, GETSET ALL: " + node.length);
			for(var i = 0; i < node.length; i++){
				value 	= getset(node, attribute, value);
			}
		} else {
			switch(typeof node){
				case "object"://is a single node
					//console.log("SINGLE NODE FOUND: " + attribute + " " + iif(value == null, "GET", "SET TO: " + value));
					switch(attribute){
						case "html":
							if(value == null){//get
								value = node.innerHTML;
							} else {
								node.innerHTML = value;
							}
							break;
						case "value":
							if(value == null){//get
								value = node.value;
							} else {
								node.value = value;
							}
							break;
						default:
							if(value == null){//get
								value = node.getAttribute(attribute);
							} else {
								node.setAttribute(attribute, value);
							}
					}
					break;
				case "string"://is a selector, I hope
					//console.log("SELECTOR FOUND: " + node);
					var nodes = document.querySelectorAll(node);//convert to a simple array
					node = [];
					for(var i = 0; i < nodes.length; i++){
						node.push(nodes[i]);
					}
					if(attribute == null){//return the array
						return node;
					} else if(nodes.length == 1){
						return getset(node[0], attribute, value);
					}
					return getset(node, attribute, value);
			}		
			//console.log("FOUND: " + value);
		}
		return 			value;
	}
	
	function iif(value, iftrue, iffalse = "") {
		if (value) {return iftrue;}
		return 			iffalse;
	}
	
	function ucfirst(text) {
		if(text.toLowerCase() == "id"){
			return 		"ID";
		}
		return 			text.left(1).toUpperCase() + text.right(text.length - 1);
	}
	
	function makecolheader(elementid, name){
		return 			'<TH ID="' + elementid + '-col-' + name + '" ONCLICK="sorttable(' + "'" + elementid + "', '" + name + "'" + ');">' + ucfirst(name) + '</TH>';
	}
	
	function maketable(elementid, data = false){
		if(!data){
			table_data[elementid] = {
				"sort_by"		: "name",
				"sort_order"	: true,
				"data"			: JSON.parse(getset("#" + elementid, "html"))
			};
			for(var key in table_data[elementid].data){
				if(table_data[elementid].data.hasOwnProperty(key)){
					table_data[elementid].data[key].name = key;
				}				
			}
		}
		data 			= "'" + elementid + "'";
		var HTML 		= '<TABLE ID="' + elementid + '-table" CLASS="psn-table"><THEAD><TR><TD COLSPAN="2">';
		
		HTML 			+= '<SELECT ID="' + elementid + '-region" CLASS="psn-region" ONCHANGE="psnquery(' + data + ');">';
		for(var region in regions){
			if(regions.hasOwnProperty(region)){
				HTML 	+= '<OPTION VALUE="' + regions[region] + '"';
				if(current_region == region || current_region == regions[region]){
					HTML += ' SELECTED';
				}
				HTML 	+= '>' + region + '</OPTION>';
			}
		}		
		HTML 			+= '</SELECT>';
		
		HTML			+= '<INPUT ID="' + elementid + '-search" TYPE="text" CLASS="textsearch" PLACEHOLDER="Search" ONKEYUP="psnquery(' + data + ');" ONPASTE="psnquery(' + data + ');">';
		
		HTML 			+= '</TD></TR><TR>';
		HTML 			+= makecolheader(elementid, "name");
		HTML 			+= makecolheader(elementid, "price");
		HTML 			+= '</TR></THEAD><TBODY ID="' + elementid + '-body">';
		HTML 			+= '</TBODY></TABLE>';
		getset("#" + elementid, "html", HTML);
		psnquery(elementid);
		return 			HTML;
	}
	
	function sorttable(elementid, column){
		if(table_data[elementid].sort_by == column){
			table_data[elementid].sort_order 	= !table_data[elementid].sort_order;
		} else {
			table_data[elementid].sort_by 		= column;
			table_data[elementid].sort_order 	= true;
		}
		psnquery(elementid);
	}
	
	function makerow(rowdata){
		var HTML 		= '<TR>';
		var URL 		= rowdata.url;
		if(current_region != "en-us"){
			URL			= URL.replace("/en-us/", "/" + current_region + "/");
		}		
		URL 			= '<A HREF="' + URL + '" TARGET="psn">';
		
		HTML			+= '<TD>' + URL + rowdata.name + '</A></TD>';
		HTML			+= '<TD ALIGN="right">' + URL + '$' + rowdata.price + '</A></TD>';
		return 			HTML += '</TR>';
	}
	
	function sortByKey(array, key, asc) {
		return array.sort(function(a, b) {
			var x 		= a[key]; 
			var y 		= b[key];
			if(asc){
				return 	((x < y) ? -1 : ((x > y) ? 1 : 0));
			}
			return 		((x < y) ? 1 : ((x > y) ? -1 : 0));
		});
	}
	
	function psnquery(elementid){
		current_region	= getset("#" + elementid + "-region", "value");
		setCookie("region", current_region);
		var searchtext	= getset("#" + elementid + "-search", "value").trim();
		var HTML 		= '';
		
		var data 		= [];
		for(var key in table_data[elementid].data){
			if(table_data[elementid].data.hasOwnProperty(key)){
				var found = true;
				if(searchtext){
					found = key.contains(searchtext);
				}
				if(found){
					data.push(table_data[elementid].data[key]);
				}
			}
		}		
		data 			= sortByKey(data, table_data[elementid].sort_by, table_data[elementid].sort_order);	
		for(i = 0; i < data.length; i++){
			HTML		+= makerow(data[i]);
		}			
		return 			getset("#" + elementid + "-body", "html", HTML);
	}
	
	window.addEventListener('load', function () {
		var tables 		= getset(".psn-prices");		
		for(var i = 0; i < tables.length; i++){
			tables[i] 	= getset(tables[i], "id");
			console.log("LOADING " + tables[i] + ": " + maketable(tables[i]));
		}
			
	});


/*

usage:
  Add this to the page: <SCRIPT SRC="https://neotechni.github.io/psn.js"></SCRIPT>
  Add a DIV to the page, it must have an ID and a CLASS="psn-prices"
  Inside the DIV must contain ***VALID*** JSON data for the name of the game, URL and price
  the code will convert that JSON to a searchable+sortable table, saving the user's selected region (I hope)
  example: (note how the last game does not have a comma after the })
  
  <DIV ID="price1" CLASS="psn-prices">
    {
  			"AstroBot": {"url": "https://store.playstation.com/en-us/concept/10002684", "price": 79.99},
  			"Marvel’s Spider-Man 2": {"url": "https://store.playstation.com/en-us/concept/10002456", "price": 89.99},
  			"LEGO® Horizon Adventures™": {"url": "https://store.playstation.com/en-us/concept/10007722", "price": 69.99}
    }
  </DIV>

suggested default style:
  .psn-prices table, .psn-prices table th, .psn-prices table tr, .psn-prices table thead, .psn-prices table tbody{
		border			: 1px solid black;
		border-collapse	: collapse;
	}
	
	.psn-prices table thead td{
		background-color: #f9f9f9;
	}
	
	.psn-prices table th{
		background-color: #ffffff; 
		text-align		:center;
		border-bottom	:2px solid #f9f9f9;
	}
	
	.psn-prices table td{
		padding-left	: 2px; 
		padding-right	: 2px;
	}
	
	.psn-prices table tr:nth-child(even) {
		background		: darkgrey;
	}
	
	.psn-prices table tr:nth-child(odd) {
		background		: white;
	}

*/
