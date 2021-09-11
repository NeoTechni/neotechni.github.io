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
    var name = element.textContent.toLowerCase().replaceAll(" ", "-");
    element.innerHTML += '<A NAME="' + name + '">';
    tos.innerHTML += '<LI><A HREF="#' + name + '">' + element.textContent + '</A></LI>';
  }
}
