(window.webpackJsonp=window.webpackJsonp||[]).push([[4],[,,function(r,n,e){var t=e(30);"string"==typeof t&&(t=[[r.i,t,""]]);var o={hmr:!0,transform:void 0,insertInto:void 0};e(9)(t,o);t.locals&&(r.exports=t.locals)},,,,,function(r,n,e){"use strict";r.exports=function(r){var n=[];return n.toString=function(){return this.map(function(n){var e=function(r,n){var e=r[1]||"",t=r[3];if(!t)return e;if(n&&"function"==typeof btoa){var o=(f=t,"/*# sourceMappingURL=data:application/json;charset=utf-8;base64,"+btoa(unescape(encodeURIComponent(JSON.stringify(f))))+" */"),i=t.sources.map(function(r){return"/*# sourceURL="+t.sourceRoot+r+" */"});return[e].concat(i).concat([o]).join("\n")}var f;return[e].join("\n")}(n,r);return n[2]?"@media "+n[2]+"{"+e+"}":e}).join("")},n.i=function(r,e){"string"==typeof r&&(r=[[null,r,""]]);for(var t={},o=0;o<this.length;o++){var i=this[o][0];null!=i&&(t[i]=!0)}for(o=0;o<r.length;o++){var f=r[o];null!=f[0]&&t[f[0]]||(e&&!f[2]?f[2]=e:e&&(f[2]="("+f[2]+") and ("+e+")"),n.push(f))}},n}},function(r,n,e){"use strict";r.exports=function(r,n){return"string"!=typeof r?r:(/^['"].*['"]$/.test(r)&&(r=r.slice(1,-1)),/["'() \t\n]/.test(r)||n?'"'+r.replace(/"/g,'\\"').replace(/\n/g,"\\n")+'"':r)}},function(r,n,e){var t,o,i={},f=(t=function(){return window&&document&&document.all&&!window.atob},function(){return void 0===o&&(o=t.apply(this,arguments)),o}),a=function(r){var n={};return function(r,e){if("function"==typeof r)return r();if(void 0===n[r]){var t=function(r,n){return n?n.querySelector(r):document.querySelector(r)}.call(this,r,e);if(window.HTMLIFrameElement&&t instanceof window.HTMLIFrameElement)try{t=t.contentDocument.head}catch(r){t=null}n[r]=t}return n[r]}}(),d=null,p=0,s=[],l=e(10);function c(r,n){for(var e=0;e<r.length;e++){var t=r[e],o=i[t.id];if(o){o.refs++;for(var f=0;f<o.parts.length;f++)o.parts[f](t.parts[f]);for(;f<t.parts.length;f++)o.parts.push(w(t.parts[f],n))}else{var a=[];for(f=0;f<t.parts.length;f++)a.push(w(t.parts[f],n));i[t.id]={id:t.id,refs:1,parts:a}}}}function b(r,n){for(var e=[],t={},o=0;o<r.length;o++){var i=r[o],f=n.base?i[0]+n.base:i[0],a={css:i[1],media:i[2],sourceMap:i[3]};t[f]?t[f].parts.push(a):e.push(t[f]={id:f,parts:[a]})}return e}function u(r,n){var e=a(r.insertInto);if(!e)throw new Error("Couldn't find a style target. This probably means that the value for the 'insertInto' parameter is invalid.");var t=s[s.length-1];if("top"===r.insertAt)t?t.nextSibling?e.insertBefore(n,t.nextSibling):e.appendChild(n):e.insertBefore(n,e.firstChild),s.push(n);else if("bottom"===r.insertAt)e.appendChild(n);else{if("object"!=typeof r.insertAt||!r.insertAt.before)throw new Error("[Style Loader]\n\n Invalid value for parameter 'insertAt' ('options.insertAt') found.\n Must be 'top', 'bottom', or Object.\n (https://github.com/webpack-contrib/style-loader#insertat)\n");var o=a(r.insertAt.before,e);e.insertBefore(n,o)}}function x(r){if(null===r.parentNode)return!1;r.parentNode.removeChild(r);var n=s.indexOf(r);n>=0&&s.splice(n,1)}function g(r){var n=document.createElement("style");if(void 0===r.attrs.type&&(r.attrs.type="text/css"),void 0===r.attrs.nonce){var t=function(){0;return e.nc}();t&&(r.attrs.nonce=t)}return h(n,r.attrs),u(r,n),n}function h(r,n){Object.keys(n).forEach(function(e){r.setAttribute(e,n[e])})}function w(r,n){var e,t,o,i;if(n.transform&&r.css){if(!(i="function"==typeof n.transform?n.transform(r.css):n.transform.default(r.css)))return function(){};r.css=i}if(n.singleton){var f=p++;e=d||(d=g(n)),t=k.bind(null,e,f,!1),o=k.bind(null,e,f,!0)}else r.sourceMap&&"function"==typeof URL&&"function"==typeof URL.createObjectURL&&"function"==typeof URL.revokeObjectURL&&"function"==typeof Blob&&"function"==typeof btoa?(e=function(r){var n=document.createElement("link");return void 0===r.attrs.type&&(r.attrs.type="text/css"),r.attrs.rel="stylesheet",h(n,r.attrs),u(r,n),n}(n),t=function(r,n,e){var t=e.css,o=e.sourceMap,i=void 0===n.convertToAbsoluteUrls&&o;(n.convertToAbsoluteUrls||i)&&(t=l(t));o&&(t+="\n/*# sourceMappingURL=data:application/json;base64,"+btoa(unescape(encodeURIComponent(JSON.stringify(o))))+" */");var f=new Blob([t],{type:"text/css"}),a=r.href;r.href=URL.createObjectURL(f),a&&URL.revokeObjectURL(a)}.bind(null,e,n),o=function(){x(e),e.href&&URL.revokeObjectURL(e.href)}):(e=g(n),t=function(r,n){var e=n.css,t=n.media;t&&r.setAttribute("media",t);if(r.styleSheet)r.styleSheet.cssText=e;else{for(;r.firstChild;)r.removeChild(r.firstChild);r.appendChild(document.createTextNode(e))}}.bind(null,e),o=function(){x(e)});return t(r),function(n){if(n){if(n.css===r.css&&n.media===r.media&&n.sourceMap===r.sourceMap)return;t(r=n)}else o()}}r.exports=function(r,n){if("undefined"!=typeof DEBUG&&DEBUG&&"object"!=typeof document)throw new Error("The style-loader cannot be used in a non-browser environment");(n=n||{}).attrs="object"==typeof n.attrs?n.attrs:{},n.singleton||"boolean"==typeof n.singleton||(n.singleton=f()),n.insertInto||(n.insertInto="head"),n.insertAt||(n.insertAt="bottom");var e=b(r,n);return c(e,n),function(r){for(var t=[],o=0;o<e.length;o++){var f=e[o];(a=i[f.id]).refs--,t.push(a)}r&&c(b(r,n),n);for(o=0;o<t.length;o++){var a;if(0===(a=t[o]).refs){for(var d=0;d<a.parts.length;d++)a.parts[d]();delete i[a.id]}}}};var m,v=(m=[],function(r,n){return m[r]=n,m.filter(Boolean).join("\n")});function k(r,n,e,t){var o=e?"":t.css;if(r.styleSheet)r.styleSheet.cssText=v(n,o);else{var i=document.createTextNode(o),f=r.childNodes;f[n]&&r.removeChild(f[n]),f.length?r.insertBefore(i,f[n]):r.appendChild(i)}}},function(r,n){r.exports=function(r){var n="undefined"!=typeof window&&window.location;if(!n)throw new Error("fixUrls requires window.location");if(!r||"string"!=typeof r)return r;var e=n.protocol+"//"+n.host,t=e+n.pathname.replace(/\/[^\/]*$/,"/");return r.replace(/url\s*\(((?:[^)(]|\((?:[^)(]+|\([^)(]*\))*\))*)\)/gi,function(r,n){var o,i=n.trim().replace(/^"(.*)"$/,function(r,n){return n}).replace(/^'(.*)'$/,function(r,n){return n});return/^(#|data:|http:\/\/|https:\/\/|file:\/\/\/|\s*$)/i.test(i)?r:(o=0===i.indexOf("//")?i:0===i.indexOf("/")?e+i:t+i.replace(/^\.\//,""),"url("+JSON.stringify(o)+")")})}},,,,,,,,,,,,,,,,,,,,function(r,n,e){n=r.exports=e(7)(!1);var t=e(8),o=t(e(31)),i=t(e(32)),f=t(e(33)),a=t(e(34)),d=t(e(35));n.push([r.i,"body,\r\nselect {\r\n  font: 10pt sans;\r\n}\r\n\r\n.tgview.jssocials-share-link {\r\n  border-radius: 50%;\r\n}\r\n\r\n.tgview.sidenav {\r\n  -moz-box-shadow: inset 0px 1px 0px 0px #ffffff;\r\n  -webkit-box-shadow: inset 0px 1px 0px 0px #ffffff;\r\n  box-shadow: inset 0px 1px 0px 0px #ffffff;\r\n  background: -webkit-gradient(\r\n    linear,\r\n    left top,\r\n    left bottom,\r\n    color-stop(0.05, #ffffff),\r\n    color-stop(1, #f6f6f6)\r\n  );\r\n  background: -moz-linear-gradient(top, #ffffff 5%, #f6f6f6 100%);\r\n  background: -webkit-linear-gradient(top, #ffffff 5%, #f6f6f6 100%);\r\n  background: -o-linear-gradient(top, #ffffff 5%, #f6f6f6 100%);\r\n  background: -ms-linear-gradient(top, #ffffff 5%, #f6f6f6 100%);\r\n  background: linear-gradient(to bottom, #ffffff 5%, #f6f6f6 100%);\r\n  filter: progid:DXImageTransform.Microsoft.gradient(startColorstr='#ffffff', endColorstr='#f6f6f6',GradientType=0);\r\n  background-color: #ffffff;\r\n  -moz-border-radius: 6px;\r\n  -webkit-border-radius: 6px;\r\n  border-radius: 6px;\r\n  border: 1px solid #dcdcdc;\r\n  display: inline-block;\r\n  color: #666666;\r\n  font-family: Arial;\r\n  font-size: 12px;\r\n  font-weight: bold;\r\n  text-decoration: none;\r\n  text-shadow: 0px 1px 0px #ffffff;\r\n  height: 100%;\r\n  width: 0;\r\n  position: fixed;\r\n  z-index: 1005;\r\n  top: 0;\r\n  left: 0;\r\n  overflow-x: hidden;\r\n  transition: 0.5s;\r\n  padding-top: 40px;\r\n}\r\n\r\n.tgview.sidenav .closebtn {\r\n  position: absolute;\r\n  top: 0;\r\n  right: 16px;\r\n  font-size: 36px;\r\n  margin-left: 50px;\r\n  text-decoration: none;\r\n  font-size: 25px;\r\n  color: #818181;\r\n  display: block;\r\n  transition: 0.3s;\r\n}\r\n\r\n.tgview.sidenav2 {\r\n  -moz-box-shadow: inset 0px 1px 0px 0px #ffffff;\r\n  -webkit-box-shadow: inset 0px 1px 0px 0px #ffffff;\r\n  box-shadow: inset 0px 1px 0px 0px #ffffff;\r\n  background: -webkit-gradient(\r\n    linear,\r\n    left top,\r\n    left bottom,\r\n    color-stop(0.05, #ffffff),\r\n    color-stop(1, #f6f6f6)\r\n  );\r\n  background: -moz-linear-gradient(top, #ffffff 5%, #f6f6f6 100%);\r\n  background: -webkit-linear-gradient(top, #ffffff 5%, #f6f6f6 100%);\r\n  background: -o-linear-gradient(top, #ffffff 5%, #f6f6f6 100%);\r\n  background: -ms-linear-gradient(top, #ffffff 5%, #f6f6f6 100%);\r\n  background: linear-gradient(to bottom, #ffffff 5%, #f6f6f6 100%);\r\n  filter: progid:DXImageTransform.Microsoft.gradient(startColorstr='#ffffff', endColorstr='#f6f6f6',GradientType=0);\r\n  background-color: #ffffff;\r\n  -moz-border-radius: 6px;\r\n  -webkit-border-radius: 6px;\r\n  border-radius: 6px;\r\n  border: 1px solid #dcdcdc;\r\n  display: inline-block;\r\n  color: #666666;\r\n  font-family: Arial;\r\n  font-size: 12px;\r\n  font-weight: bold;\r\n  text-decoration: none;\r\n  text-shadow: 0px 1px 0px #ffffff;\r\n  height: 100%;\r\n  width: 0;\r\n  position: fixed;\r\n  z-index: 1005;\r\n  top: 0;\r\n  right: 0;\r\n  overflow-x: hidden;\r\n  transition: 0.5s;\r\n  padding-top: 40px;\r\n}\r\n\r\n.tgview.sidenav2 .closebtn {\r\n  position: absolute;\r\n  top: 0;\r\n  left: 16px;\r\n  font-size: 36px;\r\n  margin-right: 50px;\r\n  text-decoration: none;\r\n  font-size: 25px;\r\n  color: #818181;\r\n  display: block;\r\n  transition: 0.3s;\r\n}\r\n\r\n.tgview.wholeNetwork {\r\n  position: relative;\r\n  width: 1200px;\r\n  height: 600px;\r\n  border: 1px solid lightgray;\r\n  float: left;\r\n}\r\n\r\ntable.tgview.legend_table {\r\n  font-size: 11px;\r\n  border-width: 1px;\r\n  border-color: #d3d3d3;\r\n  border-style: solid;\r\n}\r\n\r\ntable.tgview.legend_table,\r\ntd {\r\n  border-width: 1px;\r\n  border-color: #d3d3d3;\r\n  border-style: solid;\r\n  padding: 2px;\r\n}\r\n\r\n/*\r\ndiv.table_content {\r\n  width: 80px;\r\n  text-align: center;\r\n}\r\ndiv.table_description {\r\n  width: 100px;\r\n}\r\n*/\r\n.tgview.operation {\r\n  font-size: 28px;\r\n}\r\n\r\n.tgview.edge-operation {\r\n  font-size: 28px;\r\n}\r\n\r\n.tgview.network-popup {\r\n  display: none;\r\n  position: absolute;\r\n  top: 350px;\r\n  left: 170px;\r\n  z-index: 299;\r\n  width: 300px;\r\n  height: 246px;\r\n  background-color: #f9f9f9;\r\n  border-style: solid;\r\n  border-width: 3px;\r\n  border-color: #5394ed;\r\n  padding: 10px;\r\n  text-align: center;\r\n}\r\n\r\n.tgview.network-edge-popUp {\r\n  display: none;\r\n  position: absolute;\r\n  top: 350px;\r\n  left: 170px;\r\n  z-index: 299;\r\n  width: 270px;\r\n  height: 170px;\r\n  background-color: #f9f9f9;\r\n  border-style: solid;\r\n  border-width: 3px;\r\n  border-color: #5394ed;\r\n  padding: 10px;\r\n  text-align: center;\r\n}\r\n\r\nhtml,\r\nbody {\r\n  height: 100%;\r\n}\r\n\r\n.tgview.theoryTreeClass {\r\n  overflow: auto;\r\n  box-shadow: 0 0 5px #ccc;\r\n  padding: 10px;\r\n  width: 370px;\r\n  border-radius: 5px;\r\n  resize: horizontal;\r\n}\r\n\r\n/* The whole context menu */\r\n.tgview.custom-menu {\r\n  display: none;\r\n  z-index: 1000;\r\n  position: absolute;\r\n  overflow: hidden;\r\n  border: 1px solid #ccc;\r\n  white-space: nowrap;\r\n  font-family: sans-serif;\r\n  background: #fff;\r\n  color: #333;\r\n  border-radius: 5px;\r\n  padding: 0;\r\n}\r\n\r\n/* The whole context menu */\r\n.tgview.custom-menu-side {\r\n  display: none;\r\n  z-index: 1000;\r\n  position: absolute;\r\n  overflow: hidden;\r\n  border: 1px solid #ccc;\r\n  white-space: nowrap;\r\n  font-family: sans-serif;\r\n  background: #fff;\r\n  color: #333;\r\n  border-radius: 5px;\r\n  padding: 0;\r\n}\r\n\r\n.tgview.custom-tooltip {\r\n  display: none;\r\n  z-index: 1000;\r\n  position: absolute;\r\n  overflow: hidden;\r\n  border: 1px solid #ccc;\r\n  font-family: sans-serif;\r\n  background: #fff;\r\n  color: #333;\r\n  border-radius: 5px;\r\n  padding: 4px;\r\n}\r\n/* Each of the context menu items in the list */\r\n.tgview.custom-menu-side li {\r\n  padding: 8px 12px;\r\n  cursor: pointer;\r\n  list-style-type: none;\r\n  transition: all 0.3s ease;\r\n}\r\n\r\n.tgview.custom-menu-side li:hover {\r\n  background-color: #def;\r\n}\r\n\r\n/* Each of the context menu items in the list */\r\n.tgview.custom-menu li {\r\n  padding: 8px 12px;\r\n  cursor: pointer;\r\n  list-style-type: none;\r\n  transition: all 0.3s ease;\r\n}\r\n\r\n.tgview.custom-menu li:hover {\r\n  background-color: #def;\r\n}\r\n\r\n/* UNUSED?\r\n.dropdown-menu {\r\n  position: absolute;\r\n  top: 100%;\r\n  left: 0px;\r\n  z-index: 1000;\r\n  display: none;\r\n  float: left;\r\n  min-width: 160px;\r\n  padding: 5px 0px;\r\n  margin: 2px 0px 0px;\r\n  list-style: outside none none;\r\n  background-color: #fff;\r\n  border: 1px solid rgba(0, 0, 0, 0.2);\r\n  border-radius: 6px;\r\n  box-shadow: 0px 5px 10px rgba(0, 0, 0, 0.2);\r\n  background-clip: padding-box;\r\n}\r\n*/\r\n.tgview.colorPicker {\r\n  margin-left: 8px;\r\n  float: left;\r\n}\r\n\r\n.tgview.generalMenu {\r\n  margin-top: 2px;\r\n  float: left;\r\n}\r\n\r\n.tgview.toolSelector {\r\n  width: 370px;\r\n  top: 0px;\r\n  position: relative;\r\n  -moz-box-shadow: inset 0px 1px 0px 0px #ffffff;\r\n  -webkit-box-shadow: inset 0px 1px 0px 0px #ffffff;\r\n  box-shadow: inset 0px 1px 0px 0px #ffffff;\r\n  background: -webkit-gradient(\r\n    linear,\r\n    left top,\r\n    left bottom,\r\n    color-stop(0.05, #ffffff),\r\n    color-stop(1, #f6f6f6)\r\n  );\r\n  background: -moz-linear-gradient(top, #ffffff 5%, #f6f6f6 100%);\r\n  background: -webkit-linear-gradient(top, #ffffff 5%, #f6f6f6 100%);\r\n  background: -o-linear-gradient(top, #ffffff 5%, #f6f6f6 100%);\r\n  background: -ms-linear-gradient(top, #ffffff 5%, #f6f6f6 100%);\r\n  background: linear-gradient(to bottom, #ffffff 5%, #f6f6f6 100%);\r\n  filter: progid:DXImageTransform.Microsoft.gradient(startColorstr='#ffffff', endColorstr='#f6f6f6',GradientType=0);\r\n  background-color: #ffffff;\r\n  -moz-border-radius: 6px;\r\n  -webkit-border-radius: 6px;\r\n  border-radius: 6px;\r\n  border: 1px solid #dcdcdc;\r\n  display: inline-block;\r\n  color: #666666;\r\n  font-family: Arial;\r\n  font-size: 12px;\r\n  font-weight: bold;\r\n  padding: 6px 24px;\r\n  text-decoration: none;\r\n  text-shadow: 0px 1px 0px #ffffff;\r\n  float: left;\r\n}\r\n\r\n.tgview.colorRect {\r\n  width: 24px;\r\n  height: 24px;\r\n  margin-right: 4px;\r\n  margin-top: 2px;\r\n  background-color: gray;\r\n  float: left;\r\n  cursor: pointer;\r\n  border: 1px solid #dcdcdc;\r\n}\r\n\r\n.tgview.standardIcon {\r\n  cursor: pointer;\r\n  background-image: url("+o+");\r\n  width: 24px;\r\n  height: 24px;\r\n  background-size: 18px 18px;\r\n  background-repeat: no-repeat;\r\n}\r\n\r\n.tgview.downloadIcon {\r\n  cursor: pointer;\r\n  background-image: url("+i+");\r\n  width: 24px;\r\n  height: 24px;\r\n  background-size: 18px 18px;\r\n  background-repeat: no-repeat;\r\n}\r\n\r\n.tgview.crosshairIcon {\r\n  cursor: pointer;\r\n  background-image: url("+f+");\r\n  width: 24px;\r\n  height: 24px;\r\n  background-size: 18px 18px;\r\n  background-repeat: no-repeat;\r\n}\r\n\r\n.tgview.undoIcon {\r\n  cursor: pointer;\r\n  background-image: url("+a+");\r\n  width: 24px;\r\n  height: 24px;\r\n  background-size: 18px 18px;\r\n  background-repeat: no-repeat;\r\n}\r\n\r\n.tgview.redoIcon {\r\n  cursor: pointer;\r\n  background-image: url("+d+");\r\n  width: 24px;\r\n  height: 24px;\r\n  background-size: 18px 18px;\r\n  background-repeat: no-repeat;\r\n}\r\n\r\n.tgview.myButton {\r\n  -moz-box-shadow: inset 0px 1px 0px 0px #dcecfb;\r\n  -webkit-box-shadow: inset 0px 1px 0px 0px #dcecfb;\r\n  box-shadow: inset 0px 1px 0px 0px #dcecfb;\r\n  background: -webkit-gradient(\r\n    linear,\r\n    left top,\r\n    left bottom,\r\n    color-stop(0.05, #bddbfa),\r\n    color-stop(1, #80b5ea)\r\n  );\r\n  background: -moz-linear-gradient(top, #bddbfa 5%, #80b5ea 100%);\r\n  background: -webkit-linear-gradient(top, #bddbfa 5%, #80b5ea 100%);\r\n  background: -o-linear-gradient(top, #bddbfa 5%, #80b5ea 100%);\r\n  background: -ms-linear-gradient(top, #bddbfa 5%, #80b5ea 100%);\r\n  background: linear-gradient(to bottom, #bddbfa 5%, #80b5ea 100%);\r\n  filter: progid:DXImageTransform.Microsoft.gradient(startColorstr='#bddbfa', endColorstr='#80b5ea',GradientType=0);\r\n  background-color: #bddbfa;\r\n  -moz-border-radius: 6px;\r\n  -webkit-border-radius: 6px;\r\n  border-radius: 6px;\r\n  border: 1px solid #84bbf3;\r\n  display: inline-block;\r\n  cursor: pointer;\r\n  font-family: Arial;\r\n  font-size: 15px;\r\n  text-decoration: none;\r\n  text-shadow: 0px 1px 0px #528ecc;\r\n\r\n  margin-top: 18px;\r\n}\r\n.tgview.myButton:hover {\r\n  background: -webkit-gradient(\r\n    linear,\r\n    left top,\r\n    left bottom,\r\n    color-stop(0.05, #80b5ea),\r\n    color-stop(1, #bddbfa)\r\n  );\r\n  background: -moz-linear-gradient(top, #80b5ea 5%, #bddbfa 100%);\r\n  background: -webkit-linear-gradient(top, #80b5ea 5%, #bddbfa 100%);\r\n  background: -o-linear-gradient(top, #80b5ea 5%, #bddbfa 100%);\r\n  background: -ms-linear-gradient(top, #80b5ea 5%, #bddbfa 100%);\r\n  background: linear-gradient(to bottom, #80b5ea 5%, #bddbfa 100%);\r\n  filter: progid:DXImageTransform.Microsoft.gradient(startColorstr='#80b5ea', endColorstr='#bddbfa',GradientType=0);\r\n  background-color: #80b5ea;\r\n}\r\n",""])},function(r,n,e){r.exports=e.p+"ba17fb9507e9abe72c02639ec16a195f.png"},function(r,n,e){r.exports=e.p+"4a15518005b67b7a488e00004a9bc62f.png"},function(r,n,e){r.exports=e.p+"9372cbb3035ec25fe753287eb0b52856.png"},function(r,n,e){r.exports=e.p+"b1c3e6026c77195167671ed2ff798785.png"},function(r,n,e){r.exports=e.p+"da47302b90d5015eacd4f0f44346ce10.png"}]]);