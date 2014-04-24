/* form-state.js */
/* Copyright (c) 2003-2007 Micah Dowty <micah@navi.cx> */
var formStateToLink=function(link,form)
{if(!link.originalHref){link.originalHref=link.href;}
var href=link.originalHref;var url_parts=href.match(/([^?]*)(\?([^#]*))?(#.*)?/);var path=url_parts[1];var query_array=(url_parts[3]||"").split("&");var fragment=url_parts[4]||"";var query_dict={};for(var i in query_array){var key_value=query_array[i].split("=");if(key_value[0]){query_dict["~"+key_value[0]]=key_value[1]||"";}}
for(var name in form){var obj=form[name];if(name&&obj&&obj.type=='text'&&obj.value){query_dict["~"+name]=(''+obj.value).replace(/%/g,"%25").replace(/&/g,"%26").replace(/#/g,"%23").replace(/;/g,"%3B").replace(/\+/g,"%2B").replace(/\?/g,"%3F").replace(/\//g,"%2F");}}
var args=[];for(var name in query_dict){if(name&&name[0]=="~"){args.push(name.substr(1)+"="+query_dict[name]);}}
link.href=path+"?"+args.join("&")+fragment;};

