/* iframe.js */
/* Copyright (c) 2007 Micah Dowty <micah@navi.cx> */
var iFrameResultCallbacks={}
iFrameLoaded=function(childDocument,childWindow)
{var cd=childDocument;var h=0;if(cd.height)h=Math.max(h,cd.height);if(cd.body){if(cd.body.offsetHeight)h=Math.max(h,cd.body.offsetHeight);if(cd.body.scrollHeight)h=Math.max(h,cd.body.scrollHeight);}
if(h<10){h=150;}
childWindow.frameElement.style.height=h+"px";var childResult=childDocument.getElementById("form-result");if(childResult){var parentResult=document.getElementById(childWindow.name+"_result");if(parentResult){parentResult.value=childResult.value;}
var handler=iFrameResultCallbacks[childWindow.name];if(handler){handler(childResult.value);}}}

