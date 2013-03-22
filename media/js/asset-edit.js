/* attachevent.js */
/* This code is copyright 2003 by Gavin Kistner, gavin@refinery.com */
/* It is covered under the license viewable at http://phrogz.net/JS/_ReuseLicense.txt */
/* Reuse or modification is free provided you abide by the terms of that license. */
function AttachEvent(obj,evt,fnc,useCapture){if(!useCapture)useCapture=false;if(obj.addEventListener){obj.addEventListener(evt,fnc,useCapture);return true;}else if(obj.attachEvent)return obj.attachEvent("on"+evt,fnc);else{MyAttachEvent(obj,evt,fnc);obj['on'+evt]=function(){MyFireEvent(obj,evt)};}}
function MyAttachEvent(obj,evt,fnc){if(!obj.myEvents)obj.myEvents={};if(!obj.myEvents[evt])obj.myEvents[evt]=[];var evts=obj.myEvents[evt];evts[evts.length]=fnc;}
function MyFireEvent(obj,evt){if(!obj||!obj.myEvents||!obj.myEvents[evt])return;var evts=obj.myEvents[evt];for(var i=0,len=evts.length;i<len;i++)evts[i]();}

/* addclasskillclass.js */
/* This code is copyright 2002-2003 by Gavin Kistner and Refinery; www.refinery.com */
/* It is covered under the license viewable at http://phrogz.net/JS/_ReuseLicense.txt */
/* Reuse or modification is free provided you abide by the terms of that license. */
function AddClass(obj,cName){KillClass(obj,cName);return obj&&(obj.className+=(obj.className.length>0?' ':'')+cName);}
function KillClass(obj,cName){return obj&&(obj.className=obj.className.replace(new RegExp("^"+cName+"\\b\\s*|\\s*\\b"+cName+"\\b",'g'),''));}
function HasClass(obj,cName){return(!obj||!obj.className)?false:(new RegExp("\\b"+cName+"\\b")).test(obj.className)}

/* addcss.js */
/* This code is copyright 2002-2003 by Gavin Kistner, gavin@refinery.com */
/* It is covered under the license viewable at http://phrogz.net/JS/_ReuseLicense.txt */
/* Reuse or modification is free provided you abide by the terms of that license. */
function AddStyleSheet(url,idx){var css,before=null,head=document.getElementsByTagName("head")[0];if(document.createElement){if(url){css=document.createElement('link');css.rel='stylesheet';css.href=url;}else css=document.createElement('style');css.media='all';css.type='text/css';if(idx>=0){for(var i=0,ct=0,len=head.childNodes.length;i<len;i++){var el=head.childNodes[i];if(!el.tagName)continue;var tagName=el.tagName.toLowerCase();if(ct==idx){before=el;break;}
if(tagName=='style'||tagName=='link'&&(el.rel&&el.rel.toLowerCase()=='stylesheet'||el.type&&el.type.toLowerCase()=='text/css'))ct++;}}
head.insertBefore(css,before);return document.styleSheets[before?idx:document.styleSheets.length-1];}else return alert("I can't create a new stylesheet for you. Sorry.");}
function AddRule(ss,selector,styles){if(!ss)return false;if(ss.insertRule)return ss.insertRule(selector+' {'+styles+'}',ss.cssRules.length);if(ss.addRule){ss.addRule(selector,styles);return true;}
return false;}

/* tabtastic.js */
/* This library is copyright 2004 by Gavin Kistner, gavin@refinery.com */
/* It is covered under the license viewable at http://phrogz.net/JS/_ReuseLicense.txt */
/* Reuse or modification is free provided you abide by the terms of that license. */
var TabtasticInit=function(){var tocTag='ul',tocClass='tabset_tabs',tabTag='a',contentClass='tabset_content';function FindEl(tagName,evt){if(!evt&&window.event)evt=event;if(!evt)return DebugOut("Can't find an event to handle in DLTabSet::SetTab",0);var el=evt.currentTarget||evt.srcElement;while(el&&(!el.tagName||el.tagName.toLowerCase()!=tagName))el=el.parentNode;return el;}
function SetTabActive(tab){if(tab.tabTOC.activeTab){if(tab.tabTOC.activeTab==tab)return;KillClass(tab.tabTOC.activeTab,'active');if(tab.tabTOC.activeTab.tabContent)KillClass(tab.tabTOC.activeTab.tabContent,'tabset_content_active');if(tab.tabTOC.activeTab.prevTab)KillClass(tab.tabTOC.activeTab.previousTab,'preActive');if(tab.tabTOC.activeTab.nextTab)KillClass(tab.tabTOC.activeTab.nextTab,'postActive');}
AddClass(tab.tabTOC.activeTab=tab,'active');if(tab.tabContent)AddClass(tab.tabContent,'tabset_content_active');if(tab.prevTab)AddClass(tab.prevTab,'preActive');if(tab.nextTab)AddClass(tab.nextTab,'postActive');}
function SetTabFromAnchor(evt){SetTabActive(FindEl('a',evt).semanticTab);}
function Init(){window.everyTabThereIsById={};var anchorMatch=/#([a-z][\w.:-]*)$/i,match;var activeTabs=[];var tocs=document.getElementsByTagName(tocTag);for(var i=0,len=tocs.length;i<len;i++){var toc=tocs[i];if(!HasClass(toc,tocClass))continue;var lastTab;var tabs=toc.getElementsByTagName(tabTag);for(var j=0,len2=tabs.length;j<len2;j++){var tab=tabs[j];if(!tab.href||!(match=anchorMatch.exec(tab.href)))continue;if(lastTab){tab.prevTab=lastTab;lastTab.nextTab=tab;}
tab.tabTOC=toc;everyTabThereIsById[tab.tabID=match[1]]=tab;tab.tabContent=document.getElementById(tab.tabID);if(HasClass(tab,'active'))activeTabs[activeTabs.length]=tab;lastTab=tab;}
AddClass(toc.getElementsByTagName('li')[0],'firstchild');}
for(var i=0,len=activeTabs.length;i<len;i++){SetTabActive(activeTabs[i]);}
for(var i=0,len=document.links.length;i<len;i++){var a=document.links[i];if(!(match=anchorMatch.exec(a.href)))continue;if(a.semanticTab=everyTabThereIsById[match[1]]){AttachEvent(a,'click',SetTabFromAnchor,false);a.tabHref=a.href;a.href="javascript:void(0)";}}
if((match=anchorMatch.exec(location.href))&&(a=everyTabThereIsById[match[1]]))SetTabActive(a);AddStyleSheet('/media/css/tabtastic.css',0);}
Init();};

/* preserve-tab.js */
/* Copyright (c) 2003-2007 Micah Dowty <micah@navi.cx> */
var preserveCurrentTab=function(formId,tabsId)
{var form=document.getElementById(formId);var tabs=document.getElementById(tabsId);var origAction=form.action;AttachEvent(form,"submit",function(){if(tabs.activeTab){form.action=tabs.activeTab.tabHref;}});};

/* change-history.js */
/* Copyright (c) 2007 Micah Dowty <micah@navi.cx> */
var ChangeHistory={};ChangeHistory.init=function(url,changesetListId,loadingId)
{this.url=url;this.changesetList=document.getElementById("changesets");this.loading=document.getElementById("changes-loading");this.moreChanges=document.getElementById("more-changes");this.moreChangesCount=document.getElementById("more-changes-count");this.nextPage=0;this.changesetList.innerHTML='';this.moreChanges.style.display="none";this.loadNextPage();}
ChangeHistory.loadNextPage=function()
{var self=this;if(self.request){return;}
self.loading.style.display="block";var responseSuccess=function(req){self.loading.style.display="none";self.request=null;self.nextPage+=1;var obj=parseJSON(req.responseText);if(obj.remaining){self.moreChangesCount.innerHTML=obj.remaining;self.moreChanges.style.display="block";}else{self.moreChanges.style.display="none";}
self.changesetList.innerHTML+=obj.html;}
var responseFailure=function(req){self.loading.style.display="none";self.request=null;self.changesetList.innerHTML+="<li>Connection error while retrieving changes ("+req.status+")</li>";}
var callback={success:responseSuccess,failure:responseFailure,timeout:15000};self.request=YAHOO.util.Connect.asyncRequest('GET',this.url+'page'+self.nextPage+'/',callback)};

/* iframe.js */
/* Copyright (c) 2007 Micah Dowty <micah@navi.cx> */
var iFrameResultCallbacks={}
iFrameLoaded=function(childDocument,childWindow)
{var cd=childDocument;var h=0;if(cd.height)h=Math.max(h,cd.height);if(cd.body){if(cd.body.offsetHeight)h=Math.max(h,cd.body.offsetHeight);if(cd.body.scrollHeight)h=Math.max(h,cd.body.scrollHeight);}
if(h<10){h=150;}
childWindow.frameElement.style.height=h+"px";var childResult=childDocument.getElementById("form-result");if(childResult){var parentResult=document.getElementById(childWindow.name+"_result");if(parentResult){parentResult.value=childResult.value;}
var handler=iFrameResultCallbacks[childWindow.name];if(handler){handler(childResult.value);}}}

/* autohide.js */
/* Copyright (c) 2003-2007 Micah Dowty <micah@navi.cx> */
var autohideWithCheckbox=function(checkboxId,targetId)
{var checkbox=document.getElementById(checkboxId);var target=document.getElementById(targetId);checkbox.onchange=function(){target.style.display=checkbox.checked?'block':'none';};checkbox.onchange();};var autohideWithLink=function(linkId,targetId,def)
{var link=document.getElementById(linkId);var target=document.getElementById(targetId);var visible=def;var update=function(){target.style.display=visible?'block':'none';};update();link.onclick=function(){visible=!visible;update();};};

