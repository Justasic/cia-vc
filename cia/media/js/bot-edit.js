/* on-text-changed.js */
/* Copyright (c) 2003-2007 Micah Dowty <micah@navi.cx> */
var attachOnTextareaChanged=function(obj,handler)
{AttachEvent(obj,"change",handler);AttachEvent(obj,"keypress",function(e){if(e.keyCode!=9){handler(e);}});};var setCheckboxOnTextChanged=function(checkboxId,textareaId){var checkbox=document.getElementById(checkboxId);var textarea=document.getElementById(textareaId);attachOnTextareaChanged(textarea,function(){checkbox.checked=true;});};

/* units.js */
/* Copyright (c) 2003-2007 Micah Dowty <micah@navi.cx> */
var UnitCollection=function(){};UnitCollection.prototype.units=[];UnitCollection.prototype.threshold=0.8;UnitCollection.prototype.precision=2;UnitCollection.prototype.format=function(value)
{var i,singular,plural,multipler,converted;for(i in this.units){singular=this.units[i][0];plural=this.units[i][1];multiplier=this.units[i][2];converted=value/multiplier;if(converted>this.threshold){break;}}
var s=converted.toFixed(this.precision);s=s.replace(/\.0*$/,"");var unit=plural;if(s=="1"){unit=singular;}
return s+' '+unit;};var TimeUnits=new UnitCollection();TimeUnits.units=[["year","years",365*24*60*60],["month","months",30*24*60*60],["week","weeks",7*24*60*60],["day","days",24*60*60],["hour","hours",60*60],["minute","minutes",60],["second","seconds",1],["millisecond","milliseconds",0.001],["microsecond","microseconds",0.000001],];var TimeUnitsAbbrev=new UnitCollection();TimeUnitsAbbrev.units=[["yr","yr",365*24*60*60],["mth","mth",30*24*60*60],["wk","wk",7*24*60*60],["d","d",24*60*60],["hr","hr",60*60],["min","min",60],["sec","sec",1],["ms","ms",0.001],["us","us",0.000001],];var StorageUnits=new UnitCollection();StorageUnits.units=[["TB","TB",1024*1024*1024*1024],["GB","GB",1024*1024*1024],["MB","MB",1024*1024],["kB","kB",1024],["byte","bytes",1],];

/* bot-status.js */
/* Copyright (c) 2007 Micah Dowty <micah@navi.cx> */
var BotStatus={};BotStatus.init=function(url)
{this.div_loading=document.getElementById("bot-status-loading");this.div_message=document.getElementById("bot-status-message");this.url=url;this.updateData();};BotStatus.showBotStatus=function(obj)
{if(!obj.request||obj.request.botnick=="???")
{this.div_message.innerHTML="<p>Inactive.</p>";return;}
if(obj.request.botnick=="---")
{this.div_message.innerHTML="<p>Waiting for bots to connect...</p>";this.scheduleDataUpdate(5);return;}
var status="<p>Ready.</p>"
status+="<p>Your bot is <strong>"+obj.request.botnick+"</strong> seeing "+obj.request.user_count+" users.</p>";this.div_message.innerHTML=status;this.scheduleDataUpdate(60);};BotStatus.showError=function(code)
{this.div_message.innerHTML="<p>Error checking bot status! ("+code+")</p>";if(code<=0||code>=500){this.scheduleDataUpdate(60);}};BotStatus.scheduleDataUpdate=function(seconds)
{var self=this;self.timer=setTimeout(function(){self.timer=null;self.updateData();},seconds*1000);};BotStatus.updateData=function()
{var self=this;self.div_loading.style.display="block";var responseSuccess=function(req){self.div_loading.style.display="none";self.showBotStatus(parseJSON(req.responseText));}
var responseFailure=function(req){self.div_loading.style.display="none";self.showError(req.status);}
var callback={success:responseSuccess,failure:responseFailure,timeout:15000};self._request=YAHOO.util.Connect.asyncRequest('GET',this.url,callback);};

/* validator.js */
/* Copyright (c) 2007 Micah Dowty <micah@navi.cx> */
var Validator={};Validator.editTimeout=2000.0;Validator.init=function(url,validMessage,fieldId,statusId,loadingId)
{this.url=url;this.validMessage=validMessage;this.field=document.getElementById(fieldId);this.status=document.getElementById(statusId);this.loading=document.getElementById(loadingId);this._oldValue=this.field.value;var self=this;attachOnTextareaChanged(this.field,function(){setTimeout(function(){self.testForChanges();},0);});}
Validator.testForChanges=function()
{if(this._oldValue==this.field.value){return;}
this._oldValue=this.field.value;if(this.timer){clearTimeout(this.timer);this.timer=null;}else{if(this.request){YAHOO.util.Connect.abort(this.request,null,false);this.loading.style.display="none";this.request=null;}
this.show("&nbsp;");}
var self=this;this.timer=setTimeout(function(){self.timer=null;self.updateData();},this.editTimeout);};Validator.show=function(message,is_error)
{if(is_error){AddClass(this.status,"error");}else{KillClass(this.status,"error");}
this.status.innerHTML=message;};Validator.updateData=function()
{var self=this;self.loading.style.display="block";var responseSuccess=function(req){self.loading.style.display="none";self.request=null;var obj=parseJSON(req.responseText);if(obj.is_valid){self.show(self.validMessage);}else{self.show(htmlEscape(obj.messages[0]),true);}}
var responseFailure=function(req){self.loading.style.display="none";self.request=null;self.show("Connection error during validation ("+req.status+")");}
var callback={success:responseSuccess,failure:responseFailure,timeout:5000};self.request=YAHOO.util.Connect.asyncRequest('POST',this.url,callback,'content='+escape(this.field.value));};

