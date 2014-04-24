/* -*- Mode: C; c-basic-offset: 4 -*- 
 * Copyright (c) 2007 Micah Dowty <micah@navi.cx>
 *
 * Continuously updating IRC Bot status information.
 */

var BotStatus = {};

BotStatus.init = function(url)
{
    this.div_loading = document.getElementById("bot-status-loading");
    this.div_message = document.getElementById("bot-status-message");
    this.url = url;
    this.updateData();
};

BotStatus.showBotStatus = function(obj)
{
    if (!obj.request || obj.request.botnick == "???")
    {
	/*
	 * There is no request, or the request is marked inactive.
	 * Don't bother updating until the user submits a bot request.
	 */
	this.div_message.innerHTML = "<p>Inactive.</p>";
	return;
    }

    if (obj.request.botnick == "---")
    {
	/*
	 * There is a request, but it's currently unfulfilled. 
	 * Reload pretty frequently, since the bot should be
	 * connecting Real Soon Now.
	 */
	this.div_message.innerHTML = "<p>Waiting for bots to connect...</p>";
	this.scheduleDataUpdate(5);
	return;
    }

    var status = "<p>Ready.</p>"

    status += "<p>Your bot is <strong>" + obj.request.botnick +
    "</strong> seeing " + obj.request.user_count + " users.</p>";

    /*
     * Update relatively slowly, since the bot is functioning normally
     * and nothing will change much.
     */
    this.div_message.innerHTML = status;
    this.scheduleDataUpdate(60);
};

BotStatus.showError = function(code)
{
    this.div_message.innerHTML = "<p>Error checking bot status! (" + code + ")</p>";

    /*
     * We'll automatically retry if this appears to be a server error,
     * or if it was a network error while contacting the server.  It
     * may be something transient, like a daemon restart or an
     * overloaded proxy.
     */
    if (code <= 0 || code >= 500) {
	this.scheduleDataUpdate(60);
    }
};

BotStatus.scheduleDataUpdate = function(seconds)
{
    var self = this;
    self.timer = setTimeout(function() { 
	self.timer = null;
	self.updateData();
    }, seconds * 1000);
};
    
BotStatus.updateData = function()
{
    var self = this;

    self.div_loading.style.display = "block";
    
    var responseSuccess = function(req) {
	self.div_loading.style.display = "none";
	self.showBotStatus(parseJSON(req.responseText));
    }
    
    var responseFailure = function(req) {
	self.div_loading.style.display = "none";
	self.showError(req.status);
    }
    
    var callback = {
	success: responseSuccess,
	failure: responseFailure,
	timeout: 15000
    };
    
    self._request = YAHOO.util.Connect.asyncRequest('GET', this.url, callback);
};
