import requests
from cia.LibCIA import Formatters


class DiscordRuleset:
	"""
	This is a really simple Discord rulset, this will be expanded
	in the future to include better messages and formattings but
	for now this is a way that CIA can actually handle discord.
	Eventually this can be stored in an account of some kind
	"""
	WEBHOOK_URL = "https://canary.discordapp.com/api/webhooks/535680915402129418/gYkIl4DZlTcxsOf96a1crlzPOgD9tZVxSArlw7XGnFcQYKqGrnohK_we6FS9j54R3BYw"


	def deliver(self, msg):

		message = Formatters.getFactory().findMedium('plaintext', msg).formatMessage(msg)

		content = {
			"content": message
		}
		requests.post(self.WEBHOOK_URL, json=content)
