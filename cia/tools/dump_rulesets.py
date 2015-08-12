#!/usr/bin/env python
"""
Dump all rulesets on the server to stdout, in a
format compatible with load_rulesets.py
"""

from cia.tools import Client


class RulesetDumper(Client.App):
    def main(self):
	print "<rulesets>"
        for ruleset in self.server.ruleset.getRulesetMap().values():
            print ruleset
#            print "\n%s" % ruleset.split("?>", 1)[1].strip()
        print "\n</rulesets>"

if __name__ == '__main__':
    RulesetDumper().main()

### The End ###
