#!/usr/bin/env python
"""
A client providing easy access to common debugging features
"""

from LibCIA import Client

class Options(Client.Options):
    optFlags = [
        ['type-profile', 't', 'Show a chart profiling which data types have the most instances'],
        ]

    optParameters = [
        ['rebuild', 'r', None, 'Rebuild one specific package or module'],
        ['eval', 'e', None, 'Evaluate arbitrary code in the context of the Debug module'],
        ]

class DebugTool(Client.App):
    optionsClass = Options

    def main(self):
        if self.config['rebuild']:
            self.server.debug.rebuild(self.config['rebuild'], self.key)
        if self.config['type-profile']:
            print self.server.debug.gc.typeProfile(self.key)
        if self.config['eval']:
            print self.server.debug.eval(self.config['eval'], self.key)

if __name__ == '__main__':
    DebugTool().main()

### The End ###
