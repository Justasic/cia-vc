#!/usr/bin/env python
"""
A utility to set the value of a metadata key for the given stats path.
This can be used to affect the presentation of a particular path in the
stats browser. For example:

./stats_metadata -k stats.key project/pigomatic title 'Pig-O-Matic (tm)'
"""

from twisted.python import usage
from LibCIA import Client
import os

class Options(Client.Options):
    def getSynopsis(self):
        return Client.Options.getSynopsis(self) + ' path key value'

    def parseArgs(self, *args):
        if len(args) == 3:
            self['path'], self['dataKey'], self['dataValue'] = args
        else:
            self.opt_help()

class MetadataTool(Client.App):
    optionsClass = Options

    def main(self):
        self.server.stats.updateMetadata(self.config['path'],
                                         {self.config['dataKey']:
                                          self.config['dataValue']},
                                         self.key)

if __name__ == '__main__':
    MetadataTool().main()

### The End ###
