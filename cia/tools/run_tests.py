#!/usr/bin/env python
#
# Runs doctests for all applicable CIA modules
#

import sys;

sys.path[0] += '/..'
import doctest

import cia.LibCIA.Web.RegexTransform
doctest.testmod(cia.LibCIA.Web.RegexTransform)

import cia.LibCIA.IRC.Formatting
doctest.testmod(cia.LibCIA.IRC.Formatting)

doctest.testmod(cia.LibCIA.Database)

import cia.LibCIA.TimeUtil
doctest.testmod(cia.LibCIA.TimeUtil)

import cia.LibCIA.Ruleset
doctest.testmod(cia.LibCIA.Ruleset)

import cia.LibCIA.ColorText
doctest.testmod(cia.LibCIA.ColorText)

import cia.LibCIA.Message
doctest.testmod(cia.LibCIA.Message)

### The End ###
