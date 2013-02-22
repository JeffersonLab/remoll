#!/usr/bin/python

import os

f = os.popen("git log -n 1 && git status -s")

boringstring = "";
fullstring = "";
nchar = 1

for line in f:
    boringstring += line
    thisstring = ""
    for x in line:
	thisstring += '\\x'+x.encode('hex')
	nchar += 1
#    print thisstring
    fullstring += thisstring;


newheadertext = """#ifndef __GITINFO_HH
#define __GITINFO_HH

/*
    Generated automatically by cmake process
    Encoding:
-------------------------------------------------------------
""" + boringstring + """
-------------------------------------------------------------
*/

char gGitInfoStr[""" + str(nchar) + "] = \"" + fullstring + '\";' \
+ \
"""

#endif//__GITINFO_HH"""

print newheadertext

#open( "include/gitinfo.hh", 
