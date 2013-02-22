#!/usr/bin/python

import os

f = os.popen("git log -n 1 && git status -s")

boringstring = "";
fullstring = "";

for line in f:
    boringstring += line

maxlen = 2048

if  len(boringstring) > maxlen:
     print "WARNING:  Truncating info from git";
     boringstring = boringstring[0:maxlen-1]

for x in boringstring:
    fullstring += '\\x'+x.encode('hex')


     

newheadertext = """#ifndef __GITINFO_HH
#define __GITINFO_HH

/*
    Generated automatically by cmake process
    Encoding:
-------------------------------------------------------------
""" + boringstring + """
-------------------------------------------------------------
*/

#define __GITMAXINFO_SIZE 2048

#define gGitInfoStr \"""" + fullstring + '\"' \
+ \
"""

#endif//__GITINFO_HH"""

print newheadertext

#newheader = open( "include/gitinfo.hh", "w")
#newheader.write(newheadertext)
#newheader.close()
