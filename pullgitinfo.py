#!/usr/bin/python

import os
import sys

# run the script from the remoll directory

presentcwd = os.getcwd()
os.chdir(presentcwd)

f = os.popen("git log -n 1 && git status -bs && echo \"\nGeant4 version\" `geant4-config --version` && echo \"  ROOT version\" `root-config --version` && echo \" `cmake --version`\" && echo \"\nGenerated at `date`\"")

boringstring = "";

if( f != 0):
	for line in f:
		boringstring += line
else:
	boringstring = "git information unavailable"

boringstring += "Source dir " + os.getcwd()
boringstring += "\nBuild  dir " + presentcwd + "\n"

newheadertext = """#ifndef __GITINFO_HH
#define __GITINFO_HH

/*
    Generated automatically by cmake process
*/

const char* gGitInfo = R\"gitinfo(
""" + boringstring + """
)gitinfo\";

#endif//__GITINFO_HH
"""


os.chdir(presentcwd)

outdir = "include/"
if not os.path.exists(outdir):
	os.makedirs(outdir)

newheader = open( outdir + "/gitinfo.hh", "w")
newheader.write(newheadertext)
newheader.close()

print( "Repository information\n", boringstring)
