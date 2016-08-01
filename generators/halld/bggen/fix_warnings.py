#!/usr/bin/python
#
# Dec. 18, 2013  David Lawrence
#
#  This script is an attempt to automatically fix the 
# over 500 warnings emitted when compiling bggen. These
# fill the nightly build messages and obscure any issues
# with our code. It was not completely successful in that
# numerous warnings still remain. Some of them are not
# easily fixed. I'm adding this script here in case it 
# is useful as a starting point for someone else trying
# to fix the issues.
#
# To use this, first create a file with warning messages
# by running make or scons -u. I did this on ifarm1102 using
# gfortran 4.4.6 so other compilers might give differently
# formatted warnings which would cause this script to fail.
#
#    scons -u > scons.out
#
#         or
#
#    make > scons.txt
#
# (I actually only tried it with the scons method)
# It's also worth nothing that I think I only ran this after
# having built everything once, then changed the pythia_h.F
# file so it was the only one contributing warnings. If
# all files are compiled, then one should modify this to
# only consider changes to the pythia_h.F file.
#
# Next, us this script to create a new pythia_h.F
# file. This should be run from the bggen directory
# (not bggen/code). 
#
# ./fix_warnings.py
#
# This will use the scons.out and code/pythia_h.F files to
# generate a new pythia_h.F file in the current directory.
# (Therefore, you should probably not run this from the "code"
# directory.
#
# When it is done, move the new pythia_h.F file into the
# code directory, replcing the existing one:
#
#  mv pythia_h.F code
#
# 

from collections import deque

# Read in entire pythia_h.F file
f = open('code/pythia_h.F', 'r')
infile = f.read().split('\n')
f.close()

# Replace any tabs with spaces
for i in range(1, len(infile)):
	infile[i] = infile[i].replace('\t', '        ')

f = open('scons.out', 'r')
prev= deque(['','','','',''])
for line in f:
	prev.popleft()
	prev.append(line)
	
	if 'Warning: Unused variable' in line:
		first = line.find("'")+1
		last = line.find("'", first)
		var = line[first:last].upper()
		
		first = prev[0].find('bggen/code/') + 11
		last = prev[0].find(':', first)
		fname = prev[0][first:last]
		
		first = last + 1
		last = prev[0].find('.', first)
		line_num = int(prev[0][first:last])-1
		
		print "var=%s in %s at line %d" % (var, fname, line_num)
		

		# Copy line of interest to working variable
		s = infile[line_num]
		
		# Remember if the line ends with a comma 
		ends_with_comma = s.endswith(',')

		# if variable was array, we need to cut the "(XXX)" out too
		first = s.find(var)
		last = first + len(var)
		if last<len(s):
			if s[last:last+1] == '(':
				last = s.find(')', first)

		# Remove variable name (including any array dimensions)
		s = s[:first] + s[last+1:]
		
		# Remove double commas
		s = s.replace(',,',',').replace(', ,',',')

		# Chop off any trailing commas at end of line but only if they
		# were not there before
		if(s.endswith(',')):
			if(not ends_with_comma): s = s[:-1]

		# If all variables were removed, comment entire line out
		ss = s.strip()
		pats = ['INTEGER', 'DOUBLE', 'CHARACTER*8', 'DIMENSION', 'DOUBLE PRECISION', 'COMPLEX*16']
		if(ss in pats): s = 'C%s' % s
		
		# If the previous line ended with a comma and this one is
		# empty, then remove the comma from the previous line
		if(infile[line_num-1].endswith(',')):
			if(ss=='' or ss=='&' or s[0:1]=='C'): infile[line_num-1] = infile[line_num-1][:-1]

		# Comment out lines with single "&"
		if(ss=='&'): s = 'C%s' % s

		print "Line  in: %s" % infile[line_num]
		infile[line_num] = s
		print "Line out: %s" % infile[line_num]
		print ""

	# Check for unused "CONTINUE" lines
	if 'Warning: Label ' in line:
		if ' at (1) defined but not used' in line:

			first = line.find("'")+1
			last = line.find("'", first)
			var = line[first:last].upper()

			first = prev[0].find('bggen/code/') + 11
			last = prev[0].find(':', first)
			fname = prev[0][first:last]

			first = last + 1
			last = prev[0].find('.', first)
			line_num = int(prev[0][first:last])-1

			print 'commenting %d: %s' % (line_num, infile[line_num])
			infile[line_num] = 'C%s' % infile[line_num]
			
			# commen out any continuation lines
			while infile[line_num+1].strip().startswith('&'):
				line_num += 1
				infile[line_num]= 'C%s' % infile[line_num]

# Write modified file
f = open('pythia_h.F', 'w')
for line in infile: f.write('%s\n' % line)
f.close()

