#!/usr/bin/python

from math import tan, sin, cos

class quartzdet:
    def __init__(self, name, z, rmin, rmax, width_top, width_bot, depth, ang, bang, ntype, zoff, dphi, nsect = 7):
	self.name = name
	self.nsect = nsect
	self.z = z
	self.rmin = rmin
	self.rmax = rmax
	self.height= rmax - rmin
	self.r0    = (rmax + rmin)/2.0
	self.width_top = width_top
	self.width_bot = width_bot
	self.depth= depth
	self.ntype = ntype
	self.ang  = ang
	self.bang  = bang
	if  ntype != len(zoff) or ntype != len(dphi):
	    print "ERROR:  quartz detector definition contains arrays of length different from the number of quartz types"
	    exit(1)
	self.zoff = zoff
	self.dphi = dphi


    def makeonerotation(self, i = 0, n = 0):
	rotname = "qdrot_" + self.name + "_" + str(i) + "_" + str(n)
	print "<rotation name=\""+ rotname + "\" " + \
		"z=\"" + str(-(n*2.0*3.1415927/self.nsect + self.dphi[i]*3.1415927/180.0)) + "\"/>"
    def makeallrotation(self):
	for i in range(self.nsect):
	    for j in range(self.ntype):
		self.makeonerotation(j,i)

    def makeoneposition(self, i = 0, n = 0):
	posname = "qdpos_" + self.name + "_" + str(i) + "_" + str(n)
	x = self.r0*cos(n*2.0*3.1415927/self.nsect + self.dphi[i]*3.1415927/180.0)
	y = self.r0*sin(n*2.0*3.1415927/self.nsect + self.dphi[i]*3.1415927/180.0)

	print "<position name=\"" +posname+"\" " + \
		"unit = \"m\" "+ \
		"x=\"" + str(x) + "\" "+ \
		"y=\"" + str(y) + "\" "+ \
		"z=\"" + str(self.z + self.zoff[i]) + "\" />"
    def makeallposition(self):
	for i in range(self.nsect):
	    for j in range(self.ntype):
		self.makeoneposition(j,i)

    def makesolidverts(self):
	vx = [None]*8
	vy = [None]*8
	vz = [None]*8

	angtodeg = 3.1415927/180.0
	thistanth = tan(self.ang*angtodeg)

	vx[2] =    self.height/2.0
	vz[2] =    self.depth/2.0
	vy[2] =    self.width_top/2.0

	vx[3] =    self.height/2.0
	vz[3] =   -self.depth/2.0
	vy[3] =    self.width_top/2.0

	vz[1] =   vz[2] - self.height*thistanth
	vx[1] =  -self.height/2.0
	vy[1] =  self.width_bot/2.0

	if self.bang != 90.0:
	    thistanph = tan(self.bang*angtodeg)
	    vx[0] =  (vx[1] + ( vx[3]*thistanth  + (vz[1] - vz[3]) )* thistanph )/(1.0 + thistanth*thistanph)
	    if vx[1] > vx[3]:
		print "quartz specification is not geometrically possible"
		exit(1)
	else:
	    vx[0] =  vx[1] + self.depth/thistanth

	vz[0] =  vz[3] - (vx[3] - vx[0] )*thistanth
	vy[0] =  self.width_bot/2.0

	for i in range(4):
	    vx[i+4] =  vx[i]
	    vy[i+4] = -vy[i]
	    vz[i+4] =  vz[i]


	for i in range(8):
	    posname = "qdvertpos_" + self.name + "_" + str(i)
	    print "<position name=\"" +posname+"\" " + \
		"unit = \"m\" "+ \
		"x=\"" + str(vx[i]) + "\" "+ \
		"y=\"" + str(vy[i]) + "\" "+ \
		"z=\"" + str(vz[i]) + "\" />"


    def makeonesolid(self):
	vname1 = [None]*6
	vname2 = [None]*6
	vname3 = [None]*6
	vname4 = [None]*6


	vname1[0] = "qdvertpos_" + self.name + "_0"
	vname2[0] = "qdvertpos_" + self.name + "_1"
	vname3[0] = "qdvertpos_" + self.name + "_2"
	vname4[0] = "qdvertpos_" + self.name + "_3"

	vname1[1] = "qdvertpos_" + self.name + "_5"
	vname2[1] = "qdvertpos_" + self.name + "_6"
	vname3[1] = "qdvertpos_" + self.name + "_2"
	vname4[1] = "qdvertpos_" + self.name + "_1"

	vname1[2] = "qdvertpos_" + self.name + "_7"
	vname2[2] = "qdvertpos_" + self.name + "_3"
	vname3[2] = "qdvertpos_" + self.name + "_2"
	vname4[2] = "qdvertpos_" + self.name + "_6"

	vname1[3] = "qdvertpos_" + self.name + "_4"
	vname2[3] = "qdvertpos_" + self.name + "_7"
	vname3[3] = "qdvertpos_" + self.name + "_6"
	vname4[3] = "qdvertpos_" + self.name + "_5"

	vname1[4] = "qdvertpos_" + self.name + "_0"
	vname2[4] = "qdvertpos_" + self.name + "_3"
	vname3[4] = "qdvertpos_" + self.name + "_7"
	vname4[4] = "qdvertpos_" + self.name + "_4"

	vname1[5] = "qdvertpos_" + self.name + "_0"
	vname2[5] = "qdvertpos_" + self.name + "_4"
	vname3[5] = "qdvertpos_" + self.name + "_5"
	vname4[5] = "qdvertpos_" + self.name + "_1"



	print "<tessellated name=\"qdsol_"+self.name+"\">"
	for i in range(6):
	    print "     <quadrangular vertex1=\"" + \
		    vname1[i] + "\" vertex2=\"" +\
		    vname2[i] + "\" vertex3=\"" +\
		    vname3[i] + "\" vertex4=\"" +\
		    vname4[i] + "\" type=\"ABSOLUTE\"/>"
	print "</tessellated>"


    def makeonevol(self, i = 0, n = 0):
	if( n >= self.nsect ):
	    print "ERROR:  too many sectors"
	    exit(1)

	rotname = "qdrot_" + self.name + "_" + str(i) + "_" + str(n)
	posname = "qdpos_" + self.name + "_" + str(i) + "_" + str(n)

	print "<volume name= \"qdvol_" + self.name + "_" + str(n) + "_" + str(i) + "\" >" 
	print "    <materialref ref=\"quartzmat\"/>" 
	print "    <solidref ref=\"qdsol_"+self.name+"\"/>"
	print "</volume>"
    def makeallvol(self):
	for i in range(self.nsect):
	    for j in range(self.ntype):
		self.makeonevol(j,i)

    def makeonephysvol(self, i = 0, n = 0):
	if( n >= self.nsect ):
	    print "ERROR:  too many sectors"
	    exit(1)

	rotname = "qdrot_" + self.name + "_" + str(i) + "_" + str(n)
	posname = "qdpos_" + self.name + "_" + str(i) + "_" + str(n)

	print "    <physvol>"
	print "         <volumeref ref=\"qdvol_" + self.name + "_" + str(n) + "_" + str(i) + "\"/>"
	print "         <positionref ref=\"" + posname + "\"/>"
	print "         <rotationref ref=\"" + rotname + "\"/>"
	print "    </physvol>"
    def makeallphysvol(self):
	for i in range(self.nsect):
	    for j in range(self.ntype):
		self.makeonephysvol(j,i)
