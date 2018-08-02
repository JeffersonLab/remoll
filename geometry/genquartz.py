#!/usr/bin/python

from quartzdet import *

thisang = 0.0
thisbang = 0.0

# Define quartz
alldet = []
alldet.append( quartzdet(name = "brownquartz", z = 27.732, rmin = 1.0, rmax = 1.1, width_top = 0.246, width_bot = 0.223,  depth=0.01, ang = thisang, bang = thisbang, \
	ntype = 2, zoff = [0.0, 0.00764], dphi = [0.0, 12.86], nsect = 14 ) )
alldet.append( quartzdet(name = "redquartz", z = 27.872, rmin = 0.88, rmax = 1.0, width_top = 0.075, width_bot = 0.066,  depth=0.01, ang = thisang, bang = thisbang, \
	ntype = 6, zoff = [0.0, 0.005, 0.029, 0.005, 0.0, 0.0137], dphi = [0.0, 4.29, 8.57, 12.86, 17.14, 21.43], nsect = 14 ) )
alldet.append( quartzdet(name = "bluequartz", z = 28.027, rmin = 0.84, rmax = 0.88, width_top = 0.197, width_bot = 0.188,  depth=0.01, ang = thisang, bang = thisbang, \
	ntype = 2, zoff = [0.0, 0.00764], dphi = [0.0, 12.86], nsect =14 ) )

alldet.append( quartzdet(name = "greenquartz", z = 28.147, rmin = 0.78, rmax = 0.84, width_top = 0.188, width_bot = 0.175,  depth=0.01, ang = thisang, bang = thisbang, \
	ntype = 2, zoff = [0.0, 0.00757], dphi = [0.0, 12.86], nsect =14 ) )

alldet.append( quartzdet(name = "yellowquartz", z = 28.247, rmin = 0.68, rmax = 0.78, width_top = 0.175, width_bot = 0.152,  depth=0.01, ang = thisang, bang = thisbang, \
	ntype = 2, zoff = [0.0, 0.00764], dphi = [0.0, 12.86], nsect =14 ) )

alldet.append( quartzdet(name = "pinkquartz", z = 28.437, rmin = 0.63, rmax = 0.68, width_top = 0.152, width_bot = 0.141,  depth=0.01, ang = thisang, bang = thisbang, \
	ntype = 2, zoff = [0.0, 0.00764], dphi = [0.0, 12.86], nsect =14 ) )


###########################################################################################

print "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\" ?> <gdml xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation=\"schema/gdml.xsd\">"


print "<define>"
for det in alldet:
     det.makeallrotation()
     det.makeallposition()
     det.makesolidverts()
print "</define>"

print """
<materials>
   <material Z="1" name="Vacuum" state="gas">
       <T unit="K" value="2.73"/>
       <P unit="pascal" value="3e-18"/>
       <D unit="g/cm3" value="1e-25"/>
      <atom unit="g/mole" value="1.01"/>
   </material>

   <material Z="1" name="quartzmat" state="solid">
       <D unit="g/cm3" value="5.0"/>
       <atom unit="g/mole" value="1.01"/>
   </material>

</materials>
"""


print """
<solids>
    <box lunit="mm" name="boxMother" x="4000" y="4000" z="80000"/>"""
for det in alldet:
     det.makeonesolid()
print "</solids>"

print "<structure>"
for det in alldet:
     det.makeallvol()
print """<volume name="logicMother">
      <materialref ref="Vacuum"/>
      <solidref ref="boxMother"/>"""
for det in alldet:
     det.makeallphysvol()
print """ </volume>


  </structure>

    <setup name="Default" version="1.0">
        <world ref="logicMother"/>
	  </setup>

</gdml>"""


