#!/usr/bin/env python
import sys
import os
import math
import time
import argparse


parser= argparse.ArgumentParser(description="Change material and thickness based on given input parameters. Example: python changeMaterial.py -m G4_Pb -t 400")
parser.add_argument("-m", dest="material", action="store", required=True, help="Provide material")
parser.add_argument("-t", dest="thickness", action="store", required=True, help="Provide a length that is 10 radiation length of the material")
parser.add_argument("-p", dest="particle", action="store", required=True, help="Provide particle name such as e+")
parser.add_argument("-e", dest="energy", action="store", required=True, help="Provide energy of particle")
parser.add_argument("-n", dest="nevents", action="store", required=True, help="Provide number of events")


args=parser.parse_args()

macro_file = os.path.realpath("macros/run.mac")
output_file=os.path.realpath("geometry_standalone_edep/mollerMother.gdml")

f=open(output_file, "w+")

out ="<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\" ?>   \n \
<gdml xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation=\"http://service-spi.web.cern.ch/service-spi/app/releases/GDML/schema/gdml.xsd\">\n \
  <define> \n \
    <constant name=\"thickness\" value=\""+str(args.thickness)+"\"/>\n \
  </define>\n \
  <materials> \n \
    <material name=\"Epoxy\" state=\"solid\"> \n \
      <D value=\"1.3\" unit=\"g/cm3\"/>   \n \
      <fraction n=\"0.5354\" ref=\"C\"/> \n \
      <fraction n=\"0.1318\" ref=\"H\"/> \n \
      <fraction n=\"0.3328\" ref=\"O\"/> \n \
    </material> \n \
    <material name=\"G10\" state=\"solid\"> \n \
      <D value=\"1.3\" unit=\"g/cm3\"/>   \n \
      <fraction n=\"0.773\" ref=\"G4_SILICON_DIOXIDE\"/> \n \
      <fraction n=\"0.147\" ref=\"Epoxy\"/> \n \
      <fraction n=\"0.080\" ref=\"G4_Cl\"/> \n \
    </material>   \n \
  </materials>  \n \
  <solids> \n \
    <box lunit=\"mm\" name=\"world_solid\" x=\"1000\" y=\"1000\" z=\"9000\"/>\n \
    <box lunit=\"mm\" name=\"testMaterial\" x=\"1000\" y=\"1000\" z=\"thickness\"/>\n \
  </solids> \n \
  <structure> \n \
      <volume name=\"layer1_log\"> \n \
	<materialref ref=\""+args.material+"\"/> \n \
	<solidref ref=\"testMaterial\"/> \n \
	<auxiliary auxtype=\"SensDet\" auxvalue=\"planeDet\"/> \n \
	<auxiliary auxtype=\"Color\" auxvalue=\"Red\"/> \n \
	<auxiliary auxtype=\"DetNo\" auxvalue=\"10\"/> \n \
      </volume> \n \
      <volume name=\"logicMother\"> \n \
      <materialref ref=\"G4_Galactic\"/> \n \
      <solidref ref=\"world_solid\"/> \n \
      <physvol name=\"layer1_phys\"> \n \
	<volumeref ref=\"layer1_log\"/> \n \
	<position name=\"layer1_pos\" unit=\"mm\" z=\"thickness/2.\"/> \n \
      </physvol>  \n \
      <auxiliary auxtype=\"Alpha\" auxvalue=\"0.1\"/> \n \
    </volume> \n \
  </structure> \n \
  <setup name=\"Default\" version=\"1.0\"> \n \
    <world ref=\"logicMother\"/> \n \
  </setup> \n \
</gdml> \n \
"

f.write(out)

m=open(macro_file, "w+")

out = " \
/remoll/setgeofile geometry_standalone_edep/mollerMother.gdml  \n \
/remoll/physlist/register QGSP_BERT_HP         \n \
/remoll/physlist/parallel/enable                \n \
/remoll/parallel/setfile geometry_standalone_edep/mollerParallel.gdml \n \
/run/initialize \n \
/remoll/evgen/set beam \n \
/remoll/oldras false \n \
/remoll/evgen/beam/rasx 0.0 \n \
/remoll/evgen/beam/rasy 0.0 \n \
/remoll/evgen/beam/corrx 0.0 \n \
/remoll/evgen/beam/corry 0.0 \n \
/remoll/evgen/beam/partName "+args.particle+" \n \
/remoll/beamene "+args.energy+" \n \
/remoll/evgen/beam/z -498 \n \
/remoll/SD/enable_all \n \
/remoll/SD/detect lowenergyneutral 10 \n \
/remoll/SD/detect secondaries 10 \n \
/remoll/filename "+args.particle+"_"+args.energy+".root\n \
/run/beamOn "+args.nevents+"\n \
"

m.write(out)
