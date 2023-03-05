#!/usr/bin/env python
import sys
import os
import math
import time
import argparse
import csv


def printMaterials():
  out="\
  <materials> \n\
    <material name=\"SGlass\" state=\"solid\"> \n\
      <D value=\"2.52\" unit=\"g/cm3\"/>   \n\
      <fraction n=\"0.80\" ref=\"G4_SILICON_DIOXIDE\"/> \n\
      <fraction n=\"0.15\" ref=\"G4_ALUMINUM_OXIDE\"/> \n\
      <fraction n=\"0.05\" ref=\"G4_MAGNESIUM_OXIDE\"/> \n\
    </material> \n \
    <material name=\"mix9010\" state=\"solid\"> \n\
      <D value=\"2.24\" unit=\"g/cm3\"/>   \n\
      <fraction n=\"0.90\" ref=\"SGlass\"/> \n\
      <fraction n=\"0.10\" ref=\"G4_ETHYL_CELLULOSE\"/> \n\
    </material>   \n\
    <material name=\"mix6337\" state=\"solid\"> \n \
      <D value=\"1.73\" unit=\"g/cm3\"/>   \n\
      <fraction n=\"0.63\" ref=\"SGlass\"/> \n\
      <fraction n=\"0.37\" ref=\"G4_ETHYL_CELLULOSE\"/> \n\
    </material>   \n\
     <material name=\"mix5050\" state=\"solid\"> \n\
      <D value=\"1.56\" unit=\"g/cm3\"/>   \n\
      <fraction n=\"0.50\" ref=\"SGlass\"/> \n\
      <fraction n=\"0.50\" ref=\"G4_ETHYL_CELLULOSE\"/> \n\
    </material>   \n\
     <material name=\"mix4060\" state=\"solid\"> \n\
      <D value=\"1.45\" unit=\"g/cm3\"/>   \n\
      <fraction n=\"0.40\" ref=\"SGlass\"/> \n\
      <fraction n=\"0.60\" ref=\"G4_ETHYL_CELLULOSE\"/> \n\
    </material>   \n\
  </materials>  \n\
  "
  
  return out

def single_slab(p):
  detname = ["layer1_log"]
  detid   = [10]
  out="\
  <solids> \n \
    <box lunit=\"mm\" name=\"world_solid\" x=\"1000\" y=\"1000\" z=\"9000\"/>\n \
    <box lunit=\"mm\" name=\"testMaterial\" x=\"1000\" y=\"1000\" z=\"thickness\"/>\n \
  </solids> \n \
  <structure> \n \
    <volume name=\""+detname[0]+"\"> \n \
      <materialref ref=\""+p["single_slab_mat"]+"\"/> \n \
      <solidref ref=\"testMaterial\"/> \n \
      <auxiliary auxtype=\"SensDet\" auxvalue=\"planeDet\"/> \n \
      <auxiliary auxtype=\"Color\" auxvalue=\"Red\"/> \n \
      <auxiliary auxtype=\"DetNo\" auxvalue=\""+str(detid[0])+"\"/> \n \
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
  return [detname, detid, out]

def layers(p):
  detname=['second_sglass_logic', 'W_sideplate_logic', 'first_sglass_logic', 'second_sglass_logic1', 'W_sideplate_logic1', 'first_sglass_logic1','top_sglass_logic', 'bot_sglass_logic']
  detid = [1,2,3,4,5,6,7,8] 
  out="\
  <solids> \n \
    <box lunit=\"mm\" name=\"world_solid\" x=\"1000\" y=\"1000\" z=\"9000\"/>\n \
    <box lunit=\"mm\" name=\"coil_straight_segment_solid\" x=\"1000\" y=\"sideplate_height+2*(topbot_sglass_width)\" z=\"single_conductor_width+2*W_sideplate_width+2*first_sglass_width+2*second_sglass_width\"/> \n \
    <box lunit=\"mm\" name=\"second_sglass_solid\" x=\"1000\" y=\"sideplate_height\" z=\"second_sglass_width\"/>\n \
    <box lunit=\"mm\" name=\"W_sideplate_solid\" x=\"1000\" y=\"sideplate_height\" z=\"W_sideplate_width\"/>\n \
    <box lunit=\"mm\" name=\"first_sglass_solid\" x=\"1000\" y=\"sideplate_height\" z=\"first_sglass_width\"/>\n \
    <box lunit=\"mm\" name=\"single_conductor_solid\" x=\"1000\" y=\"single_conductor_height\" z=\"single_conductor_width\"/>\n \
    <box lunit=\"mm\" name=\"single_gap_solid\" x=\"1000\" y=\"single_gap_height\" z=\"single_conductor_width\" /> \n \
    <box lunit=\"mm\" name=\"topbot_sglass_solid\" x=\"1000\" y=\"topbot_sglass_width\" z=\"single_conductor_width+2*W_sideplate_width+2*first_sglass_width+2*second_sglass_width\" /> \n \
    <tube lunit=\"mm\" aunit=\"deg\" name=\"water_tube_solid\" deltaphi=\"360\" startphi=\"0\" z=\"1000\" rmin=\"0\" rmax=\"water_tube_radius\"/> \n \
  </solids> \n \
  <structure> \n \
      <volume name=\""+detname[0]+"\"> \n \
        <materialref ref=\""+p["insulation_mat"]+"\"/> \n \
        <solidref ref=\"second_sglass_solid\"/> \n \
        <auxiliary auxtype=\"SensDet\" auxvalue=\"planeDet\"/> \n \
        <auxiliary auxtype=\"Color\" auxvalue=\"Red\"/> \n \
        <auxiliary auxtype=\"DetNo\" auxvalue=\""+str(detid[0])+"\"/> \n \
      </volume> \n \
       <volume name=\""+detname[1]+"\"> \n \
        <materialref ref=\""+p["sideplate_mat"]+"\"/> \n \
        <solidref ref=\"W_sideplate_solid\"/> \n \
        <auxiliary auxtype=\"SensDet\" auxvalue=\"planeDet\"/> \n \
        <auxiliary auxtype=\"Color\" auxvalue=\"Grey\"/> \n \
        <auxiliary auxtype=\"DetNo\" auxvalue=\""+str(detid[1])+"\"/> \n \
      </volume> \n \
       <volume name=\""+detname[2]+"\"> \n \
        <materialref ref=\""+p["insulation_mat"]+"\"/> \n \
        <solidref ref=\"first_sglass_solid\"/> \n \
        <auxiliary auxtype=\"SensDet\" auxvalue=\"planeDet\"/> \n \
        <auxiliary auxtype=\"Color\" auxvalue=\"Red\"/> \n \
        <auxiliary auxtype=\"DetNo\" auxvalue=\""+str(detid[2])+"\"/> \n \
      </volume> \n \
      <volume name=\""+detname[3]+"\"> \n \
        <materialref ref=\""+p["insulation_mat"]+"\"/> \n \
        <solidref ref=\"second_sglass_solid\"/> \n \
        <auxiliary auxtype=\"SensDet\" auxvalue=\"planeDet\"/> \n \
        <auxiliary auxtype=\"Color\" auxvalue=\"Red\"/> \n \
        <auxiliary auxtype=\"DetNo\" auxvalue=\""+str(detid[3])+"\"/> \n \
      </volume> \n \
      <volume name=\""+detname[4]+"\"> \n \
        <materialref ref=\""+p["sideplate_mat"]+"\"/> \n \
        <solidref ref=\"W_sideplate_solid\"/> \n \
        <auxiliary auxtype=\"SensDet\" auxvalue=\"planeDet\"/> \n \
        <auxiliary auxtype=\"Color\" auxvalue=\"Grey\"/> \n \
        <auxiliary auxtype=\"DetNo\" auxvalue=\""+str(detid[4])+"\"/> \n \
      </volume> \n \
      <volume name=\""+detname[5]+"\"> \n \
        <materialref ref=\""+p["insulation_mat"]+"\"/> \n \
        <solidref ref=\"first_sglass_solid\"/> \n \
        <auxiliary auxtype=\"SensDet\" auxvalue=\"planeDet\"/> \n \
        <auxiliary auxtype=\"Color\" auxvalue=\"Red\"/> \n \
        <auxiliary auxtype=\"DetNo\" auxvalue=\""+str(detid[5])+"\"/> \n \
      </volume> \n \
      <volume name=\""+detname[6]+"\"> \n \
        <materialref ref=\""+p["insulation_mat"]+"\"/> \n \
        <solidref ref=\"topbot_sglass_solid\"/> \n \
        <auxiliary auxtype=\"SensDet\" auxvalue=\"planeDet\"/> \n \
        <auxiliary auxtype=\"Color\" auxvalue=\"Red\"/> \n \
        <auxiliary auxtype=\"DetNo\" auxvalue=\""+str(detid[6])+"\"/> \n \
      </volume> \n \
      <volume name=\""+detname[7]+"\"> \n \
        <materialref ref=\""+p["insulation_mat"]+"\"/> \n \
        <solidref ref=\"topbot_sglass_solid\"/> \n \
        <auxiliary auxtype=\"SensDet\" auxvalue=\"planeDet\"/> \n \
        <auxiliary auxtype=\"Color\" auxvalue=\"Red\"/> \n \
        <auxiliary auxtype=\"DetNo\" auxvalue=\""+str(detid[7])+"\"/> \n \
      </volume> \n"
  detname
  for i in range(0,int(p["n_conductors"])):
    detname.append("water_tube_logic_"+str(i))
    detname.append("single_conductor_logic_"+str(i))
    detid.append(50+i)
    detid.append(60+i)
    out+="<volume name=\"water_tube_logic_"+str(i)+"\"> \n \
            <materialref ref=\""+p["conductor_hole_mat"]+"\"/> \n \
            <solidref ref=\"water_tube_solid\"/> \n \
            <auxiliary auxtype=\"SensDet\" auxvalue=\"planeDet\"/> \n \
            <auxiliary auxtype=\"Color\" auxvalue=\"Blue\"/> \n \
            <auxiliary auxtype=\"DetNo\" auxvalue=\""+str(50+i)+"\"/> \n \
          </volume> \n  \
          <volume name=\"single_conductor_logic_"+str(i)+"\"> \n \
            <materialref ref=\""+p["conductor_mat"]+"\"/> \n \
            <solidref ref=\"single_conductor_solid\"/> \n \
            <physvol name=\"water_tube_phys\"> \n \
              <volumeref ref=\"water_tube_logic_"+str(i)+"\"/> \n \
              <rotation unit=\"deg\" name=\"water_tube_rotate_"+str(i)+"\" x=\"0\" y=\"90\" z=\"0\"/> \n \
            </physvol>  \n \
            <auxiliary auxtype=\"SensDet\" auxvalue=\"planeDet\"/> \n \
            <auxiliary auxtype=\"Color\" auxvalue=\"Orange\"/> \n \
            <auxiliary auxtype=\"DetNo\" auxvalue=\""+str(60+i)+"\"/> \n \
          </volume> \n"
    if i<int(p["n_conductors"])-1:
      detname.append("single_gap_logic_"+str(i))
      detid.append(70+i)
      out+="<volume name=\"single_gap_logic_"+str(i)+"\"> \n \
              <materialref ref=\""+p["insulation_mat"]+"\"/> \n \
              <solidref ref=\"single_gap_solid\"/> \n \
              <auxiliary auxtype=\"SensDet\" auxvalue=\"planeDet\"/> \n \
              <auxiliary auxtype=\"Color\" auxvalue=\"Red\"/> \n \
            <auxiliary auxtype=\"DetNo\" auxvalue=\""+str(70+i)+"\"/> \n \
            </volume> \n "
  out+=" <volume name=\"logicCoilStraightSegment\"> \n \
      <materialref ref=\"G4_Galactic\"/> \n \
      <solidref ref=\"coil_straight_segment_solid\"/> \n \
      <physvol name=\"second_sglass_phys\"> \n \
        <volumeref ref=\"second_sglass_logic\"/> \n \
        <position name=\"second_sglass_pos\" unit=\"mm\" z=\"-second_sglass_width/2.0-W_sideplate_width-first_sglass_width-single_conductor_width/2.0\"/> \n \
      </physvol>  \n \
      <physvol name=\"W_sideplate_phys\"> \n \
        <volumeref ref=\"W_sideplate_logic\"/> \n \
        <position name=\"W_sideplate_pos\" unit=\"mm\" z=\"-W_sideplate_width/2.0-first_sglass_width-single_conductor_width/2.0\"/> \n \
      </physvol>  \n \
      <physvol name=\"first_sglass_phys\"> \n \
        <volumeref ref=\"first_sglass_logic\"/> \n \
        <position name=\"first_sglass_pos\" unit=\"mm\" z=\"-first_sglass_width/2.0-single_conductor_width/2.0\"/> \n \
      </physvol>  \n \
      <physvol name=\"second_sglass_phys1\"> \n \
        <volumeref ref=\"second_sglass_logic1\"/> \n \
        <position name=\"second_sglass_pos1\" unit=\"mm\" z=\"second_sglass_width/2.0+first_sglass_width+W_sideplate_width+single_conductor_width/2.0\"/> \n \
      </physvol>  \n \
      <physvol name=\"W_sideplate_phys1\"> \n \
        <volumeref ref=\"W_sideplate_logic1\"/> \n \
        <position name=\"W_sideplate_pos1\" unit=\"mm\" z=\"W_sideplate_width/2.0+first_sglass_width+single_conductor_width/2.0\"/> \n \
      </physvol>  \n \
      <physvol name=\"first_sglass_phys1\"> \n \
        <volumeref ref=\"first_sglass_logic1\"/> \n \
        <position name=\"first_sglass_pos1\" unit=\"mm\" z=\"first_sglass_width/2.0+single_conductor_width/2.0\"/> \n \
      </physvol> \n\
       <physvol name=\"top_sglass_phys\"> \n \
        <volumeref ref=\"top_sglass_logic\"/> \n \
        <position name=\"top_sglass_pos\" unit=\"mm\" y=\"sideplate_height/2.0+(topbot_sglass_width)/2.0\"/> \n \
      </physvol>  \n \
      <physvol name=\"bot_sglass_phys\"> \n \
        <volumeref ref=\"bot_sglass_logic\"/> \n \
        <position name=\"bot_sglass_pos\" unit=\"mm\" y=\"-sideplate_height/2.0-(topbot_sglass_width)/2.0\" /> \n \
      </physvol>  \n  "

  for i in range(0,int(p["n_conductors"])):
    out+="<physvol name=\"single_conductor_phys_"+str(i)+"\"> \n \
         <volumeref ref=\"single_conductor_logic_"+str(i)+"\"/> \n \
         <position name=\"single_conductor_pos_"+str(i)+"\" unit=\"mm\" y=\"-sideplate_height/2.0+"+str(i+0.5)+"*single_conductor_height+"+str(i)+"*single_gap_height\"/> \n \
         </physvol>  \n "
    if i<int(p["n_conductors"])-1:
      out+="<physvol name=\"single_gap_phys_"+str(i)+"\"> \n \
         <volumeref ref=\"single_gap_logic_"+str(i)+"\"/> \n \
         <position name=\"single_gap_pos_"+str(i)+"\" unit=\"mm\" y=\"-sideplate_height/2.0+"+str(i+1)+"*single_conductor_height+"+str(i+0.5)+"*single_gap_height\" /> \n \
         </physvol>  \n "
  out+=" <auxiliary auxtype=\"Alpha\" auxvalue=\"0.1\"/> \n \
    </volume> \n \
    <volume name=\"logicMother\"> \n \
      <materialref ref=\"G4_Galactic\"/> \n \
      <solidref ref=\"world_solid\"/> \n \
      <physvol name=\"coil_straight_segment_phys\"> \n \
        <volumeref ref=\"logicCoilStraightSegment\"/> \n \
        <rotation name=\"rot_coil_straight\" unit=\"deg\" x=\""+p["x_rotation"]+"\" y=\""+p["y_rotation"]+"\" z=\""+p["z_rotation"]+"\" /> \n \
      </physvol> \n \
    <auxiliary auxtype=\"Alpha\" auxvalue=\"0.1\"/> \n \
    </volume> \n \
    </structure> \n \
    <setup name=\"Default\" version=\"1.0\"> \n \
      <world ref=\"logicMother\"/> \n \
    </setup> \n \
   </gdml> \n \
  "
  return [detname, detid, out]

parser= argparse.ArgumentParser(description="Generate geometry to run standalone sims. Example: python generateStandalone.py -c config1.list")
parser.add_argument("-c", dest="par_list", action="store", required=True, help="Include a config file with list of parameters")

args=parser.parse_args()

p={}    # dictionary of parameter values

with open(args.par_list) as csvfile:
     reader=csv.reader(csvfile, delimiter=',', quotechar='|')
     for row in reader:
         p[row[0]]=row[1].strip()
    

macro_file = os.path.realpath(p["run_macro"])
output_file=os.path.realpath(p["output_geometry"])

f=open(output_file, "w+")

out =""

if (p["single_slab"] =="true"):
 out ="<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\" ?>   \n\
  <gdml xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation=\"http://service-spi.web.cern.ch/service-spi/app/releases/GDML/schema/gdml.xsd\">\n \
  <define> \n\
    <constant name=\"thickness\" value=\""+p["thickness"]+"\"/>\n \
  </define>\n "
 out+=printMaterials()
 layerinfo=single_slab(p)
 out+=layerinfo[2]

else:
 p["gap_height"] = (float(p["stack_height"])-float(p["n_conductors"])*float(p["single_conductor_height"]))/(float(p["n_conductors"])-1)
 out ="<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\" ?>   \n \
  <gdml xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation=\"http://service-spi.web.cern.ch/service-spi/app/releases/GDML/schema/gdml.xsd\">\n \
  <define> \n \
    <constant name=\"single_conductor_width\" value=\""+str(p["single_conductor_width"])+"\"/>\n \
    <constant name=\"single_conductor_height\" value=\""+str(p["single_conductor_height"])+"\"/>\n \
    <constant name=\"single_gap_height\" value=\""+str(p["gap_height"])+"\"/>\n \
    <constant name=\"first_sglass_width\"  value=\""+str(p["first_insulation_width"])+"\"/>\n \
    <constant name=\"W_sideplate_width\" value=\""+str(p["sideplate_width"])+"\"/>\n \
    <constant name=\"second_sglass_width\"  value=\""+str(p["second_insulation_width"])+"\"/>\n \
    <constant name=\"topbot_sglass_width\"  value=\""+str(p["topbot_insulation_width"])+"\"/>\n \
    <constant name=\"sideplate_height\" value=\""+str(p["stack_height"])+"\"/>\n \
    <constant name=\"water_tube_radius\" value=\""+str(p["conductor_hole_radius"])+"\"/>\n \
  </define>\n \
 "
 out+=printMaterials()
 layerinfo = layers(p)
 out+=layerinfo[2]

f.write(out)

m=open(macro_file, "w+")


out = "\
/remoll/setgeofile geometry_standalone_edep/mollerMother.gdml  \n\
/remoll/physlist/register QGSP_BERT_HP         \n\
/remoll/physlist/parallel/enable                \n\
/remoll/parallel/setfile geometry_standalone_edep/mollerParallel.gdml \n\
/run/initialize \n\
"
for i in layerinfo[0]:
  out+="/remoll/geometry/userlimits/usermaxallowedstep "+i+" "+p["max_step_limit_charged_particles"]+"*mm \n\
"
out+="/remoll/evgen/set beam \n\
/remoll/oldras false \n\
/remoll/evgen/beam/rasrefz "+p["beam_z_pos"]+" mm \n\
/remoll/evgen/beam/rasx 0.0 \n\
/remoll/evgen/beam/rasy 0.0 \n\
/remoll/evgen/beam/corrx 0.0 \n\
/remoll/evgen/beam/corry 0.0 \n\
/remoll/evgen/beam/partName "+p["particle"]+" \n\
/remoll/beamene "+p["energy"]+" \n\
/remoll/evgen/beam/origin "+p["beam_x_pos"]+" "+p["beam_y_pos"]+" "+p["beam_z_pos"]+" mm \n\
/remoll/evgen/beam/th "+p["beam_th_dir"]+" deg \n\
/remoll/evgen/beam/ph "+p["beam_ph_dir"]+" deg \n\
/remoll/SD/disable_all \n"
for i in layerinfo[1]:
  out += "\
/remoll/SD/enable "+str(i)+" \n\
/remoll/SD/detect lowenergyneutral "+str(i)+" \n\
/remoll/SD/detect secondaries "+str(i)+" \n"
out+="\
/remoll/filename "+p["config_name"]+".root\n\
/run/beamOn "+p["nevents"]+"\n\
"

m.write(out)

