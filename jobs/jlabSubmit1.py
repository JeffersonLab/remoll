#!/usr/bin/python
from subprocess import call
import sys, os, time, tarfile

def main():

#FIXME Update these
    email = "vdoomra@jlab.org"

    config = "test_run"

    identifier = raw_input("Please enter the identifier: ")

    sourceDir = "/work/halla/moller12gev/vdoomra"
    #outDir = "/lustre/expphy/volatile/halla/parity/chandan/sim_out/"+identifier
    outDir = "/lustre/expphy/volatile/halla/moller12gev/vdoomra"+config
    if not os.path.exists(outDir):
       os.makedirs(outDir)
    nrEv   = 500 #900000
    nrStart= 1
    nrStopActual = 4 #60
    nrStop = nrStopActual+1 #60

    print('Running ' + str(nrEv*(nrStop - nrStart)) + ' events...')

    #jobName=config + '_' + identifier + '_' + val + "mm" + '_%03dkEv'%(nrEv/1000)
    jobName=config + '_' + identifier + '_%02dkEv'%(nrEv/1000)

    ###tar exec+geometry
    make_tarfile(sourceDir,config,identifier)

    for jobNr in range(nrStart,nrStop): # repeat for jobNr jobs
        print("Starting job setup for jobID: " + str(jobNr))

        jobFullName = jobName + '_%03d'%jobNr
        outDirFull=outDir+"/"+jobFullName
        createMacFiles(config, outDirFull, sourceDir, nrEv, jobNr, identifier)

        ###copy tarfile
        call(["cp",sourceDir+"/rad_analysis/default.tar.gz",
              outDir+"/"+jobFullName+"/default.tar.gz"])

    createXMLfile(sourceDir,outDir,jobName,nrStart,nrStop,email,identifier)

    print "All done for config ",config,"_",identifier," for #s between ",nrStart, " and ", nrStopActual


def createMacFiles(config,outDirFull,sourceDir,nrEv,jobNr,identifier):

    if not os.path.exists(outDirFull+"/log"):
        os.makedirs(outDirFull+"/log")

    f=open(outDirFull+"/"+"runexample_"+identifier+".mac",'w')
    f.write("/remoll/setgeofile geometry/mollerMother_merged.gdml\n")
    f.write("/remoll/physlist/register QGSP_BERT_HP\n")
    f.write("/remoll/physlist/parallel/enable\n")
    f.write("/remoll/parallel/setfile geometry/mollerParallel.gdml\n")
    f.write("/run/numberOfThreads 10\n")
    f.write("/run/initialize\n")
    f.write("/remoll/addfield map_directory/hybridJLAB.txt\n")
    f.write("/remoll/addfield map_directory/upstreamJLAB_1.25.txt\n")
    #f.write("/remoll/evgen/set moller\n")
    f.write("/remoll/evgen/set beam\n")
    #f.write("/remoll/evgen/set elastic\n")
    #f.write("/remoll/oldras false\n")
    f.write("/remoll/evgen/beam/origin 0 0 -7.5 m\n")
    f.write("/remoll/evgen/beam/rasx 5 mm\n")
    f.write("/remoll/evgen/beam/rasy 5 mm\n")
    f.write("/remoll/evgen/beam/corrx 0.065\n")
    f.write("/remoll/evgen/beam/corry 0.065\n")
    #f.write("/remoll/beam_corrph 0.02134\n")
    #f.write("/remoll/beam_corrth 0.02134\n")
    f.write("/remoll/evgen/beam/rasrefz -4.5 m\n")
    f.write("/remoll/beamene 11 GeV\n")
    f.write("/remoll/beamcurr 70 microampere\n")
    f.write("/remoll/SD/disable_all\n")
    f.write("/remoll/SD/enable 28\n")
    f.write("/remoll/SD/detect lowenergyneutral 28\n")
    f.write("/remoll/SD/detect secondaries 28\n")
    f.write("/remoll/SD/detect boundaryhits 28\n") 
 
    f.write("/remoll/SD/enable 55\n")
    f.write("/remoll/SD/detect lowenergyneutral 55\n")
    f.write("/remoll/SD/detect secondaries 55\n")
    f.write("/remoll/SD/detect boundaryhits 55\n")  

    f.write("/remoll/SD/enable 56\n")
    f.write("/remoll/SD/detect lowenergyneutral 56\n")
    f.write("/remoll/SD/detect secondaries 56\n")
    f.write("/remoll/SD/detect boundaryhits 56\n")  

    f.write("/remoll/SD/enable 57\n")
    f.write("/remoll/SD/detect lowenergyneutral 57\n")
    f.write("/remoll/SD/detect secondaries 57\n")
    f.write("/remoll/SD/detect boundaryhits 57\n")  

    f.write("/remoll/SD/enable 58\n")
    f.write("/remoll/SD/detect lowenergyneutral 58\n")
    f.write("/remoll/SD/detect secondaries 58\n")
    f.write("/remoll/SD/detect boundaryhits 58\n")  

    #f.write("/remoll/SD/enable 3340\n")
    #f.write("/remoll/SD/detect lowenergyneutral 3340\n")
    #f.write("/remoll/SD/detect secondaries 3340\n")
    #f.write("/remoll/SD/detect boundaryhits 3340\n") 
   # f.write("/remoll/SD/enable 95\n")
   # f.write("/remoll/SD/detect lowenergyneutral 95\n")
   # f.write("/remoll/SD/detect secondaries 95\n")
   # f.write("/remoll/SD/detect boundaryhits 95\n")  
   # f.write("/remoll/SD/enable 55\n")
   # f.write("/remoll/SD/detect lowenergyneutral 55\n")
   # f.write("/remoll/SD/detect secondaries 55\n")
   # f.write("/remoll/SD/enable 56\n")
   # f.write("/remoll/SD/detect lowenergyneutral 56\n")
   # f.write("/remoll/SD/detect secondaries 56\n")
   # f.write("/remoll/SD/enable 3333\n")
   # f.write("/remoll/SD/detect lowenergyneutral 3333\n")
   # f.write("/remoll/SD/detect secondaries 3333\n")
   # f.write("/remoll/SD/detect boundaryhits 3333\n")  
   # f.write("/remoll/SD/enable 3334\n")
   # f.write("/remoll/SD/detect lowenergyneutral 3334\n")
   # f.write("/remoll/SD/detect secondaries 3334\n")
   # f.write("/remoll/SD/detect boundaryhits 3334\n")  
   # f.write("/remoll/SD/enable 3335\n")
   # f.write("/remoll/SD/detect lowenergyneutral 3335\n")
   # f.write("/remoll/SD/detect secondaries 3335\n")
   # f.write("/remoll/SD/detect boundaryhits 3335\n")  
   # f.write("/remoll/SD/enable 3336\n")
   # f.write("/remoll/SD/detect lowenergyneutral 3336\n")
   # f.write("/remoll/SD/detect secondaries 3336\n")
   # f.write("/remoll/SD/detect boundaryhits 3336\n")  
   # f.write("/remoll/SD/enable 3337\n")
   # f.write("/remoll/SD/detect lowenergyneutral 3337\n")
   # f.write("/remoll/SD/detect secondaries 3337\n")
   # f.write("/remoll/SD/detect boundaryhits 3337\n")  
   # f.write("/remoll/SD/enable 3338\n")
   # f.write("/remoll/SD/detect lowenergyneutral 3338\n")
   # f.write("/remoll/SD/detect secondaries 3338\n")
   # f.write("/remoll/SD/detect boundaryhits 3338\n")  
   # f.write("/remoll/SD/enable 3339\n")
   # f.write("/remoll/SD/detect lowenergyneutral 3339\n")
   # f.write("/remoll/SD/detect secondaries 3339\n")
   # f.write("/remoll/SD/detect boundaryhits 3339\n")  
    #f.write("/remoll/SD/enable 96\n")
    #f.write("/remoll/SD/detect lowenergyneutral 96\n")
    #f.write("/remoll/SD/detect secondaries 96\n")
    #f.write("/remoll/SD/enable 97\n")
    #f.write("/remoll/SD/detect lowenergyneutral 97\n")
    #f.write("/remoll/SD/detect secondaries 97\n")
    #f.write("/remoll/kryptonite/volume logicUSTracker\n")
    #f.write("/remoll/kryptonite/volume logicDSTracker\n")
    #f.write("/remoll/kryptonite/volume logicWasher_12\n")
    f.write("/remoll/kryptonite/enable\n")
    f.write("/process/list\n")
    f.write("/remoll/filename remollout.root\n")
    f.write("/run/beamOn "+str(nrEv)+"\n")
    f.close()
    return 0

def createXMLfile(sourceDir,outDir,jobName,nrStart,nrStop,email,identifier):

    if not os.path.exists(sourceDir+"/rad_analysis/jobs"):
        os.makedirs(sourceDir+"/rad_analysis/jobs")

    f=open(sourceDir+"/rad_analysis/jobs/"+jobName+".xml","w")
    f.write("<Request>\n")
    f.write("  <Email email=\""+email+"\" request=\"false\" job=\"true\"/>\n")
    f.write("  <Project name=\"moller12gev\"/>\n")

     f.write("  <Track name=\"debug\"/>\n")
#    f.write("  <Track name=\"analysis\"/>\n")
#    f.write("  <Track name=\"simulation\"/>\n")

    f.write("  <Name name=\""+jobName+"\"/>\n")
    f.write("  <OS name=\"general\"/>\n")
    f.write("  <Memory space=\"3535\" unit=\"MB\"/>\n")

    f.write("  <Command><![CDATA[\n")
    f.write("    pwd\n")
    f.write("    tar -zxvf default.tar.gz\n")
    f.write("    ./remoll runexample_"+identifier+".mac\n")
#    f.write("    ./pruneTree remollout.root 101 n\n")
#    f.write("    ./pruneTree remollout.root 101 n\n")
    #f.write("    ./pruneTree remollout.root 99 n\n")
#    f.write("    rm -f remollout.root\n")
#    f.write("    mv remollout.root Coll1\n")
    f.write("  ]]></Command>\n")

    for number in range(nrStart,nrStop): # repeat for nr jobs
        idName= outDir+"/"+jobName+'_%03d'%(number)
        rootfile= outDir+"/remollout_"+identifier+'%d'%(number)
        f.write("  <Job>\n")
        f.write("    <Input src=\""+idName+"/runexample_"+identifier+".mac\" dest=\"runexample_"+identifier+".mac\"/>\n")
        f.write("    <Input src=\""+idName+"/default.tar.gz\" dest=\"default.tar.gz\"/>\n")

      #  f.write("    <Output src=\"remollout_optimized_det99.root\" dest=\""+rootfile+"_99.root\"/>\n")
        f.write("    <Output src=\"remollout.root\" dest=\""+rootfile+".root\"/>\n")
        f.write("    <Stdout dest=\""+idName+"/log/log.out\"/>\n")
        f.write("    <Stderr dest=\""+idName+"/log/log.err\"/>\n")
        f.write("  </Job>\n\n")

    f.write("</Request>\n")
    f.close()
    return 0

def make_tarfile(sourceDir,config,ident):
    print "making geometry tarball"
    if os.path.isfile(sourceDir+"/rad_analysis/default.tar.gz"):
        os.remove(sourceDir+"/rad_analysis/default.tar.gz")
    tar = tarfile.open(sourceDir+"/rad_analysis/default.tar.gz","w:gz")
    tar.add(sourceDir+"/remoll/build/remoll",arcname="remoll")
    tar.add(sourceDir+"/remoll/build/libremoll_rdict.pcm",arcname="libremoll_rdict.pcm")
    tar.add(sourceDir+"/remoll/build/libremoll.so",arcname="libremoll.so")
    #tar.add(sourceDir+"/rad_analysis/pruneTree",arcname="pruneTree")
    #tar.add(sourceDir+"/macros/runexample_"+ident+".mac",arcname="runexample_"+ident+".mac") 
    #runexample overwrite could crash
    tar.add(sourceDir+"/remoll/map_directory",arcname="map_directory")
    #tar.add(sourceDir+"/remoll/geometry",arcname="geometry")
    #tar.add(sourceDir+"/remoll/geometry/schema",arcname="geometry/schema")
    tar.add(sourceDir+"/remoll/geometry/materials.xml",arcname="geometry/materials.xml")
    tar.add(sourceDir+"/remoll/geometry/matrices.xml",arcname="geometry/matrices.xml")
    tar.add(sourceDir+"/remoll/geometry/positions.xml",arcname="geometry/positions.xml")
    tar.add(sourceDir+"/remoll/geometry/solids/world.xml",arcname="geometry/solids/world.xml")
    tar.add(sourceDir+"/remoll/geometry/mollerParallel.gdml" ,arcname="geometry/mollerParallel.gdml") 
    tar.add(sourceDir+"/remoll/geometry/mollerMother_merged.gdml" ,arcname="geometry/mollerMother_merged.gdml") 
    tar.add(sourceDir+"/remoll/geometry/target/subTargetRegion.gdml" ,arcname="geometry/target/subTargetRegion.gdml") 
    tar.add(sourceDir+"/remoll/geometry/hall/hallDaughter_merged.gdml" ,arcname="geometry/hall/hallDaughter_merged.gdml")
    tar.add(sourceDir+"/remoll/geometry/hall/hallDaughter_dump.gdml" ,arcname="geometry/hall/hallDaughter_dump.gdml")
    tar.add(sourceDir+"/remoll/geometry/hall/subDumpDiffuser.gdml" ,arcname="geometry/hall/subDumpDiffuser.gdml")
    tar.add(sourceDir+"/remoll/geometry/upstream/upstreamDaughter_merged.gdml" ,arcname="geometry/upstream/upstreamDaughter_merged.gdml")
    tar.add(sourceDir+"/remoll/geometry/upstream/upstreamToroid.gdml" ,arcname="geometry/upstream/upstreamToroid.gdml")
    tar.add(sourceDir+"/remoll/geometry/upstream/upstreamBeampipe.gdml" ,arcname="geometry/upstream/upstreamBeampipe.gdml")
    tar.add(sourceDir+"/remoll/geometry/hybrid/hybridToroid.gdml" ,arcname="geometry/hybrid/hybridToroid.gdml")
    tar.add(sourceDir+"/remoll/geometry/hybrid/hybridDaughter_merged.gdml" ,arcname="geometry/hybrid/hybridDaughter_merged.gdml")
    tar.add(sourceDir+"/remoll/geometry/huts/lefthut.gdml" ,arcname="geometry/huts/lefthut.gdml")
    tar.add(sourceDir+"/remoll/geometry/huts/righthut.gdml" ,arcname="geometry/huts/righthut.gdml")
    tar.add(sourceDir+"/remoll/geometry/showermax/showerMaxGen.gdml" ,arcname="geometry/showermax/showerMaxGen.gdml")
    tar.add(sourceDir+"/remoll/geometry/pion/pionDetectorSystem.gdml" ,arcname="geometry/pion/pionDetectorSystem.gdml")
    tar.add(sourceDir+"/remoll/geometry/beampipe/downstream/beampipeDSMother.gdml" ,arcname="geometry/beampipe/downstream/beampipeDSMother.gdml")

    tar.close()

if __name__ == '__main__':
    main()

