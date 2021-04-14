#!/usr/bin/python
from subprocess import call
import sys, os, time, tarfile

def main():

    email = "palatchi@jlab.org"

    config = "remoll_bellows_beam_100M"

    sourceDir = "/w/moller12gev-sciwork18/palatchi/moller/remoll/"
    outDir = "/volatile/halla/moller12gev/palatchi/"+config

    activeDetectors = [28, 70, 71, 72, 73, 74, 75, 76]

    if not os.path.exists(outDir):
       os.makedirs(outDir)
    nrEv   = 100000 #900000 100k is 10hours
    nrStart= 1
    nrStop = 1001 #(nrStop -nrStart) total jobs
    submit  = 1

    print('Running ' + str(nrEv*(nrStop - nrStart)) + ' events...')

    jobName=config + '_%02dkEv'%(nrEv/1000)

    ###tar exec+geometry
    make_tarfile(sourceDir)

    for jobNr in range(nrStart,nrStop): # repeat for jobNr jobs
        print("Starting job setup for jobID: " + str(jobNr))

        jobFullName = jobName + '_%03d'%jobNr
        outDirFull=outDir+"/"+jobFullName
        createMacFile(outDirFull,nrEv,jobNr,activeDetectors)

        call(["cp",sourceDir+"/jobs/default.tar.gz",
              outDir+"/"+jobFullName+"/default.tar.gz"])

        createSBATCHfile(sourceDir,outDirFull,jobName,jobNr)

        if submit==1:
            call(["sbatch",sourceDir+"/jobs/"+jobName+".sh"])

    print "All done for config ",config," for #s between ",nrStart, " and ", nrStop


def createMacFile(outDirFull,nrEv,jobNr, detectorList):

    if not os.path.exists(outDirFull):
        os.makedirs(outDirFull)

    f=open(outDirFull+"/"+"macro.mac",'w')

    f.write("/remoll/geometry/setfile geometry/mollerMother.gdml\n")
    f.write("/remoll/parallel/setfile geometry/mollerParallel.gdml \n")
    f.write("/remoll/physlist/parallel/enable \n")
    f.write("/run/initialize\n")
    f.write("/remoll/printgeometry true\n")
    f.write("/remoll/addfield map_directory/V1DSg.3.txt\n")
    f.write("/remoll/addfield map_directory/V1U.2a.txt\n")
#?    f.write("/remoll/oldras true\n")

    f.write("/remoll/evgen/beam/origin 0 0 -7.5 m\n")
    f.write("/remoll/evgen/beam/rasx 5 mm\n")
    f.write("/remoll/evgen/beam/rasy 5 mm\n")
    f.write("/remoll/evgen/beam/corrx 0.065\n")
    f.write("/remoll/evgen/beam/corry 0.065\n")
    f.write("/remoll/evgen/beam/rasrefz -4.5 m\n")
    f.write("/remoll/beamene 11 GeV\n")
    f.write("/remoll/beamcurr 65 microampere\n")
#    f.write("/remoll/evgen/set elastic\n")
    f.write("/remoll/evgen/set beam\n")   
    f.write("/remoll/evgen/beamPolarization +L\n")
    f.write("/remoll/field/equationtype 2\n")
    f.write("/remoll/field/steppertype 2\n")
    f.write("/remoll/field/print\n")
    f.write("/remoll/beamene 11 GeV\n")
    f.write("/remoll/beamcurr 65 microampere\n")

#add from cip job
    f.write("/remoll/SD/disable_all\n")
    for det in detectorList:
        f.write("/remoll/SD/enable "+str(det)+"\n")
        f.write("/remoll/SD/detect lowenergyneutral "+str(det)+"\n")
        f.write("/remoll/SD/detect secondaries "+str(det)+"\n")
        f.write("/remoll/SD/detect boundaryhits "+str(det)+"\n")  

    f.write("/remoll/kryptonite/enable\n")
    f.write("/remoll/kryptonite/add VacuumKryptonite\n")
    f.write("/remoll/kryptonite/add Tungsten\n")
    f.write("/remoll/kryptonite/add Copper\n")
    f.write("/remoll/kryptonite/add Lead\n")
    f.write("/remoll/kryptonite/add CW95\n")
    f.write("/remoll/kryptonite/list\n")
    f.write("/process/list\n")
    f.write("/remoll/seed "+str(jobNr)+"\n")
    f.write("/remoll/filename remollout.root\n")
    f.write("/remoll/printtargetinfo\n")
    f.write("/run/beamOn "+str(nrEv)+"\n")

    f.close()
    return 0

def createSBATCHfile(sourceDir,outDirFull,jobName,jobNr):

    if not os.path.exists(sourceDir+"/jobs"):
        os.makedirs(sourceDir+"/jobs")

    f=open(sourceDir+"/jobs/"+jobName+".sh","w")
    f.write("#!/bin/bash\n")
    f.write("#SBATCH --ntasks=1\n")
    f.write("#SBATCH --job-name="+jobName+'_%03d'%jobNr+"\n")
    f.write("#SBATCH --output="+outDirFull+"/log.out\n")
    f.write("#SBATCH  --error="+outDirFull+"/log.err\n")
    #f.write("#SBATCH --partition=priority\n")
    f.write("#SBATCH --partition=production\n")
    f.write("#SBATCH --account=halla\n")
    f.write("#SBATCH --mem-per-cpu=5000\n")
    f.write("cd "+outDirFull+"\n")
    f.write("tar -zxvf default.tar.gz\n")
    f.write("./remoll macro.mac\n")
    f.close()
    return 0

def make_tarfile(sourceDir):
    print "making geometry tarball"
    if os.path.isfile(sourceDir+"/jobs/default.tar.gz"):
        os.remove(sourceDir+"/jobs/default.tar.gz")
    tar = tarfile.open(sourceDir+"/jobs/default.tar.gz","w:gz")
    tar.add(sourceDir+"/build/remoll",arcname="remoll")
    tar.add(sourceDir+"/build/libremoll.so",arcname="libremoll.so")
#    tar.add(sourceDir+"/map_directory//hybridJLAB.txt",arcname="map_directory/hybridJLAB.txt")
#    tar.add(sourceDir+"/map_directory/upstreamJLAB_1.25.txt",arcname="map_directory/upstreamJLAB_1.25.txt")
    tar.add(sourceDir+"/map_directory/V1DSg.3.txt",arcname="map_directory/V1DSg.3.txt")
    tar.add(sourceDir+"/map_directory/V1U.2a.txt",arcname="map_directory/V1U.2a.txt")
    tar.add(sourceDir+"/geometry/materials.xml",arcname="geometry/materials.xml")
    tar.add(sourceDir+"/geometry/matrices.xml",arcname="geometry/matrices.xml")
    tar.add(sourceDir+"/geometry/positions.xml",arcname="geometry/positions.xml")
    tar.add(sourceDir+"/geometry/solids/world.xml",arcname="geometry/solids/world.xml")
    tar.add(sourceDir+"/geometry/mollerParallel.gdml" ,arcname="geometry/mollerParallel.gdml") 
    tar.add(sourceDir+"/geometry/mollerMother.gdml" ,arcname="geometry/mollerMother.gdml") 
    tar.add(sourceDir+"/geometry/donut/donutConcreteLead.gdml" ,arcname="geometry/donut/donutConcreteLead.gdml") 
    tar.add(sourceDir+"/geometry/target/subTargetRegion.gdml" ,arcname="geometry/target/subTargetRegion.gdml") 
    tar.add(sourceDir+"/geometry/electronics/subSBSbunker.gdml" ,arcname="geometry/electronics/subSBSbunker.gdml") 
    tar.add(sourceDir+"/geometry/hall/hallDaughter_merged.gdml" ,arcname="geometry/hall/hallDaughter_merged.gdml")
    tar.add(sourceDir+"/geometry/hall/hallDaughter_dump.gdml" ,arcname="geometry/hall/hallDaughter_dump.gdml")
    tar.add(sourceDir+"/geometry/hall/subDumpDiffuser.gdml" ,arcname="geometry/hall/subDumpDiffuser.gdml")
    tar.add(sourceDir+"/geometry/upstream/upstreamDaughter_merged.gdml" ,arcname="geometry/upstream/upstreamDaughter_merged.gdml")
    tar.add(sourceDir+"/geometry/upstream/upstreamToroid.gdml" ,arcname="geometry/upstream/upstreamToroid.gdml")
    tar.add(sourceDir+"/geometry/upstream/upstreamBeampipe.gdml" ,arcname="geometry/upstream/upstreamBeampipe.gdml")
    tar.add(sourceDir+"/geometry/upstream/sieve.gdml" ,arcname="geometry/upstream/sieve.gdml")
    tar.add(sourceDir+"/geometry/hybrid/hybridToroid.gdml" ,arcname="geometry/hybrid/hybridToroid.gdml")
    tar.add(sourceDir+"/geometry/hybrid/hybridDaughter_merged.gdml" ,arcname="geometry/hybrid/hybridDaughter_merged.gdml")
    tar.add(sourceDir+"/geometry/huts/lefthut.gdml" ,arcname="geometry/huts/lefthut.gdml")
    tar.add(sourceDir+"/geometry/showermax/showerMaxGen.gdml" ,arcname="geometry/showermax/showerMaxGen.gdml")
    tar.add(sourceDir+"/geometry/tracking/trackingDaughter.gdml" ,arcname="geometry/tracking/trackingDaughter.gdml")
    tar.add(sourceDir+"/geometry/pion/" ,arcname="geometry/pion/")
    tar.add(sourceDir+"/geometry/pion/TS/" ,arcname="geometry/pion/TS/")
    tar.add(sourceDir+"/geometry/pion/GEM/" ,arcname="geometry/pion/GEM/")
    tar.add(sourceDir+"/geometry/pion/Lucite/" ,arcname="geometry/pion/Lucite/")
    tar.add(sourceDir+"/geometry/beampipe/downstream/beampipeDSMother.gdml" ,arcname="geometry/beampipe/downstream/beampipeDSMother.gdml")
    tar.add(sourceDir+"/geometry/beampipe/premoller/beampipeRaster.gdml" ,arcname="geometry/beampipe/premoller/beampipeRaster.gdml")
    tar.close()

if __name__ == '__main__':
    main()

