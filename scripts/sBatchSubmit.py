#!/apps/bin/python3
from subprocess import call
import sys, os, time, tarfile

def main():

    email = "vdoomra@jlab.org"

    config = "moller_shieldConf17"

    sourceDir = "/work/halla/moller12gev/vdoomra/remoll"
    outDir = "/volatile/halla/moller12gev/vdoomra/"+config

    activeDetectors = [28, 30, 99, 101, ## MD, GEMs, roof, wall
                       5530, 5543, 5555, 5556, 5510, ##tgt
                       5603, 5613, 5614, 5619, 5620, ##coll1
                       5695, 5696, 5697, 5698, 5699, 5705, 5706, 5707, 5708, ##dsCoil
                       5714, ##hybridbottom
                       5719, 5720, ##driftPipe
                       5521, ##RasterBox
                       5840, 5842, 5843, 5845, 5850, 5852, 5853, ##PSBunkerVirtualPlanes
                       5860, 5861, 5862, 5863, 5864, 5865, 5866, 5867] ##PSBunkerBoxes
                       

    if not os.path.exists(outDir):
       os.makedirs(outDir)
    nrEv   = 100000 #100000
    nrStart= 0
    nrStop = 4 #(nrStop -nrStart)
    submit  = 0 ## submit is 1 to submit job, submit is 0 to create folder without submitting the jobs

    print('Running ' + str(nrEv*(nrStop - nrStart)) + ' events...')

    jobName=config + '_%03dkEv'%(nrEv/1000)

    ###tar exec+geometry
    make_tarfile(sourceDir)
    call(["cp",sourceDir+"/jobs/default.tar.gz",
          outDir+"/default.tar.gz"])

    for jobNr in range(nrStart,nrStop): # repeat for jobNr jobs
        print("Starting job setup for jobID: " + str(jobNr))

        jobFullName = jobName + '_%04d'%jobNr
        outDirFull=outDir+"/"+jobFullName
        createMacFile(sourceDir, outDirFull,nrEv,jobNr,activeDetectors)

        createSBATCHfile(sourceDir,outDirFull,jobName,jobNr)

        if submit==1:
            print("submitting", jobName)
            call(["sbatch",sourceDir+"/jobs/"+jobName+".sh"])

    print("All done for config ",config," for #s between ",nrStart, " and ", nrStop)


def createMacFile(srcDir, outDirFull,nrEv,jobNr, detectorList):

    if not os.path.exists(outDirFull):
        os.makedirs(outDirFull)

    f=open(outDirFull+"/"+"macro.mac",'w')
    f.write("/remoll/setgeofile geometry/mollerMother_merged.gdml\n")
    f.write("/remoll/physlist/register QGSP_BERT_HP\n")
    f.write("/remoll/physlist/parallel/enable\n")
    f.write("/remoll/parallel/setfile geometry/mollerParallel.gdml\n")
    #f.write("/run/numberOfThreads 16\n")
    f.write("/run/initialize\n")
    f.write("/remoll/addfield "+srcDir+"/map_directory/V2U.1a.50cm.txt\n")
    f.write("/remoll/addfield "+srcDir+"/map_directory/V2DSg.9.75cm.txt\n")
    f.write("/remoll/evgen/set beam\n")
    f.write("/remoll/evgen/beam/origin 0 0 -7.5 m\n")
    f.write("/remoll/evgen/beam/rasx 5 mm\n")
    f.write("/remoll/evgen/beam/rasy 5 mm\n")
    f.write("/remoll/evgen/beam/corrx 0.065\n")
    f.write("/remoll/evgen/beam/corry 0.065\n")
    f.write("/remoll/evgen/beam/rasrefz -4.5 m\n")
    f.write("/remoll/beamene 11 GeV\n")
    f.write("/remoll/beamcurr 65 microampere\n")
    f.write("/remoll/SD/disable_all\n")

    for det in detectorList:
        f.write("/remoll/SD/enable "+str(det)+"\n")
        f.write("/remoll/SD/detect lowenergyneutral "+str(det)+"\n")
        f.write("/remoll/SD/detect secondaries "+str(det)+"\n")
        f.write("/remoll/SD/detect boundaryhits "+str(det)+"\n")

    f.write("/remoll/kryptonite/volume Lbox1_log\n")
    f.write("/remoll/kryptonite/volume Lbox2_log\n")
    f.write("/remoll/kryptonite/volume Lbox3_log\n")
    f.write("/remoll/kryptonite/volume Lbox4_log\n")
    f.write("/remoll/kryptonite/volume Lbox5_log\n")
    f.write("/remoll/kryptonite/volume Sbox1_log\n")
    f.write("/remoll/kryptonite/volume Sbox2_log\n")
    f.write("/remoll/kryptonite/volume Sbox3_log\n")
    f.write("/remoll/kryptonite/enable\n")
    f.write("/process/list\n")
    f.write("/remoll/seed "+str(int(time.clock_gettime(0)) + jobNr)+"\n")
    f.write("/remoll/filename o_remoll.root\n")
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
    f.write("#SBATCH --exclude=farm19104,farm19105,farm19106,farm19107,farm1996,farm19101\n")
    f.write("cd "+outDirFull+"\n")
    f.write("cp ../default.tar.gz ./\n")
    f.write("tar -zxvf default.tar.gz\n")
    f.write("./remoll macro.mac\n")
    f.write("rm -rf default.tar.gz geometry libremoll.so macro.mac remoll\n")
    f.close()
    return 0

def make_tarfile(sourceDir):
    print("making geometry tarball")
    if os.path.isfile(sourceDir+"/jobs/default.tar.gz"):
        os.remove(sourceDir+"/jobs/default.tar.gz")
    tar = tarfile.open(sourceDir+"/jobs/default.tar.gz","w:gz")
    tar.add(sourceDir+"/build/remoll",arcname="remoll")
    tar.add(sourceDir+"/build/libremoll.so",arcname="libremoll.so")
    tar.add(sourceDir+"/geometry/materials.xml",arcname="geometry/materials.xml")
    tar.add(sourceDir+"/geometry/matrices.xml",arcname="geometry/matrices.xml")
    tar.add(sourceDir+"/geometry/positions.xml",arcname="geometry/positions.xml")
    tar.add(sourceDir+"/geometry/solids/world.xml",arcname="geometry/solids/world.xml")
    tar.add(sourceDir+"/geometry/mollerParallel.gdml" ,arcname="geometry/mollerParallel.gdml") 
    tar.add(sourceDir+"/geometry/mollerMother_merged.gdml" ,arcname="geometry/mollerMother_merged.gdml") 
    tar.add(sourceDir+"/geometry/target/subTargetRegion.gdml" ,arcname="geometry/target/subTargetRegion.gdml") 
    tar.add(sourceDir+"/geometry/electronics/subSBSbunker.gdml" ,arcname="geometry/electronics/subSBSbunker.gdml")
    tar.add(sourceDir+"/geometry/electronics/subPSbunker.gdml" ,arcname="geometry/electronics/subPSbunker.gdml")
    tar.add(sourceDir+"/geometry/hall/hallDaughter_merged.gdml" ,arcname="geometry/hall/hallDaughter_merged.gdml")
    tar.add(sourceDir+"/geometry/hall/hallDaughter_dump.gdml" ,arcname="geometry/hall/hallDaughter_dump.gdml")
    tar.add(sourceDir+"/geometry/hall/subDumpDiffuser.gdml" ,arcname="geometry/hall/subDumpDiffuser.gdml")
    tar.add(sourceDir+"/geometry/upstream/upstreamDaughter_merged.gdml" ,arcname="geometry/upstream/upstreamDaughter_merged.gdml")
    tar.add(sourceDir+"/geometry/upstream/upstream_nose_shield_beampipe.gdml" ,arcname="geometry/upstream/upstream_nose_shield_beampipe.gdml")
    tar.add(sourceDir+"/geometry/upstream/inner_upstream_nose_shield_beampipe.gdml" ,arcname="geometry/upstream/inner_upstream_nose_shield_beampipe.gdml")
    tar.add(sourceDir+"/geometry/upstream/upstream.gdml" ,arcname="geometry/upstream/upstream.gdml")
    tar.add(sourceDir+"/geometry/upstream/upstreamToroid.gdml" ,arcname="geometry/upstream/upstreamToroid.gdml")
    tar.add(sourceDir+"/geometry/upstream/upstreamTorusRegion.gdml" ,arcname="geometry/upstream/upstreamTorusRegion.gdml")
    tar.add(sourceDir+"/geometry/upstream/inner_upstream_nose_shield_beampipe.gdml" ,arcname="geometry/upstream/inner_upstream_nose_shield_beampipe.gdml")
    tar.add(sourceDir+"/geometry/upstream/upstream_nose_shield_beampipe.gdml" ,arcname="geometry/upstream/upstream_nose_shield_beampipe.gdml")
    tar.add(sourceDir+"/geometry/upstream/upstream_Conf6.gdml" ,arcname="geometry/upstream/upstream_Conf6.gdml")
    tar.add(sourceDir+"/geometry/upstream/upstream_Conf7.gdml" ,arcname="geometry/upstream/upstream_Conf7.gdml")
    tar.add(sourceDir+"/geometry/upstream/upstream_Conf8.gdml" ,arcname="geometry/upstream/upstream_Conf8.gdml")
    tar.add(sourceDir+"/geometry/upstream/upstreamBeampipe.gdml" ,arcname="geometry/upstream/upstreamBeampipe.gdml")
    tar.add(sourceDir+"/geometry/hybrid/hybridToroid.gdml" ,arcname="geometry/hybrid/hybridToroid.gdml")
    tar.add(sourceDir+"/geometry/hybrid/hybridDaughter_merged.gdml" ,arcname="geometry/hybrid/hybridDaughter_merged.gdml")
    tar.add(sourceDir+"/geometry/huts/lefthut.gdml" ,arcname="geometry/huts/lefthut.gdml")
    tar.add(sourceDir+"/geometry/showermax/showerMaxGen.gdml" ,arcname="geometry/showermax/showerMaxGen.gdml")
    tar.add(sourceDir+"/geometry/pion/pionDetectorSystem.gdml" ,arcname="geometry/pion/pionDetectorSystem.gdml")
    tar.add(sourceDir+"/geometry/beampipe/downstream/beampipeDSMother.gdml" ,arcname="geometry/beampipe/downstream/beampipeDSMother.gdml")
    tar.add(sourceDir+"/geometry/beampipe/premoller/beampipeRaster.gdml" ,arcname="geometry/beampipe/premoller/beampipeRaster.gdml")
    tar.close()

if __name__ == '__main__':
    main()

