C
C--- Flags to control the job
C
C
      INTEGER    MXPGEANT,MXPKC
      PARAMETER (MXPGEANT=1000,MXPKC=1000)
C
      COMMON/PHCTRL/ NEVENT,RUNNO,EPH_LIM(2),EELEC,EPEAK,ZCOLL,EPYMIN,ELOWMIN
     +              ,DCOLL
     +              ,IWROUT(3),LUNWR(3),IRND_SEQ,NPRIEV
     +              ,IDBEAM,NHBEA
     +              ,IFPYTH
     +              ,IDLOWEN
     +              ,IPLUND(MXPGEANT),IDECLUND(MXPGEANT)
     +              ,KCGEAN(-MXPKC:MXPKC)
     +              ,RATESEC
     +              ,VERTEX(3)
      INTEGER   NEVENT    ! number of events to simulate (F)
     +         ,RUNNO     ! run number to use
     +         ,IWROUT    ! (1)>0 - write out the HDDS file (events) (F)
C                           (2)>0 - write out a sequential file
C                           (3)>0 - write out an ntuple file
     +         ,LUNWR     ! (1) LUN for HDDS file
C                           (2) LUN for the sequential file
C                           (3) LUN for the ntuple file
     +         ,IRND_SEQ  ! the random number sequence (each integer number gives a different sequence) (F)
     +         ,NPRIEV    ! number of events to print
     +         ,IDBEAM    ! histogram ID for the beam (=0 - fixed energy) (F)
     +         ,NHBEA     ! number of bins in IDBEAM
     +         ,IFPYTH    ! PYTHIA is used
     +         ,IDLOWEN   ! <>0 - starting ID of histograms for the low energy generator
      INTEGER   IPLUND    ! PYTHIA particle codes (KF) 
     +         ,IDECLUND  !  =0 - forbid the decays of this particle in PYTHIA
      INTEGER   KCGEAN    ! GEANT code for the PYTHIA internal code KC (with sign)
      REAL      EPH_LIM ! limits on the photon beam energy GeV (F)
     +         ,EELEC   ! electron beam energy (F)
     +         ,EPEAK   ! energy of the coherent peak (the right edge) (F) 
     +         ,DCOLL   ! collimator diameter (m) (F)
     +         ,ZCOLL   ! distance to the collimator (cm) (F)
     +         ,EPYMIN  ! minimal energy for PYTHIA, (F)
C                         below that the low energy generator is used
C                         the value may be adjusted to the bin boundary of IDBEA
     +         ,ELOWMIN ! minimal energy for the low energy generator (F) 
     +         ,RATESEC ! reference interation rate (Hz), calculated
     +         ,VERTEX  ! Vertex set in HDDM output file (cm)  n.b. 0,0,0 is the default which means hdgeant will use its default, 0,0,65
C


