C
C---  Simulated event
C
      INTEGER MXTRA
      PARAMETER (MXTRA=100)
      COMMON/EV_RECORD/ IEVENT,INDUM(2),IEVPROC
     +                 ,ITPIN(2,2),AMIN(2),PIN(3,2)
     +                 ,NTRA 
     +                 ,ITPTRA(6,MXTRA),AMTRA(MXTRA),PTRA(3,MXTRA),KETRA(2),XSTRA(3)
      INTEGER IEVENT   ! event number
     +       ,INDUM    !  dummies (for later use)
     +       ,IEVPROC  ! the process number (=0 - PYTHIA)
     +       ,ITPIN    ! (1,k)=GEANT type, (2,k) - KF (LUND), k=1 - beam, =2 -target 
     +       ,NTRA     ! number of particles including the beam and the target
     +       ,ITPTRA   ! (1,k) track type (GEANT), (2-6,k) - LUND flags (KS,decays)
     +       ,KETRA    ! IEVPROC and ibin
      REAL    AMIN     ! masses of the beam and the target
     +       ,PIN      ! (1-3,k) - 3-momenta, k=1 - beam, k=2 - target
     +       ,AMTRA    ! secondary particles' masses
     +       ,PTRA     ! 3-momenta
     +       ,XSTRA    !  ebeam,xstot,xssum

C

 
