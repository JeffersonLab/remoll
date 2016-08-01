C
C---  BG processes definitions
C
      INTEGER MXPROC,MXOUT
      PARAMETER (MXPROC=10,MXOUT=6)
      COMMON/BG_PROC/  ITYPROC(MXOUT,MXPROC)
      COMMON/BG_PROC1/ CNPROC(MXPROC)
C
      INTEGER ITYPROC      ! (1:6,iproc) - GEANT types (or 0) of the secondary particles for process iproc
      CHARACTER CNPROC*16  ! (iproc)     - the process description (name)
