      SUBROUTINE PYTH_INI(IERR)
C
C---  Initilize PYTHIA
C     Reaction: gamma+p
C---  Input: /phctrl/ beam energy 
C            file "pythia-geant.dat" contains a table for PYTHIA<->GEANT particle code conversion
C            file "pythia.dat" - redefinition of PYTHIA parameters (from HERMES, adapted to GLUEX)           
C
      IMPLICIT NONE
      INTEGER IERR
C
      INCLUDE 'bg_ctrl.inc'
C
      INTEGER PYCOMP
      EXTERNAL PYCOMP
C
      DOUBLE PRECISION dbeam
      CHARACTER cpar*100
C
      INTEGER lun,lout,i,j,lenc,kf,kc,ks,iost
C
C     ------------------------------------------------------------------
C
      IERR=1
C
      lun=9
      lout=6
C
C---    KF/KC/GEANT mapping
C
      DO i=1,MXPGEANT
         kf=IPLUND(i)
         IF(kf.NE.0) THEN
            kc=PYCOMP(kf)
            IF(kc.GT.0.AND.kc.LE.MXPKC) THEN
               IF(kf.LT.0) kc=-kc
               KCGEAN(kc)=i
               kc=ABS(kc)
C                  write(lout,FMT='(10I8)') i,kf,kc
C
C---             Forbid the decays for particles with GEANT code
C
               IF(IDECLUND(i).EQ.0) THEN
                  WRITE(cpar,1000) kc,0
 1000             FORMAT('MDCY(',I4,',1)=',I2)
                  CALL PYGIVE(cpar)
               ENDIF
            ENDIF
         ENDIF
      ENDDO
C 
C---   Read the pythia settings for JLab energies
C      
      OPEN(lun,FILE='pythia.dat',STATUS='OLD',IOSTAT=iost
     +       ,FORM='FORMATTED')
      IF(iost.NE.0) THEN
         WRITE(lout,*) ' *** ERROR: Missing file pythia.dat'
         GO TO 999
      ENDIF
 30   READ(lun,'(A)',IOSTAT=iost) cpar
      IF(iost.EQ.0) THEN
         CALL PYGIVE(cpar)
         GO TO 30
      ELSE IF(iost.GT.0) THEN
         WRITE(lout,*) ' *** ERROR: Reading file pythia.dat'
         GO TO 999
      ENDIF
      CLOSE(lun)
C
C---      Variable energy?
C
      IF(NHBEA.GT.1) THEN
         cpar='MSTP(171)=1'
         CALL PYGIVE(cpar)
         cpar='MSTP(172)=1'
         CALL PYGIVE(cpar)
      ENDIF
C
C---   Initialize PYTHIA
C
      dbeam=DBLE(EPH_LIM(2))
C
      CALL PYINIT('FIXT','gamma','p+',dbeam)
C
      IERR=0
 999  CONTINUE
C
      END

