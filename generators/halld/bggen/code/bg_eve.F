      SUBROUTINE BG_EVE(IPRI)
C
C---  Simulates one BG event
C     IPRI>0 - print this event
C
      IMPLICIT NONE
      INTEGER IPRI
C
      INCLUDE 'bg_ctrl.inc'
      INCLUDE 'bg_proc.inc'
      INCLUDE 'bg_partc.inc'
      INCLUDE 'bg_evec.inc'
C
      REAL HRNDM1
      LOGICAL HEXIST
C
      INTEGER i,j,ip,ierr,lout,idnt
      REAL ebeam
      CHARACTER cent(2)*6,cproc*16
C
      REAL ptmp1(4,2),ptmp2(4,MXTRA)   ! auxill. arrays to simplify the HDDM mapping
      INTEGER ifl1(6,2),ifl2(6,MXTRA)
C
C     ------------------------------------------------------------------
C
      lout=6
      IEVPROC=-1
      INDUM(1)=0
      INDUM(2)=0
      cent(1)='beam  '
      cent(2)='target'
C
C---    Beam energy
C
      IF(IDBEAM.EQ.0.OR..NOT.HEXIST(IDBEAM)) GO TO 999
C
      ebeam=HRNDM1(IDBEAM)   ! Get the random photon energy
C      ebeam=1.0           ! Manually set the photon energy
C
C---   Beam/target definitions
C
      ITPIN(1,1)=1    ! beam GEANT type
      ITPIN(1,2)=14   ! beam target type
      ITPIN(2,1)=IPLUND(ITPIN(1,1)) ! KF types
      ITPIN(2,2)=IPLUND(ITPIN(1,2))
C
      DO i=1,2
         AMIN(i)=AM_PART(ITPIN(1,i))
      ENDDO
      DO i=1,3
         PIN(i,1)=0.
         PIN(i,2)=0.
      ENDDO
      PIN(3,1)=ebeam
C
      NTRA=0
C
C---   Choose the package
C
      ierr=0
      IF(ebeam.LT.EPYMIN) THEN
C
         CALL LOWEN_EVE(ierr)
         IF(IEVPROC.GT.0) cproc=CNPROC(IEVPROC)
C
      ELSE
C
         CALL PYTH_EVE(ierr)
         cproc='PYTHIA'
C
      ENDIF
      IF(ierr.NE.0) GO TO 999
C
C---  Remove the GEANT type for the decaying particles (KF type is retained)
C             needed to avoid copying these particles into GEANT
C
      DO i=1,NTRA
         IF(ITPTRA(2,i).NE.1.AND.ITPTRA(3,i).NE.0) ITPTRA(1,i)=0
      ENDDO  
C
C---     Print the event
C
      IF(IPRI.NE.0) THEN
C Following three write statements are commented out just to print only xs list. Default output
         WRITE(lout,1000) IEVENT,IEVPROC,cproc
 1000    FORMAT(' Event ',I6,'  Process=',I4,3X,A16)
         WRITE(lout,1005) 
     +     (cent(i),(ITPIN(j,i),j=1,2),AMIN(i),(PIN(j,i),j=1,3),i=1,2)
 1005    FORMAT(1X,A6,3X,I3,2X,I5,3X,4X,F8.4,3X,3F8.3)
         WRITE(lout,1010) 
     +      (i,(ITPTRA(j,i),j=1,6),AMTRA(i),(PTRA(j,i),j=1,3),i=1,NTRA)
 1010    FORMAT(1X,I3,3X,I3,I6,2X,I5,3X,3I4,4X,F8.4,3X,3F8.3)
C     Write only sec. events added by Rakitha
C         WRITE(lout,1011) 
C     +      (IEVENT, i,(ITPTRA(j,i),j=1,6),AMTRA(i),(PTRA(j,i),j=1,3),i=1,NTRA)
C 1011    FORMAT(I6,1X,I3,3X,I3,I6,2X,I5,3X,3I4,4X,F8.4,3X,3F8.3)
C        Write additional information with cross section here added by Rakitha
C         WRITE(lout,1015) IEVENT, XSTRA(1),KETRA(2),KETRA(1),XSTRA(2),XSTRA(3)
C 1015    FORMAT(I6,5X,F8.4,3X,I3,3X,I3,3X,F8.6,5X,F8.6)
C        write only output tracks (secondaries created for each event with the event number as the first column
C         WRITE(lout,1016) 
C     +      (IEVENT,i,(ITPTRA(j,i),j=1,6),AMTRA(i),(PTRA(j,i),j=1,3),i=1,NTRA)
C 1016    FORMAT(I6,5X,I3,3X,I3,I6,2X,I5,3X,3I4,4X,F8.4,3X,3F8.3)

      ENDIF
C
C---   Output file for HDDM
C
      IF(IWROUT(1).NE.0) THEN
         DO i=1,2
            DO j=1,6
               ifl1(j,i)=0
            ENDDO
            ifl1(1,i)=ITPIN(1,i)
            ifl1(3,i)=ITPIN(2,i)
            DO j=1,3
               ptmp1(j,i)=PIN(j,i)
            ENDDO
            ptmp1(4,i)=SQRT(ptmp1(1,i)**2+ptmp1(2,i)**2+ptmp1(3,i)**2
     +                     +AMIN(i)**2)
         ENDDO
         DO i=1,NTRA
            DO j=1,6
               ifl2(j,i)=ITPTRA(j,i)
            ENDDO
C
            DO j=1,3
               ptmp2(j,i)=PTRA(j,i)
            ENDDO
            ptmp2(4,i)=SQRT(ptmp2(1,i)**2+ptmp2(2,i)**2+ptmp2(3,i)**2
     +                     +AMTRA(i)**2)
         ENDDO
         WRITE(6,*) ' WARNING: HDDM output disabled in this version!'
C         CALL WRITE_HDDM_EVENT(RUNNO, IEVENT,IEVPROC
C     +                        ,ifl1(1,1),ptmp1(1,1)
C     +                        ,NTRA,ifl2(1,1),ptmp2(1,1))
C         write(6,1010) (i,(ifl1(j,i),j=1,6),(ptmp1(j,i),j=1,4),i=1,2)
C         write(6,1010) (i,(ifl2(j,i),j=1,6),(ptmp2(j,i),j=1,4),i=1,NTRA)
      ENDIF
C
C---   Sequential output file
C
      IF(IWROUT(2).NE.0) THEN
         WRITE(LUNWR(2)) IEVENT,IEVPROC
     +        ,(( ITPIN(j,i),j=1,2), AMIN(i),( PIN(j,i),j=1,3),i=1,2)
     +   ,NTRA,((ITPTRA(j,i),j=1,6),AMTRA(i),(PTRA(j,i),j=1,3),i=1,NTRA)
      ENDIF
C
C---   NTUPLE
C
      IF(IWROUT(3).NE.0) THEN
         idnt=9
         CALL HFNT(idnt)
      ENDIF
C
 999  CONTINUE
C      write(6,*) ebeam,IEVPROC,ibin,xstot,xssum,NTRA
C
      END
C

