C
C---  Read the file produced with BGG
C---   Prints events
C
      REAL FUNCTION BGG_PRI(IFL)
C
      IMPLICIT NONE
      INTEGER IFL
C
      INCLUDE ?
C
      INTEGER ip,i,j
      CHARACTER cent(2)*6,cproc*16
C
C     ------------------------------------------------------------------
C
C---   Open file
C
      BGG_PRI=0.
      cent(1)='beam  '
      cent(2)='target'
      WRITE(6,1000) ieve,irun,iend,iproc
 1000 FORMAT(' Event ',I6,' Run ',I4,' End ',I4,'  Process=',I4)
      WRITE(6,1005) 
     +     (cent(i),(itypin(j,i),j=1,2),amin(i),(pin(j,i),j=1,3),i=1,2)
 1005 FORMAT(1X,A6,3X,I3,2X,I5,3X,4X,F8.4,3X,3F8.3)
      WRITE(6,1010) 
     +      (i,(ityp(j,i),j=1,6),am(i),(pout(j,i),j=1,3),i=1,np)
 1010 FORMAT(1X,I3,3X,I3,I6,2X,I5,3X,3I4,4X,F8.4,3X,3F8.3)

C
 999  CONTINUE
C
      END

