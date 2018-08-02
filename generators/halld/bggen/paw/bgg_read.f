C
C---  Example: read the file produced with BGG
C
      PROGRAM READ_BGG
C
      IMPLICIT NONE
C
C
      REAL      beammom
C
      INTEGER lun,lout,i,j,nevent,lenc,iost,iev,ntra
C
      INTEGER mxtra
      PARAMETER (mxtra=10)
      REAL     am(mxtra),pp(3,mxtra),ami(2),ppi(3,2)
      INTEGER  iproc,ityp(6,mxtra),itypi(2,2)
C
C     ------------------------------------------------------------------
C
      lun=9
      lout=6
C
C
C---   Open file
C
      OPEN(lun,FILE='bggen.dat',STATUS='OLD',IOSTAT=iost
     +       ,FORM='UNFORMATTED')
      IF(iost.NE.0) THEN
         WRITE(lout,*) ' *** ERROR: Missing file bggen.dat'
         GO TO 999
      ENDIF
C
      nevent=0
C

 10   READ(lun,IOSTAT=iost) iev,iproc
     +        ,(( itypi(j,i),j=1,2), ami(i),( ppi(j,i),j=1,3),i=1,2)
     +   ,ntra,(( ityp (j,i),j=1,6), am (i),( pp (j,i),j=1,3),i=1,ntra)
      IF(iost.EQ.0) THEN
         nevent=nevent+1
         IF(nevent.LT.50) THEN

            WRITE(lout,1000) iev,iproc
 1000       FORMAT(' Event ',I6,'  Process=',I4)
            WRITE(lout,1005) 
     +       (i,(itypi(j,i),j=1,2),ami(i),(ppi(j,i),j=1,3),i=1,2)
 1005       FORMAT(1X,I6,3X,I3,2X,I5,3X,4X,F8.4,3X,3F8.3)
            WRITE(lout,1010) 
     +       (i,(ityp (j,i),j=1,6),am (i),(pp (j,i),j=1,3),i=1,ntra)
 1010       FORMAT(1X,I3,3X,I3,I6,2X,I5,3X,3I4,4X,F8.4,3X,3F8.3)
         ENDIF
C
         GO TO 10
      ENDIF
C
      WRITE(lout,*) ' Read events =',nevent
C
      CLOSE(lun)
C--
C
 999  CONTINUE
C
      END

