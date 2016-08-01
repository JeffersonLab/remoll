      SUBROUTINE BG_NTUP_INI(IERR)
C
C---      Initialize the ntuple
C
      IMPLICIT NONE
      INTEGER IERR
C
      COMMON/QUEST/ IQUEST(100)
      INTEGER       IQUEST
C
      INCLUDE 'bg_ctrl.inc'
      INCLUDE 'bg_evec.inc'
C
      INTEGER lrec,idnt,iost
      CHARACTER cpar*3
C
      IERR=1
C      lrec=2048
C      IQUEST(10)=128000
      lrec=4096
      IQUEST(10)=128000
      CALL HROPEN(LUNWR(3),'bgkin','bggen.nt','NQE',lrec,iost)
      IF(iost.NE.0) THEN
         WRITE(6,*)'*** ERROR opening NTUPLE, iost=',iost
         GO TO 999
      ENDIF
      idnt=9
      CALL HBNT(idnt,'BGkinem',' ')
      CALL HBNAME(idnt,'run',IEVENT,'ieve')
      CALL HBNAME(idnt,'run',INDUM(1),'irun')
      CALL HBNAME(idnt,'run',INDUM(2),'iend[-120,120]')
      CALL HBNAME(idnt,'bgki',IEVPROC,'iproc')
      CALL HBNAME(idnt,'bgki',ITPIN(1,1) ,'itypin(2,2)')
      CALL HBNAME(idnt,'bgki',AMIN(1) ,'amin(2)')
      CALL HBNAME(idnt,'bgki',PIN(1,1),'pin(3,2)')
      WRITE(cpar,FMT='(I3)') MXTRA
      CALL HBNAME(idnt,'bgki',NTRA,'np[0,'//cpar//']')
      CALL HBNAME(idnt,'bgki',ITPTRA(1,1) ,'ityp(6,np)')
      CALL HBNAME(idnt,'bgki',AMTRA(1) ,'am(np)')
      CALL HBNAME(idnt,'bgki',PTRA(1,1),'pout(3,np)')
C
      IERR=0
 999  RETURN
      END
