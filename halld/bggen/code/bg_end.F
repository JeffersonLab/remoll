      SUBROUTINE BG_END
C
C---     End of the job for BG simulation
C
      IMPLICIT NONE
C
      INCLUDE 'bg_ctrl.inc'
C
      INTEGER icycle,lun,lrec,idnt,iost
C
      IF(IWROUT(1).NE.0) THEN
        WRITE(6,*) ' WARNING: HDDM output disabled in this version!'
C         CALL CLOSE_HDDM_OUTPUT
      ENDIF
C
      IF(IWROUT(2).NE.0) THEN
         CLOSE(UNIT=LUNWR(2))
         WRITE(6,*) ' Closed output data file LUN=',LUNWR(2)
      ENDIF
C
      IF(IWROUT(3).NE.0) THEN
        icycle=0
        idnt=9
        CALL HCDIR('//bgkin',' ')
        CALL HROUT(idnt,icycle,' ')
        CALL HREND('bgkin')
        CLOSE(UNIT=LUNWR(3))
        WRITE(6,*) ' Closed output ntuple file LUN=',LUNWR(3)
      ENDIF
C
      lun=9
      lrec=1024
      CALL HROPEN(lun,'HISOUT','bggen.his','N',lrec,iost)
      CALL HROUT(0,icycle,'    ')
      CALL HREND('HISOUT')
      CLOSE(UNIT=lun)
        WRITE(6,*) ' Histograms written to file bggen.his'
C
 999  RETURN
      END
