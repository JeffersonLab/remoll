C
      SUBROUTINE EFMASS(NPAR,AMS,PPA,EFMS,POUT)
C
C     ******************************************************************
C     *                                                                *
C     * Calculate invariant mass of NPAR particles with masses         *
C     *        AMS and momenta PPA                                     *
C     *      in OMEGA frame, E - the gamma energy                      *
C     * Output: EFMS - inv. mass (Gev), POUT - momentum of combin.     *
C     *                                                                *
C     ******************************************************************
C
      IMPLICIT NONE
      INTEGER NPAR
      REAL AMS(*),PPA(4,*),EFMS,POUT(4)
C
      INTEGER ip
      DOUBLE PRECISION detot,dpxt,dpyt,dpzt,dpx,dpy,dpz,dm
C
      detot=0.
      dpxt=0.
      dpyt=0.
      dpzt=0.
      DO ip=1,NPAR
        dpx=PPA(1,ip)
        dpy=PPA(2,ip)
        dpz=PPA(3,ip)
        dm=AMS(ip)
        dpxt=dpxt+dpx
        dpyt=dpyt+dpy
        dpzt=dpzt+dpz
        detot=detot+DSQRT(dpx**2+dpy**2+dpz**2+dm**2)
      END DO
      EFMS=DSQRT(detot**2-dpxt**2-dpyt**2-dpzt**2)
      POUT(1)=dpxt
      POUT(2)=dpyt
      POUT(3)=dpzt
      POUT(4)=detot
C
      RETURN
      END
