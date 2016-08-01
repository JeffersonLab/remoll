C
      SUBROUTINE COHBEAM_INI(ID,E0,EP,ELIM,ZCOLLIM,COLDIAM)
C
C---   Photoproduction by the coherent Brem. beam
C---   ID - histogram with the dN/dE*sigma(E), 
C              where dN/dE - coh. Brem., sigma(E) - total photoprod. on protons 
C      E0 - e- energy
C      EP - coherent peak energy
C      ELIM - energy limits
C      ZCOLLIM - distance to the collimator
C      COLDIAM - collimator diameter
C
      IMPLICIT NONE
      INTEGER ID
      REAL E0,EP,ELIM(2),ZCOLLIM,COLDIAM
C
      INCLUDE 'bg_ctrl.inc'
C
      REAL DNIDX,DNCDX,GPXSECT
C
      INTEGER i,nb,ibrem
      REAL emn,emx,flx,xsec,dx,de,e,xstot,xlum,ecurr,targth,frate,vv
C
      ibrem=1
      IF(ELIM(2).LT.ELIM(1)*1.006) ibrem=0
C
C---   Initialize the coherent Bremsstrahlung 
C
      targth=30.                ! target thickness
      ecurr=2.25E-6             ! beam current on a 20um crystal - the "high luminosity" setting
      xlum=ecurr/1.6E-19*targth*0.071*0.602 ! luminosity factor:  2.25uA on 20um (1.7e-4 RL) crystal, 30 cm LH2 (1/b) 
C
      IF(ibrem.NE.0) THEN
         CALL COBREMS(E0,EP,ZCOLLIM/100.,COLDIAM) ! collimator distance in m
      ENDIF
C
      emn=ELIM(1)
      emx=ELIM(2)
      de=MIN(0.005,emx-emn)
      dx=de/E0
      nb=INT((emx-emn)/de+0.001)
      NHBEA=nb
C
      CALL HBOOK1(ID   ,'Beam flux dn/dE*sigma(E)' ,nb,emn,emx,0.)
      CALL HBOOK1(ID+10,'Total cross section in mb',nb,emn,emx,0.)
      CALL HBOOK1(ID+11,'Beam flux dn/dE'          ,nb,emn,emx,0.)
C
      frate=0.
      DO i=1,nb
         e=ELIM(1)+(i-0.5)*de
         xstot=GPXSECT(e)*1.E-3 ! x-sec in b
         IF(ibrem.NE.0) THEN
            flx=(DNIDX(e/E0)+DNCDX(e/E0))*dx/de
            vv=xstot*flx*xlum
            frate=frate+vv*de
            CALL HF1(ID+11,e,flx)
         ELSE
            vv=1.
         ENDIF
         CALL HF1(ID   ,e,vv)
         CALL HF1(ID+10,e,xstot)
      ENDDO
C      CALL HPRINT(ID)
C
      RATESEC=0.
      IF(ibrem.NE.0) THEN
         RATESEC=frate
         WRITE(6,FMT='(//10X,''Rates:'')')
         WRITE(6,1000) ecurr*1.E6,emn,emx
 1000    FORMAT(10X,'Beam: ',F8.2,' uA e-, gamma in ',2F6.2,' GeV') 
         WRITE(6,1005) targth
 1005    FORMAT(10X,'Target: ',F8.2,' cm LH2')
         WRITE(6,1010) frate/1000.
 1010    FORMAT(10X,'Interaction rate: ',F8.1,' kHz')
      ENDIF 
C
      CALL HCOPY(ID,ID+1,'    ')  ! a copy of the final histogram to be used for HRNDM1
      Call hrput(0,'cohbrems.hbk','N')
C
 999  RETURN
      END
