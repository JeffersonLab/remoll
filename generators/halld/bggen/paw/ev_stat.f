      REAL FUNCTION EV_STAT(IFL,KTYP,P1,P2,TH1,TH2)
C
C--       Event features:
C       IFL=0 - multiplicity, 0<IFL<5 - 4 momentum of the sum
C       KTYP=0 - all entries, >0 - GEANT particle type
C       P1,P2,TH1,TH2 - momentum and angle limits
C
      IMPLICIT NONE
      INTEGER IFL,KTYP
      REAL P1,P2,TH1,TH2
C
      INCLUDE ?
C
      INTEGER nm,im(4),i,j,k,ip,nn
      REAL bm(4),pm(4,4),ef
      REAL pp(4),pf,th,qq
C
      EV_STAT=0.
C
      DO j=1,4
         pp(j)=0.
      ENDDO
      nn=0
C
      DO ip=1,NP
         IF(ITYP(1,ip).GT.0) THEN
            IF(KTYP.EQ.0.OR.KTYP.EQ.ITYP(1,ip)) THEN
               qq=0.
               DO j=1,3
                  qq=qq+POUT(j,ip)**2
               ENDDO
               pf=SQRT(qq)
               th=ACOS(POUT(3,ip)/pf)*180./3.1416
               IF(pf.GE.P1 .AND.pf.LE.P2.AND.
     +            th.GE.TH1.AND.th.LE.TH2) THEN
                  DO j=1,3
                     pp(j)=pp(j)+POUT(j,ip)
                  ENDDO
                  pp(4)=pp(4)+SQRT(qq+AM(ip)**2)
                  nn=nn+1
               ENDIF
            ENDIF
         ENDIF
      ENDDO
      
C
      IF(IFL.EQ.0) THEN
         EV_STAT=nn
      ELSE IF(IFL.GE.1.AND.IFL.LE.4) THEN
         EV_STAT=pp(IFL)
      ENDIF
C
      RETURN
      END
