      REAL FUNCTION P_KIN_AUTO(IFL,KGEAN)
C
C---      Find the recoil candidate, the type ABS(KGEAN) (=14 - proton)
C---      KGEAN>0 - not originated from a resonance
C---           <0 - all
C---      Kinematic variables:
C
C        IFL=1 - -t  
C           =2 - eff. mass of the rest
C
      IMPLICIT NONE
      INTEGER IFL,KGEAN
C
      INCLUDE ?
C
      INTEGER ip,ip1,ipm,i,j
      REAL var,qq,en1,en2,p1(5),p2(5),tt,efmr,ptar(5)
C
      P_KIN_AUTO=-20.
      IF(KGEAN.EQ.0) GO TO 999
      ip1=0
      DO ip=1,NP
         IF(ITYP(1,ip).EQ.ABS(KGEAN)) THEN
            ipm=ITYP(4,ip)
            IF(KGEAN.LT.0) THEN
               ip1=ip
            ELSE
               IF(ipm.EQ.0) THEN
                  ip1=ip
               ELSE IF(ipm.GT.0.AND.ipm.LE.NP) THEN
C                  IF(ABS(ITYP(4,ipm)).LE.100) THEN
                  IF(ABS(ITYP(4,ipm)).EQ.0) THEN
                     ip1=ip
                  ENDIF
               ENDIF
            ENDIF
         ENDIF
      ENDDO
C
      IF(ip1.GT.0) THEN
         qq=0.
         DO j=1,3
            p1(j)=POUT(j,ip1)
            qq=qq+p1(j)**2
         ENDDO
         p1(4)=SQRT(qq+AM(ip1)**2)
         p1(5)=SQRT(qq)
C
         DO j=1,4
            p2(j)=0.
         ENDDO
         DO ip=1,NP
            IF(ip.NE.ip1) THEN
               IF(ITYP(1,ip).GT.0.AND.ITYP(5,ip).EQ.0) THEN
                  qq=0.
                  DO j=1,3
                     p2(j)=p2(j)+POUT(j,ip)
                     qq=qq+POUT(j,ip)**2
                  ENDDO
                  en2=SQRT(qq+AM(ip)**2)
                  p2(4)=p2(4)+en2
               ENDIF
            ENDIF
         ENDDO
C
         qq=0.
         DO j=1,3
            ptar(j)=PIN(j,2)
            qq=qq+PIN(j,2)**2
         ENDDO
         ptar(4)=SQRT(qq+AMIN(2)**2)
C
         efmr=SQRT(p2(4)**2-p2(1)**2-p2(2)**2-p2(3)**2)
         tt=AM(ip1)+AMIN(2)**2-2.*ptar(4)*p1(4)
         DO j=1,3
            tt=tt+2.*ptar(j)*p1(j)
         ENDDO
C
         IF(IFL.EQ.1) THEN
            var=-tt
         ELSE IF(IFL.EQ.2) THEN
            var=efmr
         ENDIF
         P_KIN_AUTO=var
C      
      ENDIF
C
 999  RETURN
C
      END
C
C      INCLUDE 'efmass.f'
C      INCLUDE 'gloren.f'
