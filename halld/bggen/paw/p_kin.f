      REAL FUNCTION P_KIN(IFL,K1,K2)
C
C---      Kinematic variables:
C
C        IFL=1 - -t  (target --> -(K1-targ)**2)
C           =2 - cos(th) of K1 in CM of K2 with respect to the K2 direction
C                K2=0 - in CM
C
      IMPLICIT NONE
      INTEGER IFL,K1,K2,K3,K4
C
      INCLUDE ?
C
      INTEGER i,j,kf1,kf2
      REAL var,qq,en1,en2,dir(3),p1(5),p2(5),pp1,pp2,px(4),pa(4)
     +    ,bet(4),ptar(5)
C
      P_KIN=-20.
      kf1=0
      kf2=0
      IF(K1.GE.1.AND.K1.LE.NP) THEN
         kf1=1
         qq=0.
         DO j=1,3
            p1(j)=POUT(j,K1)
            qq=qq+p1(j)**2
         ENDDO
         p1(4)=SQRT(qq+AM(K1)**2)
         p1(5)=SQRT(qq)
      ENDIF
      IF(K2.GE.1.AND.K2.LE.NP) THEN
         kf2=1
         qq=0.
         DO j=1,3
            p2(j)=POUT(j,K2)
            qq=qq+p2(j)**2
         ENDDO
         p2(4)=SQRT(qq+AM(K2)**2)
         p2(5)=SQRT(qq)
      ENDIF
      DO j=1,3
         ptar(j)=PIN(j,2)
      ENDDO
      ptar(5)=SQRT(ptar(1)**2+ptar(2)**2+ptar(3)**2)
      ptar(4)=SQRT(ptar(5)**2+AMIN(2)**2)
      
      IF(kf1.EQ.0) GO TO 999
      var=-20.
      
      IF(IFL.EQ.1) THEN
C
         var=AM(K1)**2+AMIN(2)**2-2.*p1(4)*ptar(4)
         DO j=1,3
            var=var+2.*p1(j)*ptar(j)
         ENDDO
         var=-var
C
      ELSE IF(IFL.EQ.2) THEN
C
         IF(kf2.EQ.0) THEN
            pp1=0.
            pp2=0.
            DO j=1,3
               p2(j)=POUT(j,1)+POUT(j,2)
               pp1=pp1+POUT(j,1)**2
               pp2=pp2+POUT(j,2)**2
            ENDDO
            p2(5)=SQRT(p2(1)**2+p2(2)**2+p2(3)**2)
            p2(4)=SQRT(pp1+AM(1)**2)+SQRT(pp2+AM(2)**2)
         ENDIF
C
         IF(p2(5).GT.0.) THEN
            DO j=1,3
               dir(j)=p2(j)/p2(5)
            ENDDO
         ELSE
            dir(1)=0.
            dir(2)=0.
            dir(3)=1.
         ENDIF

         DO j=1,3
            bet(j)=p2(j)/p2(4)
         ENDDO
         bet(4)=1./SQRT(1.-bet(1)**2-bet(2)**2-bet(3)**2)
C
         CALL GLOREN(bet,p1(1),px(1))
         qq=0.
         var=0.
         DO j=1,3
            qq=qq+px(j)**2
            var=var+px(j)*dir(j)
         ENDDO
         var=var/SQRT(qq)  ! COS(th)
C         write(6,FMT='(5F10.4)') p2,var 
      ENDIF
C
      P_KIN=var
C
 999  RETURN
C
      END
C
      INCLUDE 'efmass.f'
      INCLUDE 'gloren.f'
