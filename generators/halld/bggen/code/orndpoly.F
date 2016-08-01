      REAL FUNCTION ORNDPOLY(NP,RP,XLIM)
C
C---      Generate a random number to a polynomial distribution (NP<3):
C         RP(0)+RP(1)*X**1+...+RP(NP)*X**NP
C         in an interval XLIM(1):XLIM(2)
      IMPLICIT NONE
      INTEGER NP
      REAL RP(0:NP),XLIM(2)
C
      REAL RNDM
C
      INTEGER i,j,np1,np2,nsol,nrusf
      REAL p(0:20) ! - integral polynomial 
     +    ,anorm,xx,qq,rnd,xres(20)
C
      DOUBLE PRECISION dp(0:20),dx(10),dd
C
      ORNDPOLY=-9999.
      xx=0.
      IF(NP.LT.0) GO TO 999
      IF(XLIM(1).GE.XLIM(2)) GO TO 999
C
C---   Integrate the polynomial
C
      np1=NP+1
      p(0)=0.
      DO i=1,np1
         p(i)=RP(i-1)/REAL(i)
         p(0)=p(0)-p(i)*XLIM(1)**i    ! normalization: =0 at XLIM(1)
      ENDDO
C
      anorm=0.
      DO i=0,np1
         anorm=anorm+p(i)*XLIM(2)**i  ! normalization: =1 at XLIM(2)
      ENDDO
      np2=1  ! the real power of the polynomial 
      DO i=0,np1
         p(i)=p(i)/anorm
         IF(ABS(p(i)).GT.1.E-15) np2=i
      ENDDO
C
      IF(np2.LT.1) GO TO 999
C
      rnd=RNDM(dx)
      p(0)=p(0)-rnd
C
      nsol=0
      IF(np2.EQ.1) THEN          ! flat distr
         xx=XLIM(1)+rnd*(XLIM(2)-XLIM(1))  
         nsol=1
C
      ELSE IF(np2.EQ.2) THEN     ! linear
C
         qq=p(1)**2-4.*p(0)*p(2)
         IF(qq.LT.0.) THEN
            WRITE(6,*) ' *** ORNDPOLY err 1, NP=',NP
            GO TO 999
         ENDIF
         xres(1)=(-p(1)-SQRT(qq))/(2.*p(2))
         xres(2)=(-p(1)+SQRT(qq))/(2.*p(2))
         DO i=1,2
            IF(xres(i).GE.XLIM(1).AND.xres(i).LE.XLIM(2)) THEN
               xx=xres(i)
               nsol=nsol+1
            ENDIF
         ENDDO
C
      ELSE IF(np2.EQ.3) THEN     ! 2-nd, 3-rd for the integral
C
         DO i=0,np2-1
            dp(i)=p(i)/p(np2)
         ENDDO
C
         CALL DRTEQ3(dp(2),dp(1),dp(0),dx,dd)
C
         nrusf=1                 ! number of real non degenerated solutions
         IF(dd.EQ.0.D0) nrusf=2
         IF(dd.LT.0.D0) nrusf=3
         DO i=1,nrusf
            xres(i)=dx(i)
            IF(xres(i).GE.XLIM(1).AND.xres(i).LE.XLIM(2)) THEN
               xx=xres(i)
               nsol=nsol+1
            ENDIF
         ENDDO
C 
      ENDIF
      IF(nsol.GT.1) THEN
         WRITE(6,*) ' *** ORNDPOLY several solutions NP,nsol=',NP,nsol
         GO TO 999
      ELSE IF(nsol.EQ.1) THEN
         ORNDPOLY=xx
      ENDIF
C
C
 999  RETURN
      END
