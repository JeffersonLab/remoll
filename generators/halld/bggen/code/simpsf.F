      REAL FUNCTION SIMPSF(FUN,X1,X2,N2)
      IMPLICIT NONE
C
C ===  Integrate FUN between X1-X2 using Simpson method
C ===   N2 - number of intervals, even
C
      REAL FUN,X1,X2
C      EXTERNAL FUN
      INTEGER N2
C
      REAL step
      DOUBLE PRECISION s1,s2,s
      INTEGER i,n
C
      SIMPSF=0.
      IF(N2.LT.3) THEN
         WRITE(6,*) ' *** SIMPSF error - N2=',N2
         GO TO 999
      ENDIF
      IF(X1.GE.X2) THEN
         WRITE(6,*) ' *** SIMPSF error - X1,X2=',X1,X2
         GO TO 999
      ENDIF
C
      n=N2
      IF(MOD(N2,2).NE.0) n=n+1
      step=(X2-X1)/n
C
      s1=0.
      s2=0.
C
      DO i=1,n-1,2
         s1=s1+DBLE(FUN(X1+step*i))
      ENDDO
C
      DO i=2,n-2,2
         s2=s2+DBLE(FUN(X1+step*i))
      ENDDO
C
      s=(DBLE(FUN(X1))+DBLE(FUN(X2))+s1*4.D0+s2*2.D0)*DBLE(step)/3.D0
      SIMPSF=s
C
 999  RETURN
      END

      
