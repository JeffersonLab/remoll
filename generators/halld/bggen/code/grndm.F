C
C---      GEANT random function, redefined (RNDM - is in fact RANLUX)
C
      SUBROUTINE GRNDM(X,N)
      IMPLICIT NONE
      INTEGER N  !,i
      REAL X(N)
C
      CALL RANLUX(X(1),N)
C      DO i=1,N
C         X(i)=RNDM(i)
C      ENDDO
C
      END
