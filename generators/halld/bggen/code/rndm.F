C
      REAL FUNCTION RNDM(X)
      IMPLICIT NONE
      REAL X
      REAL a
C
      CALL RANLUX(a,1)
      RNDM=a
      RETURN
      END

