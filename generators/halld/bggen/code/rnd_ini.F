      SUBROUTINE RND_INI(ISEQ)
C---       RANLUX initialization (random number)
      IMPLICIT NONE
      INTEGER ISEQ
      INTEGER k1,k2,lux
C
      k1=0
      k2=0
      lux=3
      CALL RLUXGO(lux,ISEQ,k1,k2)
C
      RETURN
      END

