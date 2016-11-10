      REAL FUNCTION SAIDE(ENA,IREACA,ICUTA)
C
C---      Cross section (SAID) in mbarn
C
      IMPLICIT NONE
      REAL SAIDXSECA,ENA,STMP,SIMPSF
      EXTERNAL STMP
      INTEGER IREACA,ICUTA
C
      COMMON/CSAID/ EN,COST,IREAC,ICUT
      REAL EN,COST
      INTEGER IREAC,ICUT
C
      EN=ENA
      IREAC=IREACA
      ICUT=ICUTA
      SAIDE=SIMPSF(STMP,-1.,1.,100)*2*3.1416
C      write(6,*) 'e,ireac,icut,saide=',en,ireac,icut,saide
C
      SAIDE=SAIDE/1000.
C
      END
C
      REAL FUNCTION STMP(X)
      REAL X
C
      COMMON/CSAID/ EN,COST,IREAC,ICUT
      REAL EN,COST
      INTEGER IREAC,ICUT
      REAL sum
      INTEGER i
C
      COST=X
      sum=0.
      DO i=1,2
         IF(IREAC.EQ.0.OR.i.EQ.IREAC) THEN
            sum=sum+SAIDXSECA(EN,COST,i,ICUT)
         ENDIF
      ENDDO
      STMP=sum
C      write(6,*) STMP
C
      END
