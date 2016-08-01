      REAL FUNCTION SAIDXSECA(E,COSTH,IPROC,ICUT)
C
C ---  SAID  gamma+p --> pi N cross section
C ---  E - photons energy (E<2000 MeV)
C ---  COSTH - cos of pion angle in CM
C ---  IPROC = 1 - pi0 p
C              2 - pi+ n
C              3 ... is at the moment unclear to me
C      Returns cross section in microbarn/ster for pion in CM
C
C---      SAID parametrization (the version I have) seems not to work above 2 GeV
C---      ELSA measurement gives 3.7 mub at 2 GeV and 1.25 nub at 3 GeV: 
C              For ICUT>0 (emx=2 GeV) I use exp(-1.*(E-emx))*SAID(emx)
C
C     From I.Strakovsky, D.Arndt
C Follows Knochlein, Dreschel, Tiator, Z.Phys.A352(1995) 327-343
C
C
C
C
      IMPLICIT NONE
      REAL E,COSTH
      INTEGER IPROC,ICUT
C
      REAL     PRFAMP,OBSPRD
      EXTERNAL PRFAMP,OBSPRD
C
      REAL ee,fr(4),fi(4),dx3,res,fac,emx
      INTEGER it
C
      emx=2.
      fac=1.
      ee=E
      IF(ICUT.GT.0.AND.ee.GT.emx) THEN
         fac=EXP(-1.*(ee-emx))
         ee=emx
      ENDIF
C      
      ee=ee*1000.
      it=1
      res=0.
      IF(ee.GT.155.) THEN
         CALL SAIDGET(ee,COSTH,IPROC)
         res=OBSPRD(it)
C         res=10.
      ELSE
         res=0.
      ENDIF
      SAIDXSECA=res*fac
C
      RETURN
      END
C
      SUBROUTINE SAIDGET(E,COSTH,IPROC)
      IMPLICIT NONE
      REAL E,COSTH,PRFAMP
      INTEGER IPROC
      REAL fr(4),fi(4),dx3,s
C
      s=PRFAMP(E,COSTH,IPROC,fr(1),fi(1),dx3)
      RETURN
      END
