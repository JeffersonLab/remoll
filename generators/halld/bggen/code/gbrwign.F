      REAL FUNCTION GBRWIGN(DUMMY)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *  Breit-Wigner distribution                                     *
C.    *    ==>Called by : GDECAY                                       *
C.    *                                                                *
C.    ******************************************************************
C.
      IMPLICIT NONE
      REAL     DUMMY
      REAL RNDM
      INTEGER itry
      REAL gg,gm,xrn,pi,de
C.
C.    ------------------------------------------------------------------
C
C--  Create Lorentz distributed energy with FWHM HBAR/TLIFE.
C--  (via integral-transformation of Lorentz-distribution)
C--                 (M.Guckes)
C           f(E)=gamma/2pi/(E**2+gamma**2/4) - non relativistic
C---       Modified by E.Ch. May 2007
         itry=0
 10      itry=itry+1
C         CALL GRNDM(rndm,1)
C         gamma=3.291086E-25/TLIFE*2.
         de=1./2.*TAN(3.1416*(RNDM(de)-0.5))
C         write(6,*) itry,gamma,de
         IF(ABS(de).GT.2.0) THEN
            IF(itry.LT.1000) GO TO 10
            WRITE(6,*) ' *** GBRWIGN: too many tries for tau='
     +           ,itry
            de=0.
         ENDIF
         GBRWIGN=de
C
      RETURN
      END
