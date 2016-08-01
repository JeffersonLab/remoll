* $Header: /afs/cern.ch/exp/compass/src/cvs/comgeant/code/src/omgbatch/ompro/gdecan.F,v 1.1.1.1 1997/06/02 17:39:52 fdr Exp $
* $Log: gdecan.F,v $
* Revision 1.1.1.1  1997/06/02 17:39:52  fdr
* Comgeant Monte-Carlo
*
* Revision 3.2.0.1  1996/11/07 19:23:23  las
* First CVS version.
*
*CMZ :          06/03/96  16.55.41  by  E.Chudakov
*-- Author : Adapted from FOWL by D.Barberis, Uni.HD, 4/10/89.
*--          Updated by E.Chudakov (random numbers, SAVE...)  02/01/95
*--          Updated by E.Chudakov (equal weights)            06/03/96
C
      SUBROUTINE GDECAN(NPFOWL,TEFOWL,AMFOWL,WTFOWL,PCFOWL)
*---
*     Phase-space decay into N particles.
*
*     Input:
*              NPFOWL               number of decay particles
*              TEFOWL               mass of decaying particle
*              AMFOWL(1:NPFOWL)     masses of decay products
*              WTFOWL<=0.           diff. weights of events
*              WTFOWL>0.            equal (=1.) weights of events
*     Output:
*              WTFOWL               weight of the event (or not changed)
*              PCFOWL(1:4,1:NPFOWL) four-momentum of decay products
*
*     Adapted from FOWL by D.Barberis, Uni.HD, 4/10/89.
*---
C#if defined OMGEANT_VERSION
C      CHARACTER*80 VersionString
C      DATA VersionString /
C     & '$Id: gdecan.F,v 1.1.1.1 1997/06/02 17:39:52 fdr Exp $'/
C#endif
      DIMENSION AMFOWL(NPFOWL), PCFOWL(4,NPFOWL)
      COMMON /SHUFFL/ RNO(50), NTNM4, NTM2, NTM1
      PARAMETER ( MXFOWL = 20 )
      DIMENSION PD(MXFOWL), EMM(MXFOWL), EMS(MXFOWL), SM(MXFOWL)
      DIMENSION AMSAVE(MXFOWL)
      REAL      wgtinim,rrr
      INTEGER   ntry
C
      SAVE ETC,NPSAVE,AMSAVE,SM,TECMTM,WTMAXQ,EMS,EMM
C
      DATA TWOPI / 6.2831853073 /
      DATA ETC / -1. /
*---     Initialization.
      wgtinim=WTFOWL
      ntry=0
      WTFOWL = 0.
      CALL VZERO(PCFOWL,4*NPFOWL)
      IF (NPFOWL.LT.2.OR.NPFOWL.GT.MXFOWL) GO TO 900
*---     Check if same as last time, if so skip first part.
      IF (TEFOWL.EQ.ETC) THEN
         IF (NPFOWL.EQ.NPSAVE) THEN
            DO 150 I=1,NPFOWL
               IF (AMFOWL(I).NE.AMSAVE(I)) GO TO 151
  150       CONTINUE
            GO TO 300
         ENDIF
      ENDIF
  151 ETC = TEFOWL
      NPSAVE = NPFOWL
      CALL UCOPY(AMFOWL,AMSAVE,NPFOWL)
C      PRINT 9001, NPFOWL,TEFOWL,(AMFOWL(I),I=1,NPFOWL)
C 9001 FORMAT(2X,I3,'-BODY PHASESPACE BY FOWL',4X,F8.5,' going to '
C     +      ,(6F8.5))
      NTM1 = NPFOWL - 1
      NTM2 = NTM1 - 1
      NTP1 = NPFOWL + 1
      NTNM4 = 3 * NPFOWL - 4
      EMM(1) = AMFOWL(1)
      TM = 0.0
      DO 200 I=1,NPFOWL
         EMS(I) = AMFOWL(I)**2
         TM = TM + AMFOWL(I)
         SM(I) = TM
  200 CONTINUE
*---     Constants depending on TEFOWL.
      TECMTM = TEFOWL - TM
      EMM(NPFOWL) = TEFOWL
*---     Constant cross-section as function of TEFOWL.
      EMMAX = TECMTM + AMFOWL(1)
      EMMIN = 0.0
      WTMAX = 1.0
      DO 350 I=2,NPFOWL
         EMMIN = EMMIN + AMFOWL(I-1)
         EMMAX = EMMAX + AMFOWL(I)
         WTMAX = WTMAX * OPDK(EMMAX,EMMIN,AMFOWL(I))
  350 CONTINUE
      WTMAXQ = 1.0 / WTMAX
*---     Calculation of WT based on effective masses EMM.
  300 CALL ORANGNR
*---      ORANGNR fills RNO with 3*NPFOWL-4 random numbers,
*---           of which the first NPFOWL-2 are ordered.
      IF (NTM2.GT.0) THEN
         DO 508 J=2,NTM1
            EMM(J) = RNO(J-1) * (TECMTM) + SM(J)
  508    CONTINUE
      ENDIF
      WTFOWL = WTMAXQ
      IR = NTM2
      DO 530 I=1,NTM1
         PD(I) = OPDK(EMM(I+1),EMM(I),AMFOWL(I+1))
         WTFOWL = WTFOWL * PD(I)
  530 CONTINUE
C
C---      Try again in order to get rid of the weight?
C
      IF(wgtinim.GT.1.E-10) THEN
        ntry=ntry+1
        IF(ntry.LT.10000) THEN
          CALL GRNDM(rrr,1)
          IF(rrr.GT.WTFOWL/wgtinim) GO TO 300
        ENDIF
      ENDIF
C
*---      Complete specification of event (Raubold-Lynch method).
      PCFOWL(1,1) = 0.0
      PCFOWL(2,1) = PD(1)
      PCFOWL(3,1) = 0.0
      DO 570 I=2,NPFOWL
         PCFOWL(1,I) = 0.0
         PCFOWL(2,I) = -PD(I-1)
         PCFOWL(3,I) = 0.0
         IR = IR + 1
         BANG = TWOPI * RNO(IR)
         CB = COS(BANG)
         SB = SIN(BANG)
         IR = IR + 1
         C = 2.0 * RNO(IR) - 1.0
         S = SQRT(1.0-C*C)
         IF (I.LT.NPFOWL) THEN
            ESYS = SQRT(PD(I)**2+EMM(I)**2)
            BETA = PD(I) / ESYS
            GAMA = ESYS / EMM(I)
            DO 568 J=1,I
               AA = PCFOWL(1,J)**2 + PCFOWL(2,J)**2 + PCFOWL(3,J)**2
               PCFOWL(4,J) = SQRT(AA+EMS(J))
               CALL OROTES2(C,S,CB,SB,PCFOWL(1,J))
               PSAVE = GAMA * ( PCFOWL(2,J) + BETA * PCFOWL(4,J) )
               PCFOWL(2,J) = PSAVE
  568       CONTINUE
         ELSE
 1567       DO 1568 J=1,I
               AA = PCFOWL(1,J)**2 + PCFOWL(2,J)**2 + PCFOWL(3,J)**2
               PCFOWL(4,J) = SQRT(AA+EMS(J))
               CALL OROTES2(C,S,CB,SB,PCFOWL(1,J))
 1568       CONTINUE
         ENDIF
  570 CONTINUE
  900 RETURN
      END
      FUNCTION OPDK(A,B,C)
*
*--   CMS momentum for a two-body decay ( A --> B + C )
*
      A2 = A*A
      B2 = B*B
      C2 = C*C
      PD = A2 + (B2-C2)**2/A2 - 2.0*(B2+C2)
      IF (PD.LT.0.) THEN
         PRINT 900, A, B, C, PD
         PD=0.
      ENDIF
      OPDK = 0.5 * SQRT(PD)
      RETURN
  900 FORMAT('0PDK :  A,B,C,PD =',4E15.7)
      END
      SUBROUTINE ORANGNR
*
*---      Assembles random numbers for one event.
*
      COMMON /SHUFFL/ RNO(50), NTNM4, NTM2, NTM1
C      DO i= 1,NTM2
C        RNO(I) = RNDM(DUMMY)
C      END DO
C
      CALL GRNDM(RNO(1),NTM2)
C
*---      Order the first NTM2 random numbers
*---        two is a special case (faster)
      IF (NTM2-2) 200,160,110
  110 KM1 = NTM2 - 1
      DO 150 I= 1, KM1
      IQUIT = 0
      NI = NTM2 - I
      DO 140 J= 1, NI
      IF (RNO(J) - RNO(J+1)) 140,140,120
  120 SAV = RNO(J)
      RNO(J) = RNO(J+1)
      RNO(J+1) = SAV
      IQUIT = 1
  140 CONTINUE
      IF (IQUIT) 200,200,150
  150 CONTINUE
      GO TO 200
  160 IF (RNO(1).LE.RNO(2)) GO TO 200
      SAV = RNO(1)
      RNO(1) = RNO(2)
      RNO(2) = SAV
  200 CONTINUE
*---      Choose the rest of the random numbers.
C      DO 300 I= NTM1, NTNM4
C  300 RNO(I) = RNDM(DUMMY)
C
C      DO i= NTM1,NTNM4
C        RNO(I) = RNDM(DUMMY)
C      END DO
      CALL GRNDM(RNO(NTM1),NTNM4-NTM1+1)
C
      RETURN
      END
      SUBROUTINE OROTES2(C,S,C2,S2,PCF)
*
*---      This subroutine does two rotations (xy and xz).
*
      DIMENSION PCF(4)
      SA     = PCF(1)
      SB     = PCF(2)
      A      = SA*C - SB*S
      PCF(2) = SA*S + SB*C
      B      = PCF(3)
      PCF(1) = A*C2 - B*S2
      PCF(3) = A*S2 + B*C2
      RETURN
      END
