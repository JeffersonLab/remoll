C
C ---  SAID  gamma+p --> pi N cross section
C
C     From I.Strakovsky, D.Arndt
C Follows Knochlein, Dreschel, Tiator, Z.Phys.A352(1995) 327-343
C
C  Usage:
C      ee=E*1000.
C      it=1
C      s=PRFAMP(ee,COSTH,IPROC,fr,fi,dx3)
C      res=OBSPRD(it)
C
C ---  E - photons energy (E<2 GeV)
C ---  COSTH - cos of pion angle in CM
C ---  IPROC = 1 - pi0 p
C              2 - pi+ n
C              3 ... is at the moment unclear to me
C      Returns cross section in microbarn/ster for pion in CM
C
C ***************************************************
      REAL FUNCTION PRFAMP(EGX,Z,IR,FRV,FIV,S3)
C      IMPLICIT NONE
C SUBROUTINE TO GET "F" AMPLITUDES FOR PION-PHOTOPRODUCTION 11/93 ARNDT
      REAL EGX,Z,FRV(4),FIV(4),S3
      INTEGER IR
      COMMON/PRFA/EMR(6,2,6),EMI(6,2,6),NTL1(18),NTL2(18),TTLPN(18)
      INTEGER NTL1,NTL2
      REAL EMR,EMIT,TLPN
      REAL F2(4),CIS(4,2),PP(8),PDP(8),EMPI(6,2,6)
      COMMON/AMPLS/HRX(4),HIX(4),QCM,ZKCM,CS,EG
C to add a calculation of observables 9/18/02 RAA
      REAL ZM,EGM,C1,C3,SQ
      INTEGER IRM,i,k,ii
      DATA IRM,ZM,EGM,C1,C3,SQ/27,27.0,0.0,0,0,0/
C      SAVE IRM,ZM,EGM,C1,C3,SQ
C      SAVE F2,CIS,PP,PDP,EMPI,II
      SAVE
C      write(6,*) 'EGX,Z,IR,II',EGX,Z,IR,II  ! MYPRI
      CS=Z
      EG=ABS(EGX)
      IF(IRM.NE.27) GO TO 1
      SQ=SQRT(2.0)
      CIS(2,1)=SQ
      CIS(2,2)=-SQ/3.0
      CIS(1,1)=1.0
      CIS(1,2)=2.0/3.0
      CIS(3,1)=SQ
      CIS(3,2)=SQ/3.0
      CIS(4,1)=-1.0
      CIS(4,2)=2.0/3.0
    1 IF(IR.NE.IRM) EGM=0.0
      IF(EGX.EQ.EGM) GO TO 2
C      write(6,*) 'EGX,EGM,IR,II',EGX,EGM,IR,II  ! MYPRI
      IRM=IR
      C3=1.0
      C1=0.0
      IF(IR.GT.6) GO TO 12
      C1=1.0
      C3=0.0
      IF(IR.GT.4) GO TO 12
      I=IR
      IF(I.LT.1) I=1
      C3=CIS(I,2)
      C1=CIS(I,1)
   12 EGM=EGX
      I=IR
      IF(IR.LT.1) I=1
      IF(I.GT.4) I=4
      II=I
      CALL PROPEC(EG,EMPI)
      CALL PRSM02(EG,I,EMR,EMI,NTL1,TTLPN)
    2 IF(Z.EQ.ZM) GO TO 3
      ZM=Z
      CALL PJDRV(Z,8,PP,PDP)
    3 CONTINUE
C      write(6,*) FRV
      DO k=1,4
C         write(6,*) 'k=',K
         FRV(k)=0.
         FIV(k)=0.
      ENDDO
C      write(6,*) 'EG,II,Z',EG,II,Z  ! MYPRI
      CALL FOPEC(EG,II,Z,FRV)
      ME=3
      IF(IR.GT.2.AND.IR.LT.6) ME=5
      MM=ME+1
      LL=0
    5 LL=LL+1
      IF(LL.GT.6) GO TO 98
      DO 9 M=1,6
      DO 9 J=1,2
      Z1=EMR(M,J,LL)
      Z1P=EMPI(M,J,LL)
      EMR(M,J,LL)=Z1-Z1P
      EMPI(M,J,LL)=0.0
    9 CONTINUE
      ZL=LL-1
      EP=C3*EMR(1,2,LL)+C1*EMR(ME,2,LL)
      EM=C3*EMR(1,1,LL)+C1*EMR(ME,1,LL)
      BP=C3*EMR(2,2,LL)+C1*EMR(MM,2,LL)
      BM=C3*EMR(2,1,LL)+C1*EMR(MM,1,LL)
      EPI=C3*EMI(1,2,LL)+C1*EMI(ME,2,LL)
      EMX=C3*EMI(1,1,LL)+C1*EMI(ME,1,LL)
      BPI=C3*EMI(2,2,LL)+C1*EMI(MM,2,LL)
      BMI=C3*EMI(2,1,LL)+C1*EMI(MM,1,LL)
      FRV(1)=FRV(1)+PP(LL+1)*(ZL*BP+EP)
      FIV(1)=FIV(1)+PP(LL+1)*(ZL*BPI+EPI)
      IF(LL.EQ.1) GO TO 5
      IF(LL.LT.3) GO TO 6
      FRV(1)=FRV(1)+PP(LL-1)*((ZL+1.0)*BM+EM)
      FIV(1)=FIV(1)+PP(LL-1)*((ZL+1.0)*BMI+EMX)
    6 FRV(2)=FRV(2)+PP(LL)*((ZL+1.0)*BP+ZL*BM)
      FIV(2)=FIV(2)+PP(LL)*((ZL+1.0)*BPI+ZL*BMI)
      FRV(3)=FRV(3)+PDP(LL+1)*(EP-BP)
      FIV(3)=FIV(3)+PDP(LL+1)*(EPI-BPI)
      IF(LL.LT.3) GO TO 7
      FRV(3)=FRV(3)+PDP(LL-1)*(EM+BM)
      FIV(3)=FIV(3)+PDP(LL-1)*(EMX+BMI)
    7 FRV(4)=FRV(4)+PDP(LL)*(BP-EP-BM-EM)
      FIV(4)=FIV(4)+PDP(LL)*(BPI-EPI-BMI-EMX)
      GO TO 5
   98 S=0.0
      DO 11 K=1,4
      F2(K)=FRV(K)**2+FIV(K)**2
   11 S=S+F2(K)
      CALL PRKIN(EG,IR,EPI,ZKCM,QCM)
      S2=(1.0-Z**2)
      S=F2(1)+F2(2)+S2*(F2(3)+F2(4))/2.0
      S=S-2.0*Z*(FRV(1)*FRV(2)+FIV(1)*FIV(2))
      S=S+S2*(FRV(1)*FRV(4)+FIV(1)*FIV(4)+FRV(2)*FRV(3)+FIV(2)*FIV(3))
      S=S+S2*Z*(FRV(3)*FRV(4)+FIV(3)*FIV(4))
      PRFAMP=S*QCM/ZKCM/100.0
      S3=F2(3)+F2(4)+2.0*Z*(FRV(3)*FRV(4)+FIV(3)*FIV(4))
      S3=S3*QCM/ZKCM/200.0*S2
C convert F to H 9/18/02
      sh=sqrt((1.0-z)/2.0)
      ch=sqrt(1.0-sh**2)
      hrx(3)=sq*ch*sh**2*(FRV(3)-FRV(4))
      hrx(1)=-sq*sh*ch**2*(FRV(3)+FRV(4))
      hrx(2)=hrx(3)+sq*ch*(FRV(2)-FRV(1))
      hrx(4)=sq*sh*(FRV(2)+FRV(1))-hrx(1)
      hix(3)=sq*ch*sh**2*(FIV(3)-FIV(4))
      hix(1)=-sq*sh*ch**2*(FIV(3)+FIV(4))
      hix(2)=hix(3)+sq*ch*(FIV(2)-FIV(1))
      hix(4)=sq*sh*(FIV(2)+FIV(1))-hix(1)
   99 RETURN
      END
C ****************************************************************
      SUBROUTINE PJDRV(Z,JMX,PP,PDP)
      DIMENSION PP(20),PDP(20)
C GET LEGENDRE DERIVATIVE PP(1ST) AND PDP(2ND)
      SAVE 
      JM=JMX
      IF(JM.GT.20) JM=20
      J=0
      PJ=1.0
      PJM=0.0
    1 J=J+1
      ZJ=J-1
      PP(J)=0.0
      PDP(J)=0.0
      IF(J.LT.2) GO TO 2
      PP(J)=ZJ*PJM+Z*PP(J-1)
      IF(J.LT.3) GO TO 2
      PDP(J)=Z*PDP(J-1)+(ZJ+1.0)*PP(J-1)
    2 X=PJ
      PJ=((2.0*ZJ+1.0)*Z*PJ-ZJ*PJM)/(ZJ+1.0)
      PJM=X
      IF(J.LT.JM) GO TO 1
      RETURN
      END
C ************************************************
      SUBROUTINE FOPEC(EL,IR,Z,F)
      DIMENSION F(4),EPX(4),E2X(4),GC(4),AA(4),BB(4),CC(4)
      DATA SQ2,WP,WN,UP,UN,GN/1.41421,135.04,938.256,1.793,-1.913,62.51/
      DATA EPX,E2X/0.0,1.0,-1.0,0.0,1.0,0.0,1.0,0.0/
      DATA GC/-1.0,-1.0,-1.0,1.0/
C      SAVE EPX,E2X,GC,AA,BB,CC
      SAVE
      S=WN*(WN+2.0*EL)
      W=SQRT(S)
      ZK=EL/SQRT(1.0+2.0*EL/WN)
      Q=SQRT((S-(WN+WP)**2)*(S-(WN-WP)**2)/4.0/S)
      IF(Q.LT.0.0)  GO TO 99
      Z2=SQRT(Q**2+WN**2)
      ZU=-Z2/Q
      ZT=SQRT(Q**2+WP**2)/Q
      Z2=SQRT(Z2+WN)
      Z1=SQRT(SQRT(ZK**2+WN**2)+WN)
      DT=-2.0*(ZT-Z)
      DU=2.0*(ZU-Z)
      GG=1000.0*GN/W
      USCL=(W+WN)/2.0/Z1/Z2
      AA(1)=0.0
      AA(2)=0.0
      AA(3)=GG*Z2/Z1
      AA(4)=-GG*Z1*Q/Z2/ZK
      BB(1)=-GG*W/Q/USCL/2.0
      BB(2)=GG*W/Z2**2/2.0/USCL
      BB(3)=-AA(3)
      BB(4)=-AA(4)
      GG=GG*USCL
      CC(1)=-GG*Z2**2/Q
      CC(2)=GG
      CC(3)=-GG*Z2**2/WN
      CC(4)=-GG*Q/WN
C      write(6,*) 'EL,IR,Z,F',EL,IR,Z,F  ! MYPRI
      E2=E2X(IR)
      G=GC(IR)
      IF(IR.EQ.2.OR.IR.EQ.3)  G=G*SQ2
      EPI=EPX(IR)
      U2=(UN+E2*(UP-UN))
      F(1)=G*(AA(1)*EPI/DT+(E2*BB(1)+U2*CC(1))/DU)
      F(2)=G*(AA(2)*EPI/DT+(E2*BB(2)+U2*CC(2))/DU)
      F(3)=G*(AA(3)*EPI/DT+(E2*BB(3)+U2*CC(3))/DU)
      F(4)=G*(AA(4)*EPI/DT+(E2*BB(4)+U2*CC(4))/DU)
   99 RETURN
      END
C **************************************************
      SUBROUTINE PRSM02(TLB,IR,EMR,EMI,NTL,TTLPN)
C get photo-production multipoles (from VPI analysis)
C Tlab=Photon LAB energy (MeV); IR=1(Pi0), 2(Pi+), 3(Pi-), 4(Pi0N)
C NTL(20) is a TITLE which is set on the 1st call to the subroutine
C EMR(6,2,6) is the REAL part (in mFm) and EMI is the IMAGINARY part
C of the multipole amplitudes. The INDEX (M,J,L) labels the state as fol
C M=1(pE3/2), 2(pM3/2), 3(pE1/2), 4(pM1/2), 5(nE1/2), 6(nM1/2)
C L=ORBITAL angular momentum, J=1(j=l-1/2) or 2(j=l+1/2). (actually L=l+
C some examples: S11pE=(3,2,1)  S31pE=(1,2,1)  P33pM=(4,2,2) P33pE=(3,2,
C P11pM=(4,1,2) D15nM=(6,2,3) .....
      COMMON/GOMEGA/GOM1,GOM2,GOMS,GPI2,GP1,GP2
      COMMON/PRKC/IPRK
      DIMENSION PEM(15,6,2,6),EMR(6,2,6),EMI(6,2,6),NF(6,2,6),CCS(5)
     C,EMPI(6,2,6),QL(10),NTL(13),NTC(13),PP(400),TPNR(4,8),TPNI(4,8)
      DIMENSION TTLPN(15),PP1(70),PP2(70),PP3(70),PP4(70),PP5(66)
      CHARACTER HTL*52
      EQUIVALENCE (PP,PP1),(PP(71),PP2),(PP(141),PP3)
     C,(PP(211),PP4),(PP(281),PP5)
      DATA CCS/  22.500,   0.000,  13.750,   0.000,   0.000/
      DATA IPRKX/  1/
      DATA HTL/'SM02K 2000 MEV P(148) CHI/DP=35297/17571            '/
      DATA IMX/346/
      DATA PP1/ 0.25121E+13, 0.14625E+02,-0.12639E+03, 0.75187E+02,
     C 0.00000E+00,-0.18097E+02, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C-0.59426E+01, 0.00000E+00,-0.63967E+02, 0.10470E+03, 0.21321E+13,
     C-0.86934E+01, 0.10773E+01,-0.11660E+00, 0.00000E+00, 0.64122E+02,
     C-0.86162E+01, 0.17660E+00, 0.00000E+00,-0.16826E+02, 0.15135E+01,
     C 0.10282E+02, 0.25521E+13,-0.96450E+00, 0.23707E+02, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00,-0.44563E+02, 0.65940E+02, 0.21212E+13,
     C 0.61205E+01,-0.10655E+01,-0.56700E-01, 0.00000E+00, 0.15401E+03,
     C-0.92590E+01, 0.21412E+13, 0.16290E+01,-0.92310E+00, 0.60700E-01,
     C 0.00000E+00, 0.24447E+02,-0.21964E+01, 0.25612E+13, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00,-0.13333E+02, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00, 0.93731E+01, 0.24122E+13,-0.10673E+01,
     C 0.49582E+01, 0.00000E+00, 0.00000E+00,-0.15440E+01, 0.24222E+13,
     C-0.13886E+02/
      DATA PP2/ 0.11655E+03,-0.13603E+04, 0.17137E+04, 0.34104E+02,
     C 0.53811E+02,-0.16301E+02, 0.49180E+00, 0.00000E+00, 0.00000E+00,
     C-0.98292E+02, 0.23824E+02, 0.25322E+13, 0.22764E+02,-0.72372E+02,
     C 0.53892E+02, 0.00000E+00, 0.59900E+02,-0.69650E+02, 0.25422E+13,
     C-0.16052E+02, 0.57929E+02,-0.62187E+02, 0.00000E+00, 0.00000E+00,
     C-0.28311E+02, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C 0.59717E+01, 0.25522E+13,-0.40780E+01, 0.94920E+01, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C-0.46976E+01, 0.25622E+13, 0.17593E+01, 0.25113E+13, 0.21683E+01,
     C-0.31918E+02, 0.00000E+00, 0.00000E+00,-0.16604E+03, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00, 0.21527E+02, 0.00000E+00,-0.75828E+02,
     C 0.16466E+03, 0.25213E+13,-0.10177E+02, 0.51979E+02,-0.86352E+02,
     C 0.00000E+00, 0.33052E+02, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C 0.36886E+01, 0.00000E+00,-0.27140E+01, 0.25313E+13, 0.34127E+02,
     C-0.89989E+02/
      DATA PP3/ 0.00000E+00, 0.00000E+00, 0.10452E+03,-0.10911E+03,
     C 0.00000E+00, 0.00000E+00,-0.53911E+02, 0.14084E+03,-0.28059E+02,
     C 0.34639E+02, 0.25413E+13,-0.40309E+02, 0.88893E+02,-0.54692E+02,
     C 0.00000E+00, 0.18067E+03,-0.55538E+03, 0.42138E+03, 0.25513E+13,
     C 0.16695E+02, 0.00000E+00, 0.00000E+00, 0.00000E+00,-0.10387E+03,
     C 0.11062E+03, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C 0.77412E+01, 0.25613E+13,-0.65508E+01, 0.20229E+02, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C 0.25953E+01, 0.25123E+13,-0.25835E+02, 0.85524E+02,-0.70269E+02,
     C 0.00000E+00,-0.34619E+02, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00, 0.65460E+01, 0.25223E+13,-0.22901E+01,
     C 0.25323E+13, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C 0.13747E+01, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.68360E+00,
     C 0.25423E+13, 0.43825E+01,-0.97807E+01, 0.00000E+00, 0.00000E+00,
     C 0.00000E+00/
      DATA PP4/ 0.00000E+00, 0.00000E+00, 0.00000E+00,-0.16460E+01,
     C 0.00000E+00, 0.14276E+01, 0.25523E+13,-0.68300E-01, 0.25623E+13,
     C 0.31487E+01, 0.00000E+00, 0.00000E+00, 0.00000E+00,-0.17942E+02,
     C 0.25114E+13, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C 0.31147E+02, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.49006E+01,
     C 0.25214E+13,-0.13890E+01, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C-0.82595E+02, 0.16338E+03, 0.25314E+13,-0.11818E+02, 0.13203E+02,
     C 0.00000E+00, 0.00000E+00, 0.36882E+02,-0.38961E+02, 0.25414E+13,
     C 0.14468E+01,-0.58022E+01, 0.00000E+00, 0.00000E+00, 0.10382E+02,
     C 0.25514E+13, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C-0.46150E+01, 0.25614E+13, 0.21488E+01, 0.00000E+00, 0.00000E+00,
     C 0.00000E+00,-0.56096E+01, 0.25124E+13,-0.32589E+01, 0.53944E+01,
     C 0.25224E+13,-0.49784E+02, 0.19046E+03,-0.18472E+03, 0.00000E+00,
     C 0.16002E+02, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C 0.00000E+00/
      DATA PP5/ 0.22656E+01, 0.25324E+13, 0.13610E+00, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.45565E+02,-0.65437E+02,
     C 0.25424E+13,-0.42560E+00, 0.25524E+13, 0.00000E+00, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00, 0.45573E+01, 0.25624E+13, 0.11595E+02,
     C-0.21803E+02, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00, 0.30548E+02, 0.00000E+00,-0.84679E+01,
     C 0.25115E+13, 0.62770E+00, 0.25215E+13,-0.42580E+00, 0.25415E+13,
     C-0.72950E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.11346E+02,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C-0.12784E+01, 0.25615E+13, 0.91120E+00, 0.25225E+13, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.32608E+02, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00,-0.55856E+01, 0.25425E+13,-0.34430E+00,
     C 0.25625E+13, 0.12970E+00/
      DATA IRM,TLBX/-1,-27.0/
      DATA WN,WPIC,EPIMM,QB/938.256,139.65,0.0,0.0/
      DATA WPI,ZXM/135.04,0/
      SAVE
      IF(TLBX.NE.-27.0) GO TO 10
      IPRK=IPRKX
      GOM1=CCS(1)
      GOM2=CCS(2)
      GPI2=CCS(3)
      GP1=CCS(4)
      GP2=CCS(5)
      DO 51 M=1,6
      DO 51 J=1,2
      DO 51 L=1,6
      NF(M,J,L)=0
      IF(L.GT.2) NF(M,J,L)=3
      EMR(M,J,L)=0.0
      EMI(M,J,L)=0.0
      EMPI(M,J,L)=0.0
      DO 51 K=1,15
   51 PEM(K,M,J,L)=0.0
      DO 54 N=1,13
   54 NTL(N)=NTC(N)
      I=1
   52 Z=PP(I)/1.E8
      NL=Z+0.1
      NFM=NL/1000
      NL=NL-1000*NFM
      M=NL/100
      NL=NL-100*M
      J=NL/10
      L=NL-10*J
      NF(M,J,L)=NFM
      K=0
   53 I=I+1
      IF(I.GT.IMX) GO TO 10
      IF(PP(I).GT.1.E8) GO TO 52
      K=K+1
      PEM(K,M,J,L)=PP(I)
      GO TO 53
C  10 CONTINUE
   10 IF(TLB.EQ.TLBX.AND.IR.EQ.IRM) GO TO 97
      TLBX=TLB
      IRM=IR
      CALL PRKIN(TLB,IR,EPI,ZKCM,QCM)
      EPX=EPI
      IF(EPX.LT.0.0) EPX=0.0
      IF(EPX.EQ.EPIMM) GO TO 25
      EPIMM=EPX
      S=(WN+WPIC)**2+2.0*WN*EPX
      QB=WN*SQRT(EPX*(EPX+2.0*WPIC)/S)
      CALL PRKIN(TLB,1,EPZ,ZKZ,Q0)
      IF(IPRK.NE.1) EPZ=EPX
      IF(EPZ.LT.0.5) EPZ=0.5
      CALL PNFIXD(EPX,0,TPNR,TPNI,TTLPN)
      CALL PRBORN(TLB,EMPI,5)
   25 DO 11 MM=1,6
      DO 11 JJ=1,2
      DO 11 LL=1,6
      NNF=NF(MM,JJ,LL)
      NROT=NNF/10
      NNF=NNF-10*NROT
      DER=0.
      DEI=0.
      IF(QCM.LE.0.0) GO TO 12
      IF(NNF.LE.0) GO TO 12
      IF(NNF.GT.10) GO TO 12
      N=2
      IF(MM.GT.2) N=0
      N=N+JJ
      IF(N.EQ.1.AND.LL.EQ.1) GO TO 12
      IF(N.EQ.3.AND.LL.EQ.1) GO TO 12
      TER=TPNR(N,LL)
      TEI=TPNI(N,LL)
c     mjl=100*mm+10*jj+ll-1
C     if(mjl.eq.320) write(*,224) tlb,ir,epx,epz
C 224 format(f8.2,i3,2f9.3)
   13 BRN=EMPI(MM,JJ,LL)
      Z=EPX/WPI
      IF(NNF.GT.4) Z=EPX/(EPX+800.0)
      QK=QB/ZKCM
      NEO=0
      IF(NNF.EQ.2) NEO=2
      IF(NNF.EQ.3) NEO=2
      IF(NNF.EQ.5) NEO=4
C SUPPRESS ZR,ZB AT THRESHOLD BY QK**NEO
      ZR=0.0
      IF(QB.LE.0.0) GO TO 24
      ZR=Z*(PEM(6,MM,JJ,LL)+Z*(PEM(7,MM,JJ,LL)+Z*PEM(8,MM,JJ,LL)))
      ZR=(ZR+PEM(5,MM,JJ,LL))*WPI/QB
      IF(LL.EQ.1) GO TO 24
      ZZ=1.0/QK
      IF(IPRK.EQ.1) ZZ=QCM*ZKCM/QB**2
      ZR=ZR*ZZ**(LL-1)
   24 IF(NNF.GT.2) GO TO 1
      ZB=Z*(PEM(2,MM,JJ,LL)+Z*(PEM(3,MM,JJ,LL)+Z*PEM(4,MM,JJ,LL)))
      ZB=ZB+PEM(1,MM,JJ,LL)
      IF(LL.GT.1) ZB=ZB*QK**(LL-1)
      GO TO 3
    1 WX=2.0*WPI
      ZX=SQRT(QCM**2+WX**2)/QCM
      IF(ZX.NE.ZXM) CALL QJOFX(QL,ZX,8)
      ZXM=ZX
      ZB=PEM(1,MM,JJ,LL)*QL(LL)+PEM(2,MM,JJ,LL)*QL(LL+1)
      ZB=ZX*(ZB+PEM(3,MM,JJ,LL)*QL(LL+2)+PEM(4,MM,JJ,LL)*QL(LL+3))
    3 IF(NEO.GT.0) ZB=ZB*QK**NEO
      IF(NEO.GT.0) ZR=ZR*QK**NEO
      ZB=ZB+BRN
      IF(LL.EQ.1) GO TO 32
      ZZ=1.0
      IF(IPRK.NE.1) GO TO 33
      ZZ=QCM/Q0
   33 ZB=ZB*ZZ**(LL-1)
   32 DER=ZB*(1.0-TEI)+ZR*TER
      DEI=ZB*TER+ZR*TEI
      ZPR=PEM(9,MM,JJ,LL)+Z*PEM(10,MM,JJ,LL)
      ZPI=PEM(11,MM,JJ,LL)+Z*PEM(12,MM,JJ,LL)
      SGR=TEI-TER**2-TEI**2
      IF(SGR.LE.0.0001) GO TO 12
      IF(NROT.NE.2) GO TO 2
      DER=DER+ZPR*SGR
      DEI=DEI+ZPI*SGR
      GO TO 12
    2 ZPR=ZPR*SGR*0.0174532
      Z=DEI
      S=SIN(ZPR)
      C=COS(ZPR)
      DEI=C*Z+S*DER
      DER=C*DER-S*Z
   12 EMR(MM,JJ,LL)=DER
      EMI(MM,JJ,LL)=DEI
C     if(mjl.ne.320) go to 11
C     write(*,223) tlb,epz,ir,iprk,ter,tei,zr,zb,brn,der,dei
C 223 format(2f7.2,2i3,/7f9.4)
   11 CONTINUE
   97 RETURN
      END
C ******************************************************                
      SUBROUTINE PNFIXD(EX,IRR,TRZ,TIZ,NTL)                                
C Get SAID partial waves. Parameters are in DATA statements
C E is Tlab(MeV), IR=0(PiN),1(Pi+P),2(Pi-P),3(Cxs)
C T(N,L) is PW for l=L-1, and N=1(I=1/2,J-), 2(I=1/2,J+),3(I=3/2,J-)
C and 4(I=3/2,J+) eg (N,L)=(2,1) for S11, (4,1) for S31, (4,2) for P33
C (1,4) for F15 .......
C NTL is set on 1st call and is a "title" for the SAID solution encoded
      DIMENSION PP(309),NNTL(13),PP1(70),PP2(70),PP3(70),PP4(70)
     C,PP5(29)
      DIMENSION TR(4,8),TI(4,8),NFM(4,8),P(30,4,8),TRZ(4,8),TIZ(4,8)
      DIMENSION NTL(13),W1(3),W2(3),DW2(3),V(8,3),VI(8,3),BPL(8)
      CHARACTER HTL*52
      EQUIVALENCE (PP,PP1),(PP(71),PP2),(PP(141),PP3)
     C,(PP(211),PP4),(PP(281),PP5)
      DATA HTL/'FA01 606075 47250/23862 P+=22177/10447 P-=19250/ 955'/
      DATA IMX/309/
      DATA PP1/ 0.12100E+11, 0.41288E+00,-0.13924E+01, 0.13877E+01,
     C 0.31046E+00, 0.17271E+04, 0.00000E+00, 0.17986E+01,-0.11787E+01,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00, 0.74568E+00, 0.00000E+00, 0.72014E+00,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00,-0.17839E+02, 0.42738E+02,
     C 0.00000E+00, 0.00000E+00, 0.29133E+00,-0.12634E+03,-0.17319E+04,
     C-0.34065E+00, 0.43100E+11, 0.55000E+03, 0.14100E+11,-0.23745E+00,
     C-0.40815E+01, 0.24988E+01, 0.45263E+01, 0.00000E+00,-0.69706E+01,
     C 0.19502E+02,-0.32668E+02, 0.64469E+02, 0.20457E+01, 0.11200E+11,
     C-0.14944E+01, 0.14949E+02,-0.20730E+02, 0.87321E+01, 0.00000E+00,
     C 0.56328E+01,-0.41320E+01, 0.23247E+01, 0.13954E+00, 0.13365E+01,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00,-0.76876E+01, 0.73057E+01,
     C 0.18855E+00, 0.12200E+11,-0.55000E+00, 0.71356E-01,-0.23340E+01,
     C 0.77392E+01, 0.00000E+00,-0.54956E+01, 0.12286E+02,-0.40895E+01,
     C 0.12199E+02/
      DATA PP2/ 0.13200E+11,-0.10257E+01, 0.14949E+01,-0.17620E+02,
     C 0.21060E+02, 0.00000E+00,-0.73219E+01, 0.12156E+02, 0.00000E+00,
     C 0.40128E+01, 0.14200E+11, 0.20881E+01,-0.35999E+01, 0.20928E+00,
     C 0.00000E+00, 0.13799E+04, 0.26112E+01,-0.17167E+01, 0.41016E+01,
     C-0.25959E+01, 0.84030E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C-0.18774E+01, 0.12122E+01, 0.11300E+11, 0.77035E+00, 0.40153E+00,
     C-0.16197E+00, 0.00000E+00, 0.00000E+00, 0.82027E+01, 0.00000E+00,
     C-0.84116E+02, 0.16688E+03, 0.64067E+00, 0.00000E+00, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00,-0.77289E+00, 0.00000E+00,
     C 0.17007E+01, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.97351E+00,
     C 0.12300E+11, 0.64725E+00,-0.10737E+01, 0.17166E+01,-0.24625E+00,
     C 0.00000E+00, 0.55684E+00, 0.56053E+01,-0.43311E+02, 0.70823E+02,
     C 0.11410E+01/
      DATA PP3/ 0.13300E+11, 0.31341E+00,-0.15511E+01, 0.12276E+01,
     C 0.00000E+00, 0.00000E+00, 0.20157E+01, 0.34136E+00,-0.96421E+01,
     C 0.16231E+02, 0.10519E+01, 0.14300E+11,-0.41618E+00, 0.55328E-01,
     C 0.40027E+00, 0.00000E+00, 0.00000E+00,-0.11796E+01, 0.22979E+01,
     C-0.12290E+02, 0.15086E+02, 0.00000E+00,-0.82172E+00, 0.11400E+11,
     C 0.27747E+00, 0.12220E+01, 0.24501E+00,-0.47759E+00, 0.00000E+00,
     C 0.88028E+01, 0.00000E+00,-0.11498E+03, 0.16499E+03, 0.91327E+00,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C 0.12633E+00,-0.92953E+02,-0.17944E+04, 0.12400E+11,-0.72357E-01,
     C 0.12544E-02, 0.68633E-01, 0.00000E+00, 0.00000E+00, 0.81698E+00,
     C 0.00000E+00, 0.15558E+02,-0.13594E+02, 0.13400E+11, 0.38952E-01,
     C-0.92170E+00, 0.58149E+00, 0.00000E+00, 0.00000E+00, 0.17256E+01,
     C 0.00000E+00/
      DATA PP4/-0.14663E+02, 0.19257E+02, 0.10322E+01, 0.14400E+11,
     C 0.53698E+00,-0.58597E+00, 0.22586E+01, 0.00000E+00, 0.00000E+00,
     C-0.17342E+01, 0.52315E+01,-0.19563E+02, 0.22496E+02, 0.11500E+11,
     C 0.15721E+00, 0.89538E+00,-0.10520E+00, 0.00000E+00, 0.00000E+00,
     C 0.14820E+01, 0.00000E+00,-0.79882E+01, 0.74380E+01, 0.12500E+11,
     C 0.22720E+00,-0.62682E+00, 0.63407E+00, 0.00000E+00, 0.00000E+00,
     C 0.10044E+01, 0.00000E+00,-0.51328E+01, 0.49378E+01, 0.13500E+11,
     C 0.12831E+00,-0.83336E+00, 0.59115E+00, 0.00000E+00, 0.00000E+00,
     C 0.83169E+00, 0.14500E+11,-0.68775E-01, 0.12994E+00, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.78717E+00, 0.11600E+11,
     C 0.17199E+00, 0.10118E+01,-0.18166E+01, 0.34438E+01, 0.00000E+00,
     C 0.33512E+01, 0.00000E+00, 0.30161E+02,-0.22767E+02, 0.12600E+11,
     C 0.87780E-01,-0.10616E+01, 0.20919E+01,-0.11497E+01, 0.13600E+11,
     C 0.10591E+00,-0.33431E+00, 0.00000E+00, 0.00000E+00, 0.00000E+00,
     C 0.67893E+00/
      DATA PP5/ 0.14600E+11, 0.23625E+00, 0.53320E+00,-0.16009E+01,
     C 0.12428E+01, 0.00000E+00, 0.47710E+00, 0.11700E+11, 0.18644E+00,
     C 0.19469E+00, 0.12700E+11, 0.18686E+00,-0.37530E+00, 0.00000E+00,
     C 0.00000E+00, 0.00000E+00, 0.00000E+00, 0.54649E+00, 0.13700E+11,
     C 0.14004E+00, 0.22806E+00,-0.47070E+00, 0.00000E+00, 0.00000E+00,
     C 0.76494E+00, 0.14700E+11, 0.10472E+00, 0.48230E+00,-0.30032E+00/
      DATA W1/139.65,139.65,938.256/
      DATA W2/938.256,1212.0,547.3/
      DATA DW2/1.0,102.0,0.01/
      DATA NNL,NCH,WI,WT/8,3,139.65,938.256/
      DATA IRM,EM,S11L,NSTRT/-1,0.0,0.0,0/
      DATA WSUB,ETH/150.0,5.0/
      SAVE
      IF(NSTRT.EQ.1) GO TO 1
      DO 51 L=1,8
      DO 51 N=1,4
      NFM(N,L)=0
      TR(N,L)=0.0
      TI(N,L)=0.0
      DO 51 J=1,30
   51 P(J,N,L)=0.0
      NSTRT=1
      DO 54 N=1,13
   54 NTL(N)=NNTL(N)
      I=1
   52 Z=PP(I)/1.E8
      NL=Z+0.1
      NF=NL/100
      NL=NL-100*NF
      N=NL/10
      L=NL-10*N
C     IF(N.EQ.3.AND.L.EQ.1) NF=0
      NFM(N,L)=NF
      J=0
   53 I=I+1
      IF(I.GT.IMX) GO TO 1
      IF(PP(I).GT.1.E8) GO TO 52
      J=J+1
      P(J,N,L)=PP(I)
      IF(J.EQ.1.AND.N.EQ.3.AND.L.EQ.1) WSUB=PP(I)
      GO TO 53
    1 E=EX
      IF(E.LT.ETH) E=ETH
      IF(EM.NE.E) IRM=-27
      IF(TR(2,1).NE.S11L) IRM=-27
      IF(IRR.EQ.IRM) GO TO 98
      IRM=IRR
      IR=IRR
      IF(IR.GT.3) IR=IR-3
      EM=E
      TLB=E
      DO 55 L=1,8
      DO 55 N=1,4
      TR(N,L)=0.0
   55 TI(N,L)=0.0
      IF(IR.LT.0.OR.IR.GT.3) GO TO 99
C SMALLEST ENERGIES SEEM TO BREED TROUBLE
C     IF(TLB.LT.0.2) GO TO 99
      W=SQRT((WI+WT)**2+2.*TLB*WT)
      QPQ=(W**2-1074.7**2)*(W**2-804.6**2)
      QPQ=SQRT(QPQ/(W**2-(WI+WT)**2)/(W**2-(WT-WI)**2))
      DO 7 N=1,NCH
      WSU=W1(N)+W2(N)
      WC=WI+WT+140.0
      IF(N.EQ.1) WC=WSU-WSUB
      GU=-DW2(N)/2.
      IF(N.EQ.1.) GU=0.
      WIM=0.0
   13 CALL CMFN(W,WIM,WC,WSU,GU,NNL,V(1,N),VI(1,N))
      IF(V(1,N).NE.0.0) GO TO 7
      WIM=WIM+1.0
      GO TO 13
    7 CONTINUE
      ETA=IR
      IF(ETA.GT.1.0) ETA=-1.0
      XKMEV=SQRT(TLB*(TLB+2.0*WI)/((1.0+WI/WT)**2+2.0*TLB/WT))
      IF(IR.EQ.3) XKMEV=XKMEV*SQRT(QPQ)
      XKM=197.32/XKMEV
      PZR=SQRT(XKMEV**2+W2(1)**2)
      QZR=SQRT(XKMEV**2+W1(1)**2)
      ETA=ETA*.007297348*(QZR*PZR+XKMEV**2)/XKMEV/(PZR+QZR)
C  PUT COULOMB BARRIER FACTORS INTO VI(L,5) 8/26/82 ARNDT
      IF(ETA.GT.100.) ETA=100.
      Z=2.*3.1415927*ETA
      BL=1.
      IF(ETA.NE.0.) BL=Z/(EXP(Z)-1.)
      Z=0.
      ZZ=SQRT(QPQ)
      DO 8 L=1,NNL
      BPL(L)=BL
      Z=Z+1.
      IF(IR.NE.3) GO TO 8
      BPL(L)=SQRT(BL)*ZZ
      ZZ=ZZ*QPQ
    8 BL=BL*(1.+(ETA/Z)**2)
      DO 9 LL=1,NNL
      DO 9 NN=1,4
      NNF=NFM(NN,LL)
      TRX=0.
      TIX=0.
      NL=10*NN+LL-1
      IF(NN.EQ.1.AND.LL.EQ.1) GO TO 9
      IF(NN.EQ.3.AND.LL.EQ.1) GO TO 9
      IF(NNF.EQ.1) CALL TMCM(TLB,NL,IRR,P(1,NN,LL),V,VI,TRX,TIX)
C  ENCODE C-M K-MTX FIT FOR FORM 4 9/23/81 ARNDT
      IF(NNF.EQ.1) GO TO 14
      IF(NNF.LT.3.OR.NNF.GT.6) GO TO 10
      BL=BPL(LL)
      IF(BL.EQ.0.0) BL=1.0
      CER=V(LL,1)
      CEI=VI(LL,1)
      LI=LL
      IF(NN.EQ.1.OR.NN.EQ.3) LI=LL-2
      IF(LI.LT.1) LI=LI+2
      IF(LL.EQ.1) LI=3
      NIL=2
      IF(NIL.GT.NCH) NIL=NCH
      CIR=V(LI,NIL)
      CII=VI(LI,NIL)
      WTH=W1(1)+W2(1)
      WPITH=WTH+140.
      WCM=SQRT(WTH**2+2.*W2(1)*TLB)
      Z=(WCM-WPITH)/1000.
      ZZ=1.
      ZE=0.0
      WKP=P(5,NN,LL)
      DRL=1.0
C MASS-SPLIT K-MTX POLE PIECE FOR P33 1/95 ARNDT
      IF(WKP.EQ.0.0) GO TO 34
      DWK=P(18,NN,LL)/2.0
      IF(NL.NE.41) DWK=0.0
      IF(BL.GT.1.0001) WKP=WKP+DWK
      IF(BL.LT.0.9999) WKP=WKP-DWK
      ZZ=WKP-WTH
      DRL=WKP-WCM
      DGK=P(17,NN,LL)/2.0
      IF(DWK.EQ.0.0) DGK=0.0
      IF(DGK.EQ.0.0) GO TO 34
      IF(BL.GT.1.0001) ZE=DGK
      IF(BL.LT.0.9999) ZE=-DGK
   34 CONTINUE
      IF(NNF.GT.2) Z=(WCM-WTH)/1000.0
      LIP=LI+2
      IF(LIP.GT.8) LIP=LIP-2
      DO 12 J=1,4
      ZE=ZE+P(J,NN,LL)*ZZ
   12 ZZ=ZZ*Z
      ZEI=0.
      DIM=0.
      DO 31 J=1,3
      IF(J.NE.3) GO TO 33
      CIR=V(LIP,2)
      CII=VI(LIP,2)
      IF(NNF.EQ.3.OR.NNF.EQ.5) GO TO 33
      CIR=V(LL,3)
      CII=VI(LL,3)
   33 CONTINUE
C     IF(CII.LT.0.0) CII=0.0
      K=2+4*J
      Z0=Z*P(K,NN,LL)+Z**2*P(K+1,NN,LL)
      IF(Z0.EQ.0.0) GO TO 31
      ZZ=P(K+2,NN,LL)+Z*P(K+3,NN,LL)
      IF(NNF.GT.4) ZZ=ZZ*Z
      DIR=1.0-CIR*ZZ
      DII=-CII*ZZ
      Z2R=Z0**2*CIR
      Z2I=CII*Z0**2
      ZZ=Z2R
      Z2R=ZZ*DRL-Z2I*DIM
      Z2I=ZZ*DIM+Z2I*DRL
      ZZ=ZE
      ZE=ZE*DIR-ZEI*DII+Z2R
      ZEI=ZZ*DII+ZEI*DIR+Z2I
      ZZ=DRL
      DRL=DRL*DIR-DIM*DII
      DIM=ZZ*DII+DIM*DIR
   31 CONTINUE
      DRL=DRL-CER*ZE+CEI*ZEI
      DIM=DIM-CER*ZEI-CEI*ZE
      D2=DRL**2+DIM**2
      Z=CEI/D2
      TRX=Z*(ZE*DRL+ZEI*DIM)
      TIX=Z*(ZEI*DRL-ZE*DIM)
   14 IF(IRR.GT.3) BL=1.0
      IF(BL.EQ.1.0) GO TO 11
      IF(BL.GT.1.0.AND.NL.EQ.41) CALL ETA33(TLB,TRX,TIX)
      CALL PWCC(TLB,TRX,TIX,NL,BL,NFM(3,1),IRR)
   11 IF(TIX.GE.1.0) TRX=0.0
      IF(TIX.GT.1.0) TIX=1.0
      IF(NNF.NE.1) CALL ADDRES(E,TRX,TIX,NL,P(19,NN,LL))
      IF(TIX.GE.TRX**2+TIX**2) GO TO 10
      D2=1.0+TRX**2+TIX**2-2.0*TIX
      IF(D2.LT.1.E-20) WRITE(7,224) TLB,WCM,TRX,TIX
  224 FORMAT(' TLB, WCM=',2F8.2,' TR,TI=',2F9.5)
      IF(D2.LT.1.E-20) GO TO 10
      Z=TRX/D2
      TRX=Z/(1.+Z**2)
      TIX=Z*TRX
   10 TR(NN,LL)=TRX
    9 TI(NN,LL)=TIX
      S11L=TR(2,1)
      IF(WI.GT.150.0) GO TO 98
      IF(NFM(3,1).NE.4) GO TO 98
C add in f13 corrections for S11, P13 ONLY for TROMBERG
      L=1
      CALL TROMF13(TLB,L,IR,TR(2,L),TI(2,L),TR(4,L),TI(4,L))
      L=2
      CALL TROMF13(TLB,L,IR,TR(2,L),TI(2,L),TR(4,L),TI(4,L))
      S11L=TR(2,1)
   98 Z=1.0
      IF(EX.LT.ETH) Z=EX/ETH
      ZZ=SQRT(Z)
      DO 97 L=1,8
      IF(L.GT.1) ZZ=ZZ*Z
      DO 97 N=1,4
      TRZ(N,L)=TR(N,L)
      TIZ(N,L)=TI(N,L)
      IF(Z.EQ.1.0) GO TO 97
      TRZ(N,L)=ZZ*TR(N,L)
      TIZ(N,L)=TRZ(N,L)**2
   97 CONTINUE
   99 RETURN
      END
C *********************************************************
      SUBROUTINE TMCM(TLB,NL,IRR,P,V,VI,TRX,TIX)
C Coupled-Channel CM-K-mtx for FORM=1 9/3/01 RAA
      DIMENSION V(8,3),VI(8,3),P(30)
      DIMENSION TRL(10),TIM(10),ARC(10),AIC(10),RR(10),RI(10)
     C,CCR(4),CCI(4),RH(4)
      DATA WI,WT/139.65,938.256/
      SAVE
      NN=NL/10
      LL=NL-10*NN+1
      IRX=IRR
      IF(IRR.GT.3) IRX=IRR-3
C ??? don't know WHAT this is
      WSE=WI+WT
      WRL=SQRT(WSE**2+2.0*WT*TLB)
C do 4x4 K-matrix to channels pipi, pid, pieta(or pid+)
C P= K11(4), WkP, K12(2), K22(2), K13(2), K23(2), K33(2)
C K14(2), K24(2), K34(2), K44(2), dWk, dGk, addres(4)
      LI=LL
      IF(NN.EQ.1.OR.NN.EQ.3) LI=LL-2
      IF(LI.LT.1) LI=LI+2
      IF(LL.EQ.1) LI=3
      LIP=LI+2
      ZZR=(WRL-WSE)/1000.0
      Z2R=ZZR**2
      WKP=P(5)
      IF(IRX.EQ.2.OR.IRX.EQ.3) WKP=WKP+P(25)
      WKPR=1.0
      IF(WKP.NE.0.0) WKPR=(WKP-WRL)/1000.0
      NCX=4
      IF(NN.GT.2) NCX=3
      JMX=NCX*(NCX+1)
      JMX=JMX/2
      DO 2 J=2,JMX
      K=2*J+2
      ZR=P(K)*ZZR+P(K+1)*Z2R
      IF(J.LT.7.OR.J.GT.9) GO TO 2
      ZR=P(K)+(WRL/1000.0-1.4)*P(K+1)
    2 ARC(J)=ZR*WKPR
      ZR=1.0
      GE=0.0
      IF(WKP.EQ.0.0) GO TO 9
      ZR=(WKP-WSE)/1000.0
      IF(IRX.EQ.2.OR.IRX.EQ.3) GE=P(24)/1000.0
    9 DO 3 K=1,4
      GE=GE+P(K)*ZR
    3 ZR=ZR*ZZR
      ARC(1)=GE
      IF(ARC(7).EQ.0.0) NCX=3
      IF(ARC(4).EQ.0.0.AND.NCX.EQ.3) NCX=2
      IF(ARC(2).EQ.0.0.AND.NCX.EQ.2) NCX=1
      JD=1
      CRX=V(LL,1)
      CIX=VI(LL,1)
      DO 4 J=1,NCX
      IF(J.EQ.2) CRX=V(LI,2)
      IF(J.EQ.2) CIX=VI(LI,2)
      IF(J.EQ.3) CRX=V(LIP,2)
      IF(J.EQ.3) CIX=VI(LIP,2)
      IF(J.EQ.4) CRX=V(LL,3)
      IF(J.EQ.4) CIX=VI(LL,3)
      CCR(J)=CRX
      CCI(J)=CIX
      DO 5 K=1,J
      TRL(JD)=-ARC(JD)
      TIM(JD)=0.0
    5 JD=JD+1
      D2=CRX**2+CIX**2
      TRL(JD-1)=WKPR*CRX/D2-ARC(JD-1)
      TIM(JD-1)=-WKPR*CIX/D2
    4 CONTINUE
      CALL CMSINV(TRL,TIM,RR,RI,NCX)
      JK=1
      DO 6 J=1,NCX
      JD=J*(J-1)
      JD=JD/2
      RH(J)=CCI(J)
      IF(RH(J).LT.0.0) RH(J)=0.0
      RH(J)=SQRT(RH(J))
      DO 6 K=1,J
      KD=K*(K-1)
      KD=KD/2
      ZR=0.0
      ZI=0.0
      DO 7 M=1,NCX
      MD=M*(M-1)
      MD=MD/2
      JM=JD+M
      IF(M.GT.J) JM=MD+J
      MK=KD+M
      IF(M.GT.K) MK=MD+K
      ZR=ZR+ARC(JM)*RR(MK)
      ZI=ZI+ARC(JM)*RI(MK)
    7 CONTINUE
      DCR=CCR(K)
      DCI=CCI(K)
      D2=DCR**2+DCI**2
      TRX=RH(J)*RH(K)*(DCR*ZR+DCI*ZI)/D2
      TIX=RH(J)*RH(K)*(DCR*ZI-DCI*ZR)/D2
      IF(IRX.LT.5) GO TO 8
      IF(JK.EQ.7) GO TO 99
    6 JK=JK+1
    8 KP1=26
      CALL ADDRES(TLB,TRX,TIX,NL,P(KP1))
      IF(IRX.GT.1.AND.NL.EQ.41) CALL ETA33(TLB,TRX,TIX)
C KILL BARRIER FACTOR IF HADRONIC IS NEEDED
      if(nl.ne.41) go to 99
   99 RETURN
      END
C *********************************************************
      SUBROUTINE CMSINV(AR,AI,AIR,AII,N)
      DIMENSION AR(10),AI(10),AIR(10),AII(10),WR(60),WI(60)
      SAVE
C  INVERT COMPLEX-SYMMETRIC MATRIX
C  MATRICES ARE STORED A11,A12,A22,A13,...  N=ORDER OF MATRIX
C  G=INV OF DIAGONAL ELEMENT (M)  M=SINGULAR ORDER  W=WORKING SPACE
      M=0
      JD=0
      GR=0.0
      GI=0.0
    1 M=M+1
      JD=JD+M
      GR=AR(JD)
      GI=AI(JD)
      IF(M.EQ.1) GO TO 2
      MM=M-1
      CALL MCMCV(AIR,AII,AR(JD-MM),AI(JD-MM),WR,WI,MM)
      JJ=JD-M
      DO 3 K=1,MM
      JJ=JJ+1
      GR=GR-AR(JJ)*WR(K)+AI(JJ)*WI(K)
    3 GI=GI-AR(JJ)*WI(K)-AI(JJ)*WR(K)
    2 D2=GR**2+GI**2
      IF(D2.LT.1.E-9) D2=1.E-9
C Note!! This is to take care of SINGULAR K-mtx  9/12/01 RAA
      GR=GR/D2
      GI=-GI/D2
      AIR(JD)=GR
      AII(JD)=GI
      IF(M.EQ.1) GO TO 5
      J0=1
      DO 4 J=1,MM
      ZR=GR*WR(J)-GI*WI(J)
      ZI=GR*WI(J)+GI*WR(J)
      KK=JD-M+J
      AIR(KK)=-ZR
      AII(KK)=-ZI
      DO 4 K=1,J
      ZZR=ZR*WR(K)-ZI*WI(K)
      ZZI=ZR*WI(K)+ZI*WR(K)
      AIR(J0)=AIR(J0)+ZZR
      AII(J0)=AII(J0)+ZZI
    4 J0=J0+1
    5 IF(M.LT.N) GO TO 1
      RETURN
      END
C *********************************
      SUBROUTINE MCMCV(AR,AI,VR,VI,PR,PI,N)
C MULTIPLY COMPLEX MATRIX(A) ON COMPLEX VECTOR(V) TO GET PRODUCT(P)
      DIMENSION AR(20),AI(20),VR(20),VI(20),PR(20),PI(20)
      J0=0
      DO 1 J=1,N
      ZR=0.0
      ZI=0.0
      DO 2 I=1,J
      IJ=J0+I
      ZR=ZR+AR(IJ)*VR(I)-AI(IJ)*VI(I)
    2 ZI=ZI+AR(IJ)*VI(I)+AI(IJ)*VR(I)
      IF(J.GE.N) GO TO 3
      JP=J+1
      DO 4 I=JP,N
      IJ=I*(I-1)
      IJ=IJ/2+J
      ZR=ZR+AR(IJ)*VR(I)-AI(IJ)*VI(I)
    4 ZI=ZI+AR(IJ)*VI(I)+AI(IJ)*VR(I)
    3 PR(J)=ZR
      PI(J)=ZI
    1 J0=J0+J
      RETURN
      END
C *******************************************
      SUBROUTINE PWCC(TLB,TRX,TIX,NL,BL,NCC,IRZ)
      DATA WI,PI/139.65,3.1415927/
C NCC=5(NO CC), 4(Nordita S,P waves Tl<500), 6(Barrier+"h")
C otherwise use "Barrier" multiplication of K(Hadronic)
      IF(TLB.LT.0.5) GO TO 99
      IF(BL.EQ.1.) GO TO 99
      IF(NCC.EQ.5) GO TO 99
      IF(NCC.LT.6) GO TO 2
C Try adding "H" correction to Eff-Rng Charge-corrections 5/8/00
      T2=TRX**2+TIX**2
      D2=1.0+T2-2.0*TIX
      ZHR=TRX/D2
      ZHI=(TIX-T2)/D2
      B=SQRT(TLB*(TLB+2.0*WI))/(TLB+WI)
      E=1.0/B/137.06
      IF(BL.GT.1.0) E=-E
      h=0.0
      XX=0.0
      Z=1.0
      DO 3 J=1,10
      XX=XX+1.0/Z/(1.0+(E/Z)**2)
    3 Z=Z+1.0
      Z=2.0*PI*E
      IF(Z.GT.80.0) Z=80.0
      C2=Z/(EXP(Z)-1.0)
      H=2.0*E*(E**2*XX-0.57721-ALOG(ABS(E)))*BL/C2
      IF(NCC.EQ.7) H=0.0
      ZHR=ZHR
      ZHI=ZHI
      DR=1.0-ZHR*H
      DI=-ZHI*H
      D2=DR**2+DI**2
      ZCR=BL*(ZHR*DR+ZHI*DI)/D2
      ZCI=BL*(ZHI*DR-ZHR*DI)/D2
      Z2=ZCR**2+ZCI**2
      D=1.0+Z2+2.0*ZCI
      TRX=ZCR/D
      TIX=(ZCI+Z2)/D
      GO TO 99
    2 CONTINUE
      IF(NCC.NE.4.AND.NCC.NE.2) GO TO 1
C Nordita corrections to S,P waves for Tl<550, otherwise Barrier
      IF(TLB.GE.535.0) GO TO 1
      NN=NL/10
      LL=NL-10*NN+1
      IF(LL.GT.NCC/2) GO TO 1
C Nordita for S-waves(NNC=2), or S+P-waves(NCC=4)
C Use Barrier factors for all but S-waves MP 5/16/00 Nuts!!
      DD=0.0174532*DTROMB(NL,IRZ,TLB)
      S=SIN(DD)
      C=COS(DD)
      TCR=S*C
      TCI=S**2
      SR=1.0-2.0*TIX
      SI=2.0*TRX
      TRX=TRX+SR*TCR-SI*TCI
      TIX=TIX+SR*TCI+SI*TCR 
      GO TO 99
    1 DR=1.-TIX*(1.-BL)
      DI=TRX*(1.-BL)
      D2=(DR**2+DI**2)/BL
      Z=TRX
      TRX=(Z*DR+TIX*DI)/D2
      TIX=(TIX*DR-Z*DI)/D2
   99 RETURN
      END
C ***************************************
      FUNCTION DTROMB(NL,IS,T)
C     DO QUADRATIC TABLE LOOKUP OF TROMBERG PHASES 
C     K=1(S31),2(P31),3(P33)               Pi+P
C     4(S11),5(S31),6(P31),7(P13),8(P33)   Pi-P/CXS
C     n.b.!! corrections adjusted  so  corr =  del_nuc - del_had
C     (Tromborg had -1/3, -2/3 factors for I=3,1  pi- corrections)
C     TI = 10(25)535 MEV
C     modified June 16/00 by M.M. Pavan
c              Aug  21/01 by MMP   include P31- (Helv.Phys.Acta51,584,1978) 
      DIMENSION F(176)
      DATA F/0.110, 0.093, 0.091, 0.100, 0.100, 0.110, 0.121, 0.120
     +     , 0.131, 0.130, 0.130, 0.130, 0.130, 0.132, 0.136, 0.139
     +     , 0.141, 0.143, 0.143, 0.143, 0.142, 0.140
     +     , 0.010, 0.012, 0.024, 0.040, 0.049, 0.068, 0.074, 0.090 
     +     , 0.092, 0.105, 0.116, 0.124, 0.129, 0.135, 0.141, 0.148
     +     , 0.156, 0.164, 0.173, 0.182, 0.192, 0.202
     +     ,-0.043,-0.120,-0.277,-0.517,-0.870,-1.287,-1.450,-1.117
     +     ,-0.616,-0.229, 0.009, 0.153, 0.234, 0.289, 0.310, 0.317
     +     , 0.327, 0.329, 0.324, 0.312, 0.292, 0.265
     +     , 0.238, 0.177, 0.129, 0.096, 0.069, 0.042, 0.016, 0.000
     +     ,-0.010,-0.024,-0.030,-0.041,-0.057,-0.070,-0.081,-0.091
     +     ,-0.100,-0.109,-0.117,-0.123,-0.129,-0.134
     +     ,-0.206,-0.145,-0.111,-0.098,-0.090,-0.084,-0.082,-0.080
     +     ,-0.076,-0.073,-0.071,-0.069,-0.067,-0.065,-0.063,-0.061
     +     ,-0.058,-0.055,-0.052,-0.049,-0.046,-0.043
     +     ,-0.022,-0.051,-0.076,-0.096,-0.112,-0.126,-0.135,-0.144
     +     ,-0.152,-0.160,-0.170,-0.179,-0.187,-0.195,-0.202,-0.208
     +     ,-0.215,-0.220,-0.224,-0.229,-0.232,-0.235
     +     ,-0.007,-0.021,-0.038,-0.057,-0.072,-0.092,-0.103,-0.114
     +     ,-0.129,-0.139,-0.151,-0.161,-0.176,-0.194,-0.215,-0.236
     +     ,-0.253,-0.268,-0.282,-0.295,-0.306,-0.315
     +     , 0.154, 0.346, 0.543, 0.746, 0.945, 1.020, 0.737, 0.230
     +     ,-0.154,-0.344,-0.405,-0.409,-0.386,-0.358,-0.322,-0.285
     +     ,-0.253,-0.223,-0.195,-0.168,-0.143,-0.119/
      SAVE
C-----Initialize
      DTROMB=0.0
      K=0
C-----Ignore if PW not covered by Tromborg
      IF(IS.LT.1.OR.IS.GT.3) GO TO 99
      IF(T.GT.550.0) GO TO 99
C-----Select PW 
      IF(NL.EQ.40) K=1
      IF(NL.EQ.31.AND.IS.EQ.1) K=2
      IF(NL.EQ.41) K=3
      IF(NL.EQ.20) K=4
      IF(NL.EQ.40.AND.IS.GT.1) K=5
      IF(NL.EQ.31.AND.IS.GT.1) K=6
      IF(NL.EQ.21.AND.IS.GT.1) K=7
      IF(NL.EQ.41.AND.IS.GT.1) K=8
C-----Ignore if PW not covered by Tromborg
      IF(K.EQ.0) GO TO 99
C-----Find PW and energy TI near energy T
C-----n.b. need 3 points for quadratic interp.
      I=(T-10.0)/25.0
      I=I+2
      IF(I.GT.21) I=21
      TI=25*I-15
      I=22*(K-1)+I
c-----Interpolate to energy T using nearest 3 PWs
      F0=F(I)
      FM=F(I-1)-F0
      FP=F(I+1)-F0
C     WRITE(*,222) T,I,K,TI,F0,FM,FP
C 222 FORMAT(' T=',F5.1,' I,K,TI=',2I3,F6.1,' F0,FM,FP=',3F7.3)
      ZM=-25.0
      ZP=25.0
      Z=T-TI
      D=ZM*ZP*(ZP-ZM)
      A=(ZP**2*FM-ZM**2*FP)/D
      B=(ZM*FP-ZP*FM)/D
      DTROMB=F0+Z*(A+Z*B)
   99 RETURN
      END
C **************************************************************
      SUBROUTINE TROMF13(TL,L,MM,T1R,T1I,T3R,T3I)
C add corrections to S11 and P13 for Tromberg's f13  5/16/01 RAA
      DIMENSION P(21),I1(6),NI(6)
      DATA P/0.039,0.366,0.4165,0.0,5.112,-44.511,66.85,70.59
     C,-0.1514,-0.211,0.475,9.98,55.499,-54.3
     C,0.0,0.0,-16.82,13482.0,-55386.0,27.6,-36.73/
      DATA I1/1,4,9,12,15,20/
      DATA NI/3,5,3,3,5,2/
      SAVE
      IF(TL.GT.550.0) GO TO 99
      IF(L.GT.2) GO TO 99
      IF(MM.LT.2) GO TO 99
      ID=1
      IF(L.GT.1) ID=2 
      IF(L.GT.1.AND.TL.GT.250.0) ID=3
      NID=NI(ID)
      ID=I1(ID)
      IE=4
      IF(L.GT.1) IE=5 
      IF(L.GT.1.AND.TL.GT.180.0) IE=6
      NIE=NI(IE)
      IE=I1(IE)
      Z=TL/1000.0
      ZZ=1.0
      D13=0.0
      DO 1 I=1,NID
      D13=D13+P(ID+I-1)*ZZ
    1 ZZ=ZZ*Z
      ZZ=1.0
      E13=0.0
      DO 2 I=1,NIE
      E13=E13+P(IE+I-1)*ZZ
    2 ZZ=ZZ*Z
      E13=E13/10000.0
      D13=0.0174532*D13
C     write(*,222) tl,l,mm,t1r,t1i,t3r,t3i,d13,e13
C 222 format(f7.2,2i3,' t1=',2f7.4,' t3=',2f7.4,' d13,e13=',2e12.4)
      FCT=-1.333333
      IF(MM.EQ.3) FCT=-FCT/2.0
      ZR=E13*FCT
      ZI=D13*FCT
      D1=ATAN(T1I/(T1R+1.0E-8))
      D3=ATAN(T3I/(T3R+1.0E-12))
      IF(D3.LT.0.0) D3=D3+3.1415927
      ZZR=COS(D1+D3)
      ZZI=SIN(D1+D3)
      T1R=T1R+ZR*ZZR-ZI*ZZI
      T1I=T1I+ZR*ZZI+ZI*ZZR
   99 RETURN
      END
C ****************************************************
      SUBROUTINE ADDRES(E,TR,TI,NL,P)
C  add resonance "bump" to partial-wave 8/94 Arndt
      DIMENSION P(30)
      DATA WPI,WN/139.65,938.256/
      SAVE
      IF(P(2).GT.0.0) GO TO 10
      DRL=1.0
      DIM=0.0
      WR=SQRT((WPI+WN)**2+2.0*WN*E)
      WI=0.0
      CALL ADDRES2(WR,WI,DRL,DIM,TR,TI,NL,P)
      GO TO 99
   10 GT=P(2)
      GE=P(1)*GT
      IF(GE.EQ.0.0) GO TO 99
      GI=GT-GE
      IF(GI.LE.0.0) GI=0.0
      ER=P(3)
      IF(ER.EQ.0.0) GO TO 99
C     Z=SQRT(2.0*E**2/(E**2+ER**2))
      Z=2.0*E/(ER+E)
      RE=SQRT(Z)
      N=NL/10
      LE=NL-10*N
      IF(LE.LT.0.OR.LE.GT.8) LE=1
      IF(LE.GT.0) RE=RE*Z**LE
      RI=(E-150.0)/(ER-150.0)
      IF(RI.LT.0.0) RI=0.0
      RI=RE*RI**3
      GE=GE*RE
      GI=GI*RI
      GT=GE+GI
      Z=ER-E
      D=Z**2+GT**2
      ZR=GE*Z/D
      ZI=GE*GT/D
      IF(P(4).EQ.0.0) GO TO 1
      SR=1.0-2.0*ZI
      SI=2.0*ZR
      Z2=ZR**2+ZI**2
      ZE=P(4)*Z2
      D2=1.0+ZE**2
      ZR=ZR+(SR*ZE-SI*ZE**2)/D2
      ZI=ZI+(SR*ZE**2+SI*ZE)/D2
    1 SR=1.0-2.0*TI
      SI=2.0*TR
      TR=TR+SR*ZR-SI*ZI
      TI=TI+SR*ZI+SI*ZR
   99 RETURN
      END
C ******************************************************
      SUBROUTINE ADDRES2(WR,WI,DRL,DIM,TR,TI,NL,P)
C ADD RESONANCE FOR COMPLEX(WR,WI) ENERGY 10/18/94 ARNDT
      DIMENSION P(4)
      DATA WE,WIN/1078.0,1218.0/
      SAVE
      GT=-P(2)
      IF(GT.LT.20.0) GO TO 99
      WRES=ABS(P(3))
      GE=P(1)*GT
      GI=GT-GE
      IF(GI.LT.0.0) GI=0.0
      GE=GT-GI
      N=NL/10
      LE=NL-10*N
      D=1.0/(WR**2+WI**2)
      ZR=1.0-D*WE*WR
      ZIR=1.0-D*WIN*WR
      Z=1.0-WE/WRES
      ZZ=1.0-WIN/WRES
      ZR=ZR/Z
      ZIR=ZIR/ZZ
      IF(WR.LT.WIN) ZIR=0.0
      IF(WI.EQ.0.0) GO TO 1
      ZI=D*WE*WI/Z
      ZII=D*WIN*WI/ZZ
      RER=ZR
      REI=ZI
      CALL SQZ(RER,REI)
      RIR=ZIR**2-ZII**2
      RII=ZIR*ZII*2.0
      IF(LE.LT.1) GO TO 2
      DO 3 L=1,LE
      Z=RER
      RER=Z*ZR-REI*ZI
    3 REI=Z*ZI+REI*ZR
      GO TO 2
    1 REI=0.0
      IF(ZR.LE.0.0) ZR=0.0
      RER=SQRT(ZR)
      IF(LE.GT.0) RER=RER*ZR**LE
      RIR=ZIR**2
      RII=0.0
    2 GER=GE*RER
      GEI=GE*REI
      IF(P(3).GT.0.0) GO TO 8
      ZR=(WR-WE)/(WRES-WE)
      ZI=WI/(WRES-WE)
      Z=GER
      GER=Z*ZR-GI*ZI
      GEI=ZR*GEI+Z*ZI
    8 GIR=GI*RIR
      GII=GI*RII
      GTR=GER+GIR
      GTI=GEI+GII
      DR=WRES-WR+GTI
      DI=-WI-GTR
      Z=DRL
      DRL=Z*DR-DI*DIM
      DIM=Z*DI+DR*DIM
      D2=DR**2+DI**2
      TRR=(GER*DR+GEI*DI)/D2
      TRI=(GEI*DR-GER*DI)/D2
      IF(P(4).EQ.0.0) GO TO 5
      SR=1.0-2.0*TRI
      SI=2.0*TRR
      ZB=P(4)*GER**2/((WRES-WR)**2+GTR**2)
      TBR=ZB/(1.0+ZB**2)
      TBI=TBR*ZB
      TRR=TRR+SR*TBR-SI*TBI
      TRI=TRI+SR*TBI+SI*TBR
    5 SR=1.0-2.0*TI
      SI=2.0*TR
      TR=TR+SR*TRR-SI*TRI
      TI=TI+SR*TRI+SI*TRR
   99 RETURN
      END
C ****************************************************
      SUBROUTINE ETA33(X,TR,TI)
      DIMENSION P(4)
      DATA P/70.71,160.1,221.0,0.0307/
C CORRECTS PI-P,CXS P33 FOR N-G CROSS SECTION X=TLAB 11/91 ARNDT
      SAVE
      BW=P(1)**2/((X-P(2))**2+P(1)**2)
      Z=X**2/(X**2+P(3)**2)
      YS=P(4)*BW*Z
      ETA=1.0-YS
      TR=ETA*TR
      TI=ETA*TI+(1.0-ETA)/2.0
      RETURN
      END
C *********************************************************
      SUBROUTINE PRKIN(E,IRR,EPI,ZKCM,QCM)
C GET PION-PHOTOPRODUCTION KINEMATIC PARAMETERS
      COMMON/PRKC/IPRK
      DATA WP,WN,WPI0,WPIC/938.256,939.65,135.04,139.65/
      DATA EPIT/10.0/
      SAVE
      IR=IRR
      IF(IR.EQ.5) IR=1
      WT=WP
      IF(IR.GT.2) WT=WN
      WX=WP
      IF(IR.EQ.2.OR.IR.EQ.4) WX=WN
      WPI=WPI0
      IF(IR.EQ.2.OR.IR.EQ.3) WPI=WPIC
      ZKCM=E/SQRT(1.0+2.0*E/WT)
      S=WT*(WT+2.0*E)
      QCM=0.0
      EPI=0.0
      STH=(WPI+WX)**2
      IF(S.LE.STH) GO TO 99
      QCM=SQRT((S-STH)*(S-(WPI-WX)**2)/4.0/S)
      EPI=(S-STH)/2.0/WX
      IF(IPRK.EQ.1) GO TO 99
C the following makes Epi dependent upon Wcm and independent of charge
C channel. Generally used for solutions before April 1996. RAA
      EPI=(S-(WP+WPIC)**2)/2.0/WP
      IF(EPI.GE.EPIT) GO TO 99
      ST=(WP+WPIC)**2+2.0*EPIT*WP
      Z=(S-STH)/(ST-STH)
C "STRETCH OUT" THRESHOLD BELOW EPI=10 MEV
      EPI=EPIT*Z**2
   99 RETURN
      END
C ***********************************************************
      SUBROUTINE PRBORN(EPI,EM,NF)
      DIMENSION EM(6,2,6)
      SAVE
      CALL PROPEC(EPI,EM)
      CALL PRNPOL(EPI,EM)
      IF(NF.LT.3) GO TO 99
      CALL PREPV(EPI,EM)
      CALL PROMEGA(EPI,EM)
      IF(NF.LT.5) GO TO 99
      CALL PRRHO(EPI,EM)
   99 RETURN
      END
C ***********************************************************
      SUBROUTINE PROPEC(EL,EM)
      COMMON/GOMEGA/GOM1,GOM2,GOMS,GPI2,GP1,GP2
      DIMENSION EPX(4),E2X(4),GC(4),AA(4),BB(4),CC(4),F(12,4),QT(12)
     C,QU(12),EM(6,2,6)
      REAL qtmp
      DATA SQ2,WP,WN,UP,UN,GN/1.41421,135.04,938.256,1.793,-1.913,62.51/
C GN=SQRT(ALFA*G2)*HBARC = SQRT(13.75/137)*197.32
      DATA B,GPI2M/0,13.75/
      SAVE
      IF(GPI2.EQ.GPI2M) GO TO 20
      IF(GPI2.EQ.0.0) GPI2=GPI2M
      GN=197.32*SQRT(GPI2/137.0)
      GPI2M=GPI2
   20 CONTINUE
      DO 1 M=1,6
      DO 1 J=1,2
      DO 1 L=1,6
    1 EM(M,J,L)=0.0
      S=WN*(WN+2.0*EL)
      W=SQRT(S)
      ZK=EL/SQRT(1.0+2.0*EL/WN)
      Q=SQRT((S-(WN+WP)**2)*(S-(WN-WP)**2)/4.0/S)
      IF(Q.LT.0.0)  GO TO 99
      Z2=SQRT(Q**2+WN**2)
      ZU=-Z2/Q
      ZT=SQRT(Q**2+WP**2)/Q
      Z2=SQRT(Z2+WN)
      Z1=SQRT(SQRT(ZK**2+WN**2)+WN)
      GG=1000.0*GN/W
      USCL=(W+WN)/2.0/Z1/Z2
      AA(1)=0.0
      AA(2)=0.0
      AA(3)=GG*Z2/Z1
      AA(4)=-GG*Z1*Q/Z2/ZK
      BB(1)=-GG*W/Q/USCL/2.0
      BB(2)=GG*W/Z2**2/2.0/USCL
      BB(3)=-AA(3)
      BB(4)=-AA(4)
      GG=GG*USCL
      CC(1)=-GG*Z2**2/Q
      CC(2)=GG
      CC(3)=-GG*Z2**2/WN
      CC(4)=-GG*Q/WN
      CALL QJOFX(QT,ZT,8)
      CALL QJOFX(QU,-ZU,8)
      S=-1.0
      DO 11 L=1,8
      QU(L)=S*QU(L)
   11 S=-S
      M=0
      DO 2 I=1,3
      DO 3 K=1,4
      A=AA(K)
      IF(I.GT.1) A=2.0*A/3.0
      IF(I.NE.2) A=-A
      IF(I.EQ.1) B=CC(K)*(UN-UP)-BB(K)
      IF(I.EQ.2)  B=-(CC(K)*(2.0*UN+UP)+BB(K))/3.0
      IF(I.EQ.3) B=-(CC(K)*(2.0*UP+UN)+2.0*BB(K))/3.0
      DO 3 L=1,8
    3 F(L,K)=(A*QT(L)+B*QU(L))/2.0
      M=M+1
      L=2
    4 L=L+1
      IF(L.GT.6)  GO TO 5
      ZL=L-1
      Z=(ZL+1.0)/(2.0*ZL+1.0)
      ZZ=ZL/(2.0*ZL-1.0)
      E=F(L,1)-F(L-1,2)-Z*(F(L-1,3)-F(L+1,3))-ZZ*(F(L-2,4)-F(L,4))
      EM(M,1,L)=E/ZL
      GO TO 4
    5 L=0
    6 L=L+1
      IF(L.GT.6) GO TO 7
      ZL=L-1
      ZZ=(ZL+1.0)/(2.0*ZL+3.0)
      E=F(L,1)-F(L+1,2)+ZZ*(F(L,4)-F(L+2,4))
C       in order to bypass the boundary violation (L-1=0 !), 0 is used 
C             (I have no better idea) 12 nov 2007 E.Ch.
C
      qtmp=0.
      IF(L.GT.1) qtmp=F(L-1,3)
      EE=ZL/(2.0*ZL+1.0)*(qtmp-F(L+1,3))
      E=E+EE
      EM(M,2,L)=E/(ZL+1.0)
      GO TO 6
    7 M=M+1
      L=1
    8 L=L+1
      IF(L.GT.6) GO TO 9
      ZL=L-1
      EM(M,1,L)=(F(L-1,2)-F(L,1)+(F(L-1,3)-F(L+1,3))/(2.0*ZL+1.0))/ZL
      GO TO 8
    9 L=1
   10 L=L+1
      IF(L.GT.6)  GO TO 2
      ZL=L-1
      E=F(L,1)-F(L+1,2)-(F(L-1,3)-F(L+1,3))/(2.0*ZL+1.0)
      EM(M,2,L)=E/(ZL+1.0)
      GO TO 10
    2 CONTINUE
   99 RETURN
      END
C **************************************************************
      SUBROUTINE PRNPOL(EL,EM)
      COMMON/GOMEGA/GOM1,GOM2,GOMS,GPI2,GP1,GP2
      DIMENSION EM(6,2,6)
      DATA SQ2,WP,WN,UP,UN,GN/1.41421,135.04,938.256,1.793,-1.913,62.51/
C GN=SQRT(ALFA*G2)*HBARC = SQRT(13.75/137)*197.32
C  ADD NUCLEON POLE TERMS TO OPEC EMS
      DATA GPI2M/13.75/
      SAVE
      IF(GPI2.EQ.GPI2M) GO TO 20
      IF(GPI2.EQ.0.0) GPI2=GPI2M
      GN=197.32*SQRT(GPI2/137.0)
      GPI2M=GPI2
   20 CONTINUE
      S=WN*(WN+2.0*EL)
      W=SQRT(S)
      ZK=EL/SQRT(1.0+2.0*EL/WN)
      Q=SQRT((S-(WN+WP)**2)*(S-(WN-WP)**2)/4.0/S)
      IF(Q.LT.0.0)  GO TO 99
      Z2=SQRT(Q**2+WN**2)
      ZU=-Z2/Q
      ZT=SQRT(Q**2+WP**2)/Q
      Z2=SQRT(Z2+WN)
      Z1=SQRT(SQRT(ZK**2+WN**2)+WN)
      GG=1000.0*GN/W/4.0/WN
      ZM=Z1*Z2
      ZD=Z1/Z2
      EM(1,2,1)=EM(1,2,1)+GG*ZM*(UP-UN)
      EM(2,1,2)=EM(2,1,2)+GG*(UP-UN)*Q*ZK/ZM
      UB=(2.0*UN+UP)/3.0
      EM(3,2,1)=EM(3,2,1)+GG*(2.0*ZM*WN/(W+WN)-UP*ZK/ZD+UB*ZM)
      EM(4,1,2)=EM(4,1,2)-GG*(2.0*Q*ZD*WN/(W+WN)+UP*Q*ZD-UB*Q*ZK/ZM)
      UB=(2.0*UP+UN)/3.0
      EM(5,2,1)=EM(5,2,1)+GG*(UB*ZM-UN*ZK/ZD)
      EM(6,1,2)=EM(6,1,2)+GG*(UB*Q*ZK/ZM-UN*Q*ZD)
   99 RETURN
      END
C ************************************************************
      SUBROUTINE PREPV(EL,EM)
C ADD EXTRA TERM FOR PV COUPLING
      COMMON/GOMEGA/GOM1,GOM2,GOMS,GPI2,GP1,GP2
      REAL MN,MPI,MUN,MUP,G
      DATA MN,MPI,PI,GPI2M/938.256,135.04,3.1415927,13.75/
      DATA MUP,MUN,CE,GN/1.793,-1.913,315.65,62.51/
C  G=SQRT(4*PI*G**2)   CE=1000*SQRT(ALFA*G**2)=1000*SQRT(13.65/137)
      DIMENSION EM(6,2,6)
      SAVE
      IF(GPI2.EQ.GPI2M) GO TO 20
      IF(GPI2.EQ.0.0) GPI2=GPI2M
      GN=197.32*SQRT(GPI2/137.0)
      GPI2M=GPI2
   20 CONTINUE
      CE=1000.0*GN
      CALL PRKIN(EL,1,EPIX,RK,RQ)
      E1=SQRT(RK**2+MN**2)
      WC=E1+RK
      E2=SQRT(RQ**2+MN**2)
      EPI=SQRT(RQ**2+MPI**2)
      Z1=SQRT(E1+MN)
      Z2=SQRT(E2+MN)
      EM(1,2,1)=EM(1,2,1)+CE*(MUP-MUN)*Z1*Z2*(WC-MN)/8.0/WC/MN**2
      EM(3,2,1)=EM(3,2,1)
     &          +CE*(2.0*MUP+MUN)*Z1*Z2*(WC-MN)/12.0/WC/MN**2
      EM(5,2,1)=EM(5,2,1)
     &          +CE*(MUP+2.0*MUN)*Z1*Z2*(WC-MN)/12.0/WC/MN**2
      EM(2,1,2)=EM(2,1,2)
     &           -CE*(MUP-MUN)*(WC-MN)*RQ*Z1/Z2/8.0/WC/MN**2
      EM(4,1,2)=EM(4,1,2)
     &           -CE*(2.0*MUP+MUN)*(WC-MN)*RQ*Z1/Z2/12.0/WC/MN**2
      EM(6,1,2)=EM(6,1,2)
     &          -CE*(MUP+2.0*MUN)*(WC-MN)*RQ*Z1/Z2/12.0/WC/MN**2
      RETURN
      END
C **********************************************************
      SUBROUTINE PROMEGA(EL,EM)
C*
C* PROGRAM TO CALCULATE T-CHANEL OMEGA CHANGE TERM
C* THE CORRESPONDING EXEC FILE IS GOOS
      IMPLICIT REAL (A-H,O-Z)
      COMMON/GOMEGA/GOM1,GOM2,GOMS,GPI2,GP1,GP2
      DIMENSION EPX(4),E2X(4),GC(4),ET(6,2,6),EM(6,2,6),F(12,4)
      REAL QL(12),MN,MPI,C,BB(4),ZL,ZZ,MW
      REAL EL,E1,E2,PI,LW,G1W,G2W,G1,G2,E
      REAL qtmp
      DATA PI/3.1415926/
      DATA MN,MPI,MW/938.256,135.04,782.6/
      DATA G1W,G2W/16.0,0.0/
      DATA B/0/
      SAVE
C     IF(GOMS.NE.27) GO TO 20
C     GOMS=28.0
      G1W=GOM1
      G2W=GOM2
   20 IF(G1W.EQ.0.0) GO TO 99
      LW=0.36
      LW=LW*SQRT(4.0*PI*1.0/137.0)
      G1=197.3*LW*G1W/0.1395
      G2=197.3*LW*G2W/0.1395
      CALL PRKIN(EL,1,EPIX,RK,RQ)
      E1=SQRT(RK**2+MN**2)
      WC=E1+RK
      C=(WC-MN)/(8.0*PI*WC)
      E2=SQRT(RQ**2+MN**2)
      EPI=SQRT(RQ**2+MPI**2)
      Z1=SQRT(E1+MN)
      Z2=SQRT(E2+MN)
      BW=(MW**2+2.0*RK*EPI-MPI**2)/(2.0*RK*RQ)
      C1=C*G1
      C2=C*G2
      BB(1)=C1*Z1*Z2/RK/RQ*(WC-MN-RK*EPI/(WC-MN)+RK*RQ*BW/(WC-MN))
      BB(2)=C1*Z1/Z2/RK*(WC+MN-RK*EPI/(WC+MN)+RK*RQ*BW/(WC+MN))
      BB(3)=-C1*Z1*Z2/RK
      BB(4)=-C1*Z1/Z2*RQ/RK
      BB(1)=BB(1)-C2*Z1*Z2*MW**2/(2.0*RQ*RK*MN)
      BB(2)=BB(2)+C2*(Z1/Z2)*MW**2/(2.0*RK*MN)
      BB(3)=BB(3)+C2*Z1*Z2*(WC-MN)/(RK*2.0*MN)
      BB(4)=BB(4)-C2*Z1/Z2*RQ*(WC+MN)/(2.0*MN*RK)
      CALL QJOFX(QL,BW,8)
      DO 1 M=1,6
      DO 1 J=1,2
      DO 1 L=1,6
    1 ET(M,J,L)=0.0
      M=0
      DO 2 I=1,3
      DO 3 K=1,4
      IF (I .EQ. 1) B=BB(K)
      IF (I .EQ. 2) B=BB(K)/3.0
      IF (I .EQ. 3) B=-BB(K)/3.0
      DO 3 L=1,8
    3 F(L,K)=B*QL(L)/2.0
      M=M+1
      L=2
    4 L=L+1
      IF (L .GT. 6) GO TO 5
      ZL=L-1
      Z=(ZL+1.0)/(2.0*ZL+1.0)
      ZZ=ZL/(2.0*ZL-1.0)
      E=F(L,1)-F(L-1,2)-Z*(F(L-1,3)-F(L+1,3))-ZZ*(F(L-2,4)-F(L,4))
      ET(M,1,L)=E/ZL
      EM(M,1,L)=EM(M,1,L)+ET(M,1,L)
      GO TO 4
    5 L=0
    6 L=L+1
      IF (L .GT. 6) GO TO 7
      ZL=L-1
      ZZ=(ZL+1.0)/(2.0*ZL+3.0)
      E=F(L,1)-F(L+1,2)+ZZ*(F(L,4)-F(L+2,4))
C       in order to bypass the boundary violation (L-1=0 !), 0 is used 
C             (I have no better idea) 12 nov 2007 E.Ch.
C
      qtmp=0.
      IF(L.GT.1) qtmp=F(L-1,3)
      EE=ZL/(2.0*ZL+1.0)*(qtmp-F(L+1,3))
      E=E+EE
      ET(M,2,L)=E/(ZL+1.0)
      EM(M,2,L)=EM(M,2,L)+ET(M,2,L)
      GO TO 6
    7 M=M+1
      L=1
    8 L=L+1
      IF (L .GT. 6) GO TO 9
      ZL=L-1
      ET(M,1,L)=(F(L-1,2)-F(L,1)+(F(L-1,3)-F(L+1,3))/(2.0*ZL+1.0))/ZL
      EM(M,1,L)=EM(M,1,L)+ET(M,1,L)
      GO TO 8
    9 L=1
   10 L=L+1
      IF (L .GT. 6) GO TO 2
      ZL=L-1
      E=F(L,1)-F(L+1,2)-(F(L-1,3)-F(L+1,3))/(2.0*ZL+1.0)
      ET(M,2,L)=E/(ZL+1.0)
      EM(M,2,L)=EM(M,2,L)+ET(M,2,L)
      GO TO 10
    2 CONTINUE
      EM(1,2,1)=EM(1,2,1)-0.5*C1*Z1*Z2/(WC-MN)
      EM(2,1,2)=EM(2,1,2)-0.5*C1*Z1/Z2*RQ/(WC+MN)
      EM(3,2,1)=EM(3,2,1)-0.5*C1*Z1*Z2/(WC-MN)/3.0
      EM(4,1,2)=EM(4,1,2)-0.5*C1*Z1/Z2*RQ/(WC+MN)/3.0
      EM(5,2,1)=EM(5,2,1)+0.5*C1*Z1*Z2/(WC-MN)/3.0
      EM(6,1,2)=EM(6,1,2)+0.5*C1*Z1/Z2*RQ/(WC+MN)/3.0
   99 RETURN
      END
C *****************************************
      SUBROUTINE PRRHO(EL,EM)
      IMPLICIT REAL (A-H,O-Z)
      COMMON/GOMEGA/GOM1,GOM2,GOMS,GPI2,GP1,GP2
      DIMENSION EPX(4),E2X(4),GC(4),ET(6,2,6),EM(6,2,6),F(12,4)
      REAL QL(12),MN,MPI,C,BB(4),ZL,ZZ,MP
      REAL EL,E1,E2,PI,LP,GP1,GP2,G1,G2,E
      DATA MN,MPI,MP/938.256,135.04,770.0/
      DATA B/0/
      SAVE
      IF(GP1.EQ.0.0) GO TO 99
      PI=4.0*ATAN(1.0)
      LP=0.12
      LP=LP*SQRT(4.0*PI*1.0/137.0)
      G1=197.3*LP*GP1/0.1395
      G2=197.3*LP*GP2/0.1395
      G2=G1*GP2
      CALL PRKIN(EL,1,EPIX,RK,RQ)
      E1=SQRT(RK**2+MN**2)
      WC=E1+RK
      C=(WC-MN)/(8.0*PI*WC)
      E2=SQRT(RQ**2+MN**2)
      EPI=SQRT(RQ**2+MPI**2)
      Z1=SQRT(E1+MN)
      Z2=SQRT(E2+MN)
      BW=(MP**2+2.0*RK*EPI-MPI**2)/(2.0*RK*RQ)
      C1=C*G1
      C2=C*G2
      BB(1)=C1*Z1*Z2/RK/RQ*(WC-MN-RK*EPI/(WC-MN)+RK*RQ*BW/(WC-MN))
      BB(2)=C1*Z1/Z2/RK*(WC+MN-RK*EPI/(WC+MN)+RK*RQ*BW/(WC+MN))
      BB(3)=-C1*Z1*Z2/RK
      BB(4)=-C1*Z1/Z2*RQ/RK
      BB(1)=BB(1)+C2*Z1*Z2*MP**2/(2.0*RQ*RK*MN)
      BB(2)=BB(2)-C2*(Z1/Z2)*MP**2/(2.0*RK*MN)
      BB(3)=BB(3)+C2*Z1*Z2*(WC-MN)/(RK*2.0*MN)
      BB(4)=BB(4)-C2*Z1/Z2*RQ*(WC+MN)/(2.0*MN*RK)
      CALL QJOFX(QL,BW,8)
      DO 1 M=1,6
      DO 1 J=1,2
      DO 1 L=1,6
    1 ET(M,J,L)=0.0
      M=0
      DO 2 I=1,3
      DO 3 K=1,4
      IF (I .EQ. 1) B=0.0
      IF (I .EQ. 2) B=BB(K)
      IF (I .EQ. 3) B=BB(K)
      DO 3 L=1,8
    3 F(L,K)=B*QL(L)/2.0
      M=M+1
      L=2
    4 L=L+1
      IF (L .GT. 6) GO TO 5
      ZL=L-1
      Z=(ZL+1.0)/(2.0*ZL+1.0)
      ZZ=ZL/(2.0*ZL-1.0)
      E=F(L,1)-F(L-1,2)-Z*(F(L-1,3)-F(L+1,3))-ZZ*(F(L-2,4)-F(L,4))
      ET(M,1,L)=E/ZL
      EM(M,1,L)=EM(M,1,L)+ET(M,1,L)
      GO TO 4
    5 L=0
    6 L=L+1
      IF (L .GT. 6) GO TO 7
      ZL=L-1
      ZZ=(ZL+1.0)/(2.0*ZL+3.0)
      E=F(L,1)-F(L+1,2)+ZZ*(F(L,4)-F(L+2,4))
      EE=ZL/(2.0*ZL+1.0)*(F(L-1,3)-F(L+1,3))
      E=E+EE
      ET(M,2,L)=E/(ZL+1.0)
      EM(M,2,L)=EM(M,2,L)+ET(M,2,L)
      GO TO 6
    7 M=M+1
      L=1
    8 L=L+1
      IF (L .GT. 6) GO TO 9
      ZL=L-1
      ET(M,1,L)=(F(L-1,2)-F(L,1)+(F(L-1,3)-F(L+1,3))/(2.0*ZL+1.0))/ZL
      EM(M,1,L)=EM(M,1,L)+ET(M,1,L)
      GO TO 8
    9 L=1
   10 L=L+1
      IF (L .GT. 6) GO TO 2
      ZL=L-1
      E=F(L,1)-F(L+1,2)-(F(L-1,3)-F(L+1,3))/(2.0*ZL+1.0)
      ET(M,2,L)=E/(ZL+1.0)
      EM(M,2,L)=EM(M,2,L)+ET(M,2,L)
      GO TO 10
    2 CONTINUE
      EM(3,2,1)=EM(3,2,1)-0.5*C1*Z1*Z2/(WC-MN)-0.5*C2*Z1*Z2/MN
      EM(4,1,2)=EM(4,1,2)-0.5*C1*Z1/Z2*RQ/(WC+MN)+0.5*C2*Z1/Z2*RQ/MN
      EM(5,2,1)=EM(5,2,1)-0.5*C1*Z1*Z2/(WC-MN)-0.5*C2*Z1*Z2/MN
      EM(6,1,2)=EM(6,1,2)-0.5*C1*Z1/Z2*RQ/(WC+MN)+0.5*C2*Z1/Z2*RQ/MN
   99 RETURN
      END
C ******************************************************
      SUBROUTINE QJOFX(QS,Y,LMAX)
C  VERWEST ALGORITHMS, MOD 6/86 FOR LARGE X ARNDT
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      REAL QS,Y,CN,ZN,ZL
      DIMENSION QL(12),QS(12)
       X=Y
        IWRIT=6
       IF(LMAX.LT.2) LMAX=1
      LL=LMAX+1
      DO 1 L=1,LL
    1 QS(L)=0.
      IF(X.LT.2.) GO TO 5
C POWER SERIES IN 1/X ARNDT 6/17/86
      Z=1./X
      ZZ=Z
      ZN=-1.
   20 ZN=ZN+2.
      CN=ZZ/ZN
      DO 21 L=1,LL
      ZL=L
      QS(L)=QS(L)+CN
      CN=CN*Z*(ZN+ZL-1.)/(ZN+2.*ZL)
      IF(CN.LT.1.E-30) GO TO 22
   21 CONTINUE
   22 ZZ=ZZ*Z**2
      IF(ZZ.GT.1.E-30) GO TO 20
      GO TO 2
    5 L=LMAX
       DO 100 II=1,2
       IF (X.LT.1.030) GO TO 600
C   ** ENTERING LARGE X EXPANSION
       Z=1./(X+DSQRT(X*X-1.))
         ALF=2.*Z
         DO 3 I=1,L
   3     ALF=DBLE(I)/DBLE(2*I+1)*ALF*2.*Z
        CTOT=1.
        CNOW=1.
         DO 4 I=1,100
         CNOW=DBLE((2*I-1)*(I+L))/DBLE(2*I*I+2*I*L+I)*CNOW*Z*Z
         CTOT=CTOT+CNOW
         IF (CNOW/CTOT.LT.1.E-7  ) GO TO 99
   4     CONTINUE
         WRITE(IWRIT,333)
   99    QLOFX=ALF*CTOT
         GO TO 601
C   ** ENTERING SMALL X EXPANSION
  600    Z=1.-1./X/X
         SUM=0.
         CNOW=1.
         FNOW=DLOG(4.D0)-DLOG(Z)
         IF (L.EQ.0) GO TO 299
         DO 18 JJ=1,L
   18    FNOW=FNOW-2./DBLE(JJ)
 299     DO 48 I=01,100
         SUM=SUM+CNOW*FNOW
        IF (DABS(CNOW*FNOW/SUM ).LT.1.E-7  ) GO TO 199
         CNOW=CNOW*DBLE((L+2*I)*(L+2*I-1))/4./DBLE(I*I)*Z
 48      FNOW=FNOW+2.*(1./DBLE(I)-1./DBLE(L+2*I-1)-1./DBLE(2*I+L))
  199    QLOFX=.5/(X**(L+1))*SUM
  601    QL(II)=QLOFX
  100    L=L-1
        QJ1=QL(2)
        QJ2=QL(1)
        QS(LMAX+1)=QJ2
        QS(LMAX)=QJ1
        DO 999 IOP=2,LMAX
        J=LMAX-IOP+1
      QJ=(DBLE(2*J+1)*QJ1*X-DBLE(J+1)*QJ2)/DBLE(J)
       QS(J)=QJ
       QJ2=QJ1
 999   QJ1=QJ
  333  FORMAT (' WARNING **** QLS MAY NOT BE RIGHT *** SUM ENDED')
    2   RETURN
          END
C *********************************************************
      SUBROUTINE CMFN(WR,WI,WZ,WTR,WTI,LMX,CR,CI)
C  CHEW-MANDELSTAM FUNCTIONS 7/17/80 ARNDT
C  INT(0,1) OF X**(L+1/2)/PI/(X-Z)
C  Z=(W-WT)/(W-WZ)
      DIMENSION CR(20),CI(20)
      DATA PI/3.1415927/
      DATA ZC/2./
      SAVE
      IF(LMX.GT.10) LMX=10
      DO 10 L=1,LMX
      CR(L)=0.
   10 CI(L)=0.
      DR=WR-ABS(WZ)
      D2=DR**2+WI**2
      IF(D2.LT.1.) GO TO 99
      ZR=((WR-WTR)*DR+WI*(WI-WTI))/D2
      ZI=(DR*(WI-WTI)-WI*(WR-WTR))/D2
      AR=ZR
      AI=ZI
      CALL SQZ(AR,AI)
      IF(WZ.LT.0..AND.ZI.LT.0.) AR=-AR
      IF(WZ.LT.0..AND.ZI.LT.0.) AI=-AI
      Z2=ZR**2+ZI**2
      IF(Z2.LT.ZC**2) GO TO 11
C  USE POWER SERIES FOR Z GTO INF
      RR=ZR/Z2
      RI=-ZI/Z2
      L=0
   12 TL=2*L
      TL=(TL+3.)/2.
      SR=-RR/PI/TL
      SI=-RI/PI/TL
      TR=SR
      TI=SI
      RT=SQRT(TR**2+TI**2)
      DO 13 N=1,20
      R=TL/(TL+1.)
      Z=TR
      TR=R*(RR*Z-RI*TI)
      TI=R*(RI*Z+RR*TI)
      SR=SR+TR
      SI=SI+TI
      IF(R*RT.LT.1.E-6) GO TO 14
   13 TL=TL+1.
   14 L=L+1
      CR(L)=SR
      CI(L)=SI
      IF(L.GE.LMX) GO TO 99
      GO TO 12
   11 A2=AR**2+AI**2
      D=1.+A2+2.*AR
      ZZR=(1.-A2)/D
      ZZI=-2.*AI/D
      CALL ALG(ZZR,ZZI)
      BR=2./PI-AI+(AR*ZZR-AI*ZZI)/PI
      BI=AR+(AR*ZZI+AI*ZZR)/PI
      ZL=.5
      L=0
    1 L=L+1
      CR(L)=BR
      CI(L)=BI
      IF(L.GE.LMX) GO TO 99
      ZL=ZL+1.
      ZZ=BR
      BR=ZR*BR-ZI*BI+1./PI/ZL
      BI=ZI*ZZ+ZR*BI
      GO TO 1
   99 RETURN
      END
C **********************************************************
      SUBROUTINE ALG(ZR,ZI)
C  TAKE NATURAL LOG OF Z  BRANCH CUT AT Z=0 TAKEN TO LEFT
      DATA PI/3.1415927/
      SAVE
      IF(ZR.EQ.0.) ZR=1.E-10
      ZM=ZR**2+ZI**2
      PHI=ATAN(ZI/ZR)
      IF(ZR.GT.0.) GO TO 1
      IF(ZI.GE.0.) PHI=PHI+PI
      IF(ZI.LT.0.) PHI=PHI-PI
    1 ZR=ALOG(ZM)/2.
      ZI=PHI
      RETURN
      END
C **************************************************************
      SUBROUTINE SQZ(ZR,ZI)
C  SQRT(Z)  BRANCH CUT TAKEN TO LEFT OF Z=0
      DATA PI/3.1415927/
      SAVE
      ZM=SQRT(SQRT(ZR**2+ZI**2))
      IF(ZR.EQ.0.) ZR=1.E-10
      PHI=ATAN(ZI/ZR)
      IF(ZR.GT.0.) GO TO 1
      IF(ZI.LT.0.) PHI=PHI-PI
      IF(ZI.GT.0.) PHI=PHI+PI
    1 PHI=PHI/2.
      ZR=ZM*COS(PHI)
      ZI=ZM*SIN(PHI)
      RETURN
      END
C ***************************************************************
      FUNCTION OBSPRD(IT)
      COMMON/AMPLS/HRX(4),HIX(4),QCM,ZKCM,CS,EG
C Follows Knochlein, Dreschel, Tiator, Z.Phys.A352(1995) 327-343
C     OBSERV FOR PI-N PHOTOPRODUCTION HRX, HIX ARE AMPLITUDES IN UNITS
C     of milli-Fermis
C     IT=OBSERVABLE TYPE= 1(DSG), 2(P), 3(S), 4(T), 5(SGT) 6(G), 7(H)
C     IT=8(EMRI), 9(E), 10(F), 11(OX), 12(OZ), 13(CX), 14(CZ), 15(TX)
C     IT=16(TZ), 17(LX), 18(LZ), 19(ST3), 20(ST1), 21(ST31)
C     IT=22(DX1), 23(DX3), 24(DX13)
C     IT=25-32 Ox,Oz,Cx,Cz,Tx,Tz,Lx,Lz as measured
C in a RH lab system (z(u) along N and x(u) along y cross z)
C ROTATIONS to lab frame are from Yerevan group(Ox, Oz)
C COS(THR)=C*CN-G*S*SN     ; C=cos(th(pi,cm)), CN=cos(th(N,lab))
C G=(Eg+M)/W Lorentz factor from cm->lab
C CN=G(ALF-C)/SQRT(G**2*(ALF-C)**2+S**2)   ALF=B*SQRT(1+M**2/Q**2)
      DIMENSION H2(4)
      data wpr,wpi/938.256,135.04/
      SAVE
      IF(IT.GT.10.AND.IT.LT.19) OBSPRD=PROBSL(IT)
      IF(IT.GT.24) OBSPRD=PROBSL(IT)
      IF(IT.GT.10.AND.IT.LT.19) GO TO 99
      IF(IT.GT.24) GO TO 99
      QK=QCM/ZKCM
      OBSPRD=0.0
      IF(IT.EQ.8) GO TO 99
      IF(IT.EQ.5.AND.THTA.LE.0.0) GO TO 99
      IF(IT.GT.18.AND.IT.LT.22) GO TO 99
      DSG=0.0
      DO 1 K=1,4
      H2(K)=HRX(K)**2+HIX(K)**2
    1 DSG=DSG+H2(K)
      IF(IT.EQ.5) GO TO 3
      IF(IT.EQ.22) DSG=H2(2)+H2(4)
      IF(IT.EQ.23) DSG=H2(1)+H2(3)
      IF(IT.EQ.24) DSG=H2(2)+H2(4)-H2(1)-H2(3)
      IF(IT.GT.21) DSG=2.0*DSG
      OBSPRD=QK*DSG/200.0
      IF(IT.GT.21) GO TO 99
   30 GO TO (99,2,3,4,99,6,7,99,31,32),IT
    2 X=HIX(3)*HRX(1)-HIX(1)*HRX(3)+HIX(4)*HRX(2)-HIX(2)*HRX(4)
      GO TO 8
    3 X=HRX(1)*HRX(4)+HIX(1)*HIX(4)-HRX(2)*HRX(3)-HIX(2)*HIX(3)
      GO TO 8
    4 X=HRX(2)*HIX(1)-HRX(1)*HIX(2)-HRX(3)*HIX(4)+HIX(3)*HRX(4)
      GO TO 8
    6 X=HIX(4)*HRX(1)-HRX(4)*HIX(1)+HRX(2)*HIX(3)-HRX(3)*HIX(2)
      GO TO 8
    7 X=HIX(3)*HRX(1)-HRX(3)*HIX(1)+HIX(2)*HRX(4)-HRX(2)*HIX(4)
      GO TO 8
   31 X=(H2(4)-H2(1)-H2(3)+H2(2))/2.0
      GO TO 8
   32 X=HRX(4)*HRX(3)+HIX(4)*HIX(3)+HRX(1)*HRX(2)+HIX(1)*HIX(2)
    8 OBSPRD=X/DSG*2.0
      IF(IT.EQ.5) OBSPRD=(1.0-OBSPRD)/(1.0+OBSPRD)
   99 RETURN
      END
C *************************************
      FUNCTION PROBSL(IT)
      COMMON/AMPLS/HRX(4),HIX(4),QCM,ZKCM,CS,EG
C Follows Knochlein, Dreschel, Tiator, Z.Phys.A352(1995) 327-343
C     OBSERV FOR PI-N PHOTOPRODUCTION HRX, HIX ARE AMPLITUDES IN UNITS
C     of milli-Fermis
C     IT=OBSERVABLE TYPE= 1(DSG), 2(P), 3(S), 4(T), 5(SGT) 6(G), 7(H)
C     IT=8(EMRI), 9(E), 10(F), 11(OX), 12(OZ), 13(CX), 14(CZ), 15(TX)
C     IT=16(TZ), 17(LX), 18(LZ), 19(ST3), 20(ST1), 21(ST31)
C     IT=22(DX1), 23(DX3), 24(DX13)
C     IT=25-32 Ox,Oz,Cx,Cz,Tx,Tz,Lx,Lz as measured
C in a RH lab system (z(u) along N and x(u) along y cross z)
C ROTATIONS to lab frame are from Yerevan group(Ox, Oz)
C COS(THR)=C*CN-G*S*SN     ; C=cos(th(pi,cm)), CN=cos(th(N,lab))
C G=(Eg+M)/W Lorentz factor from cm->lab
C CN=G(ALF-C)/SQRT(G**2*(ALF-C)**2+S**2)   ALF=B*SQRT(1+M**2/Q**2)
      DIMENSION H2(4),GTH(10)
      SAVE
      PROBSL=0.0
      DSG=0.0
      DO 1 K=1,4
      H2(K)=HRX(K)**2+HIX(K)**2
    1 DSG=DSG+H2(K)
      IF(DSG.LE.0.0) GO TO 99
      ITT=IT-10
      IF(ITT.GT.10) ITT=ITT-14
      IUV=ITT/2
      IUV=ITT-2*IUV
C IUV=0(Cz), 1(Cx) ....
      GO TO  (33,33,35,35,37,37,39,39) ITT
   33 V=HRX(4)*HIX(3)-HIX(4)*HRX(3)+HRX(1)*HIX(2)-HIX(1)*HRX(2)
C  Ox, Oz go here
      U=HIX(1)*HRX(4)-HRX(1)*HIX(4)+HIX(3)*HRX(2)-HRX(3)*HIX(2)
      GO TO 7
   35 V=-HRX(4)*HRX(2)-HIX(4)*HIX(2)-HRX(1)*HRX(3)-HIX(1)*HIX(3)
C  Cx, Cz go  here
      U=(H2(4)-H2(1)-H2(2)+H2(3))/2.0
      GO TO 7
   37 V=HRX(1)*HRX(4)+HIX(1)*HIX(4)+HRX(2)*HRX(3)+HIX(2)*HIX(3)
C  Tx, Tz go  here
      U=HRX(1)*HRX(2)+HIX(1)*HIX(2)-HRX(4)*HRX(3)-HIX(4)*HIX(3)
      GO TO 7
   39 V=HRX(4)*HRX(2)+HIX(4)*HIX(2)-HRX(1)*HRX(3)-HIX(1)*HIX(3)
C  Lx, Lz go here
      U=(H2(1)+H2(4)-H2(2)-H2(3))/2.0
    7 CONTINUE
      UP=U
      VP=V
      IF(IT.LT.20) GO TO 8 
C multiply by signs in table II of Knochlein...
C change signs of Cx, Cz
      IF(ITT.EQ.3.OR.ITT.EQ.4) V=-V  
      IF(ITT.EQ.3.OR.ITT.EQ.4) U=-U  
c     nflag=pem(14,6,2,6)+0.1
c     ninv=nflag/10
c     nfg=nflag-10*ninv
c nflag=0(do cm quantities), 1(U,V), 2(U,-V), 3(-U,V), 4(-U,-V)
c nflag=5(UP=RUU, VP=RUV) 
c nflag=10*ninv+nflag. ninv=0(R,A xform), 1(identity), 2(oz, ox)
c     if(nfg.gt.2) U=-U
c     if(nfg.eq.2.or.nfg.eq.4) v=-v
c     nv=ninv
      NV=0
c     nv=pem(19,6,2,6)
      THTA=57.296*ACOS(CS)
      CALL XFORM(EG,THTA,NV,U,V,UP,VP,D)
    8 X=UP
      IF(IUV.EQ.1) X=VP
      PROBSL=X/DSG*2.0
   99 RETURN
      END
C *************************************
      SUBROUTINE XFORM(EG,THT,NFRM,U,V,UP,VP,D)
C transform cm variables U,V (eg Cz,Cx) into UP,VP
      dimension s4(3)
      DATA WPR,WPI,EGM/938.256,135.04,0.0/
      SAVE
      UP=U
      VP=V
C No Xformation if NFRM = 1
c     IF(NFRM.EQ.1) GO TO 99
C get kinematic factors
      IF(EG.EQ.EGM) GO TO 1
      S=WPR**2+2.0*WPR*EG
      Q2=(S-(WPR+WPI)**2)*(S-(WPR-WPI)**2)/4.0/S
      B=EG/(EG+WPR)
      BN=SQRT(Q2/(Q2+WPR**2))
      G=1.0/SQRT(1.0-B**2)
      GN=1.0/SQRT(1.0-BN**2)
      ALF=B/BN
    1 EGM=EG
c get angle factors C,S=COS(THT),SIN(THT)  CN,SN=COS(thN),..
c THT=PION cm angle, thN=NUCLEON lab angle
      C=COS(0.0174532*THT)
      S=SQRT(1.0-C**2)
      CN=G*(ALF-C)
      Z=CN**2+S**2
      CN=CN/SQRT(Z)
      SN=SQRT(ABS(1.0-CN**2))
C Yerevan(Ox, Oz) Xformation. This is a simple rotation so RVV
C is just COS(ROT) (they use a RH lab system)  RAA 2/18/02
C Consistent with Gilman if one starts with (-Cx, -Cz) in cm system
C as prescribed by Knochlein.., Photo and Electroproduction of eta
C Mesons Z.Phys(1992)
      RVV=C*CN-G*S*SN
C     P2=Q2*Z
C     BL=SQRT(P2/(P2+WPR**2))
C     GL=1.0/SQRT(1.0-BL**2)
C     Z=G*GN*(B*BN-C)
C     RVV=C*CN-G*S*SN
C     RVU=GN*S*CN-SN*Z
C     ZKX=C*SN+G*S*CN
C     ZKZ=-GN*S*SN-CN*Z
C     ZKX0=G*B*S
C     ZKZ0=G*GN*(B*C-BN)
C     RUU=GL*(ZKZ-BL*ZKZ0)
C     RUV=-GL*(ZKX-BL*ZKX0)
C This is a simple rotation so RUU=cos, Rvu=sin, Ruv=-sin, Ruu=Rvv
      RUU=RVV
      RVU=SQRT(ABS(1.0-RUU**2))
      if(nfrm.eq.1) rvu=-rvu
      RUV=-RVU
c     if(nfrm.ne.3) go to 3
c     rvv=-rvv
c     rvu=-rvu
C use Gilman rotation (=180-th(Yerevan))
c     ruu=-ruu
c     rvv=ruu
c     rvu=sqrt(1.0-ruu**2)
c     if(nfrm.eq.5) rvu=-rvu
c     ruv=-ruv
    3 UP=RUU*U+RUV*V
      VP=RVV*V+RVU*U 
      D=RUU*RVV-RUV*RVU
   99 RETURN
      END
