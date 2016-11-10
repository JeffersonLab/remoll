      REAL FUNCTION GPXSECP(E,IPROC)
C
C---     This function describes the partial gamma+p cross section, say gamma p --> p pi+ pi-
C---      process: 1,2  - SAID (called from elsewhere)
C                  3    - p pi+ pi-  no resonances 
C                  4    - p rho0
C                  5    - Delta++ pi-
C                  6    - p pi0 pi0
C                  7    - n pi+ pi0
C                  8    - p eta 
C                  9    - p pi+ pi- pi0
C                 10    - n 2pi+ pi-
C                 11    - p pi+ pi-  full 
C
      IMPLICIT NONE
      REAL E
      INTEGER IPROC
C
      COMMON/PAWPAR/ PARA(20)
      REAL PARA
C      VECTOR PAR(20)
C      VECTOR IPFIT(10)
C
      REAL GP_F1,GP_F2,GP_F3,GP_F4
C
      INTEGER    ig,i,k,npar,mxp,ipro,ifit
      PARAMETER (mxp=11)
      REAL f1,f2,e0,ered,ff
      REAL parf1(4,mxp)
      DATA parf1/4*0.
     +          ,4*0.
     +          ,4*0.
     +          ,0.8199,0.0268,4.405 ,62.52
     +          ,0.0914,3.5619,0.4100,2.1426
     +          ,0.3611,0.0250,18.74 ,15.333
     +          ,0.4545,0.0597,18.74 ,15.333
     +          ,0.4782,6.8940,0.0794,2.038
     +          ,0.0529,0.994 ,23.72 ,0.0
     +          ,0.0050,0.4652,1525. ,0.0
     +          ,0.3768,0.0693,18.74 ,15.333
     +          /
C
      ifit=0
      npar=2
      ipro=IPROC
C      ifit=IPFIT(1)
C      npar=IPFIT(2)
C      ipro=IPFIT(3)
C      write(6,*) ifit,npar,ipro
C
      IF(ifit.EQ.2) THEN
         DO i=1,npar
C            PARA(i)=PAR(i)
         ENDDO
      ENDIF
      IF(ifit.NE.0) THEN
         k=0
         DO i=1,4
            k=k+1
C            IF(k.LE.npar) parf1(i,ipro)=PARA(k)
         ENDDO
      ENDIF
C
      IF(ipro.LE.2) THEN  ! SAID
         GPXSECP=0.
      ELSEIF(ipro.EQ.3) THEN  ! subtraction 11-4-5
         ff=   GP_F1(parf1(1,11),E)
         ff=ff-GP_F1(parf1(1,4) ,E)
         ff=ff-GP_F4(parf1(1,5) ,E)
         GPXSECP=ff
      ELSE IF(ipro.EQ.5) THEN
C
         GPXSECP=GP_F4(parf1(1,ipro),E)
C
      ELSE IF(ipro.LE.7.OR.ipro.EQ.11) THEN
C
         GPXSECP=GP_F1(parf1(1,ipro),E)
C
      ELSE IF(ipro.EQ.8) THEN
C
         GPXSECP=GP_F2(parf1(1,ipro),E)
C
      ELSE IF(ipro.LE.11) THEN
C
         GPXSECP=GP_F3(parf1(1,ipro),E)
C
      ENDIF
C
      IF(GPXSECP.LE.0.) GPXSECP=1.E-9
C
      END
C
      REAL FUNCTION GP_F1(P,E)
      IMPLICIT NONE
      REAL P(*),E
      REAL e0,ered,f1,f2
C
      e0=P(1)
      ered=E-e0
      IF(ered.LT.0.) ered=0.
      f1=P(2)**2*ATAN(ered**2*P(3))/3.14*2.
      f2=1.+P(4)/E
      GP_F1=f1*f2
      END
C
      REAL FUNCTION GP_F2(P,E)
      IMPLICIT NONE
      REAL P(*),E
      REAL e0,ered,f1,f2
C
      e0=0.68
      ered=E-e0
      IF(ered.LT.0.) ered=0.
      f1=P(1)**2*EXP(-(E-0.6)**2/2*P(2)**2)
      f2=P(3)**2*EXP(-(E-1.1)*P(4))
      GP_F2=(f1+f2)*ered
      END
C
      REAL FUNCTION GP_F3(P,E)
      IMPLICIT NONE
      REAL P(*),E
      REAL e0,ered,f1,f2
C
      e0=0.55
      ered=E-e0
      IF(ered.LT.0.) ered=0.
      f1=P(1)**2*ATAN(ered**2*P(2))/3.14*2.
      f2=1.+P(3)/E
      GP_F3=f1*f2
      END
C
      REAL FUNCTION GP_F4(P,E)
      IMPLICIT NONE
      REAL P(*),E
      REAL e0,ered,f1,f2
C
      e0=0.4
      ered=E-e0
      IF(ered.LT.0.) ered=0.
      f1=P(1)**2*EXP(1.-((E-0.8)*P(2))**6)*ATAN(ered**2*100)
      f2=P(3)**2*EXP(-ered*P(4))*ered
      GP_F4=f1+f2
      END

