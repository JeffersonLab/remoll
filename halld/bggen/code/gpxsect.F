      REAL FUNCTION GPXSECT(E)
C
C---     This function describes the total gamma+p cross section at 0.18-100 GeV
C
      IMPLICIT NONE
      REAL E
C
C      COMMON/PAWPAR/ PARA(20)
C      REAL PARA
C      VECTOR PAR(20)
C      VECTOR IPFIT(10)
      REAL PAR(20),PARA(20)
      INTEGER IPFIT(10)
C
      INTEGER    mxgaus,ig,i,k
      PARAMETER (mxgaus=3)
      REAL pgaus(3,mxgaus)
      REAL f1,f2,f3,e0
      REAL parf1(2),parf2(2)
      DATA pgaus/0.43,0.32,0.055
     +          ,0.13,0.73,0.130
     +          ,0.08,1.08,0.080/
      DATA parf1/0.119,21.3/
      DATA parf2/0.114,1.04/
C
      IPFIT(1)=0
      IF(IPFIT(1).EQ.2) THEN
         DO i=1,13
            PARA(i)=PAR(i)
         ENDDO
      ENDIF
      IF(IPFIT(1).NE.0) THEN
         k=0
         DO i=1,2
            k=k+1
            parf1(i)=PARA(k)
         ENDDO
         DO i=1,2
            k=k+1
            parf2(i)=PARA(k)
         ENDDO
         DO ig=1,mxgaus
            DO i=1,3
               k=k+1
               pgaus(i,ig)=PARA(k)
            ENDDO
         ENDDO
      ENDIF
C
      e0=0.15
C
      f3=0
      DO ig=1,mxgaus
         f3=f3+pgaus(1,ig)*exp(-(E-pgaus(2,ig))**2/pgaus(3,ig)**2/2.)
      ENDDO
C
      f1=parf1(1)*ATAN((E-e0)*parf1(2))/3.14*2
      f2=parf2(1)*(E-e0)*EXP(-E*parf2(2))
C
      GPXSECT=f1+f2+f3
      IF(GPXSECT.LT.0.) GPXSECT=0.
C
      END

