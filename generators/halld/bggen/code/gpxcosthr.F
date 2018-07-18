      SUBROUTINE GPXCOSTHR(IPROC,E0,TMN,TMX,COSTH,IERR)
C
C===           Generates a random value for COS(TH) in CM,
C                using various functions:
C                polinomial distrubutions:
C                cos(th)=X=a(0)+a(1)*X+a(2)*X**2, where
C                a(i)=b(0,i)+b(1,i)*E0+b(2,i)*E0**2+b(3,i)*E0**3
C                the factors b are stored
C
C---     Input: E0     - energy
C               IPROC  - process
C               TMN,TMX (-t to the target) for the -t-dependence simulation
C       Output: COSTH  - COS(th) in CM - random value, for the 1-st secondary particle (should be the baryon)
C               IERR >0 - error (not defined)
C
      IMPLICIT NONE
C
      INTEGER IPROC,IERR
      REAL E0,COSTH,TMN,TMX
C
      REAL RNDM
      DOUBLE PRECISION DPOLFMY,DINT_F2
      EXTERNAL         DPOLFMY,DINT_F2
C
      COMMON/CFUN_COS/ DFPA(10)
      DOUBLE PRECISION DFPA
C
      INTEGER i,j,npol,maxf
     +       ,iset  ! array index for the polynomial coefficients for this process
     +       ,npar  ! the number of parameters in the final distribution function
     +       ,ivar  ! distribution used, =1 - cos(th), =2 - -t
     +       ,ifun  ! function used used, =1 - polynomial, =2 - exp(a+b*x)+c converted to a*exp(bx)+c
      REAL    rnd,xx,tt,qq
     +       ,csign ! +1 or -1 multiplication factor for COSTH, depending on the data used -> baryon
      DOUBLE PRECISION da(3),de0,dx,dx0,dxlim(2),dd
     +                ,df,dfe,df1,df2,dmin,dnorm
     +                ,dq(4),dtmp(4),dr(2),dl(2),dv,dres,dintg
C
      COMPLEX*16 dz(3)
C
      INTEGER mxpro,mxpar,mxfun
      PARAMETER (mxpro=3,mxpar=3,mxfun=2)
      REAL bb(4,mxpar,mxpro)  ! polynomial coefficients for cos,t functions coefficients 
     +    ,elim(2,mxpro)      ! energy limits for these polynomilas. beyond them the edge value is taken 
      INTEGER nparf(mxfun)
C
      DATA bb/
     +        10.22637,  -4.23276,   0.81462,   0.       ! p rho: (proc=4) 
     +      ,-39.04430,  29.35012,  -6.47698,   0.       ! 
     +       ,46.54426, -38.59182,   8.25751,   0.       ! 
     +       , 6.24956,  -1.58878,   0.08898,   0.       ! Delta++ pi- (5)
     +       ,16.14728, -24.19744,   6.55671,   0.       ! 
     +       , 7.32414,  -1.37016,  -0.63512,   0.       ! 
     +       ,-0.68852,   2.80721,  -1.90364,   0.36498  ! p eta (proc=8)
     +       ,-1.09740,   1.69941,  -0.67707,   0.11149  
     +       , 3.53044,  -8.35135,   5.66832,  -1.08696
     +       /
      DATA elim/
     +          1.5,   2.5
     +         ,1.25,  2.4
     +         ,0.75,  3.0
     +         /
      DATA nparf/3,3/
C
C     ------------------------------------------------------------------
C
      IERR=1
      COSTH=0.
      IF(IPROC.LT.3.OR.IPROC.GT.12) GO TO 999
      IERR=2
C
      iset=0
      IF(IPROC.EQ.4) THEN  ! p rho 
         iset=1
         npol=3
         ivar=2
         ifun=2
         csign=1.
      ELSE IF(IPROC.EQ.5) THEN ! Delta++ pi-
         iset=2
         npol=3
         ivar=2
         ifun=2
         csign=1.
      ELSE IF(IPROC.EQ.8) THEN ! p eta
         iset=3
         npol=4
         ivar=1
         ifun=1
         csign=-1.
      ENDIF
C
      IF(iset.EQ.0) GO TO 999
      IERR=3
      npar=nparf(ifun)
C
C---     Calculate the polynomial coefficients for the given energy
C
      de0=DBLE(E0)
      IF(E0.LT.elim(1,iset)) de0=elim(1,iset)
      IF(E0.GT.elim(2,iset)) de0=elim(2,iset)
      DO i=1,npar
         DO j=1,npol
            dtmp(j)=DBLE(bb(j,i,iset))
         ENDDO
C         write(6,*) 'dtmp=',(dtmp(i),i=1,npol)
         da(i)=DPOLFMY(npol,dtmp(1),de0)
      ENDDO
C
      IF(ifun.EQ.2) THEN
C
C--- convert to a*exp(bx)+c
C
         da(1)=EXP(da(1))
      ENDIF
C      write(6,*) 'da=',(da(i),i=1,npar)
C
C---    Limits of the variable
C
      IF(ivar.EQ.1) THEN  ! cos(th)
         dxlim(1)=-1.D0
         dxlim(2)= 1.D0
      ELSE IF(ivar.EQ.2) THEN  ! -t
         dxlim(1)=TMN
         dxlim(2)=TMX
      ENDIF
C
C---   For the polynomial function (ifun=1) sure that the function is positive in the full range of the variable
C
C      write(6,*) 'ifun,ivar,npol,npar=',ifun,ivar,npol,npar
      IF(ifun.EQ.1) THEN      ! p2 is assumed...
         IF(da(3).LT.0.D0.AND.
     +      da(2)**2-4.D0*da(1)*da(3).LT.0.D0) GO TO 999 ! all the curve is negative
         IERR=4
         dfe=1.D0
         IF(ABS(da(3)).GT.1.D-10) THEN ! there is an extremum
            dx0=-da(1)/2.D0/da(3)
            IF(dx0.GT.dxlim(1).AND.dx0.LT.dxlim(2)) THEN ! extremum is inside the interval
               dfe=DPOLFMY(3,da(1),dx0)
            ENDIF
         ENDIF
         df1=DPOLFMY(3,da(1),dxlim(1))
         df2=DPOLFMY(3,da(1),dxlim(2))
         dmin=MIN(dfe,df1,df2)
         IF(dmin.LT.0.D0) THEN  ! if needed, add a constant to the function in order to make it positive
            da(1)=da(1)-dmin+1.D-15
         ENDIF
      ENDIF
C
C---    Normalize the function
C
      dd=dxlim(2)-dxlim(1)
C      write(6,*) 'da=',(da(i),i=1,npar),dd
      IF(ifun.EQ.1) THEN
         dnorm=da(1)*dd+da(2)/2.D0*(dxlim(2)**2-dxlim(1)**2)
     +                 +da(3)/3.D0*(dxlim(2)**3-dxlim(1)**3)
         DO i=1,npar
            da(i)=da(i)/dnorm
         ENDDO
      ELSE IF(ifun.EQ.2) THEN
         dnorm=da(3)*dd
         IF(ABS(da(2)).GT.1.D-8) THEN
            dnorm=dnorm+
     +       da(1)/da(2)*(EXP(da(2)*dxlim(2))-EXP(da(2)*dxlim(1)))
         ELSE
            dnorm=dnorm+da(1)*dd
         ENDIF
         da(1)=da(1)/dnorm
         da(3)=da(3)/dnorm
      ENDIF
C      write(6,*) 'da=',(da(i),i=1,npar),dnorm
C
C---   Calculate the integral function crossing with rnd
C
      rnd=RNDM(rnd)
C
      IF(ifun.EQ.1) THEN
C
C---    Integral
C
         dtmp(1)=0.D0
         dtmp(2)=da(1)
         dtmp(3)=da(2)/2.
         dtmp(4)=da(3)/3.
         df=DPOLFMY(4,dtmp(1),dxlim(1))
         dtmp(1)=-df            ! the integral function is 0 at the left edge
C
C---    The integral function factors are in reverse order to match the cernlib routine
C
         DO i=1,4
            dq(i)=dtmp(5-i)
         ENDDO

C
         dq(4)=dq(4)-DBLE(rnd)
C
         CALL DMULLZ(dq(1),3,1000,dz)
C
         DO i=1,3
            dr(1)=DBLE(dz(i))
            dr(2)=DIMAG(dz(i))
            IF(ABS(dr(2)).LT.1.D-10) THEN
               IF(dr(1).GE.dxlim(1).AND.dr(1).LE.dxlim(2)) THEN
                  IERR=0
                  xx=REAL(dr(1)) ! solution found
               ENDIF
            ENDIF
         ENDDO
      ELSE IF(ifun.EQ.2) THEN
C
C---     Integral function
C
         dl(1)=dxlim(1)
         dl(2)=dxlim(2)
         IF(ABS(da(2)).LT.1E-8) THEN
            xx=dl(1)+DBLE(rnd)*(dl(2)-dl(1))
            IERR=0
         ELSE
            DFPA(1)=da(1)/da(2)
            DFPA(2)=da(2)
            DFPA(3)=da(3)
            DFPA(4)=0.D0
            DFPA(4)=-DINT_F2(dl(1),0) ! the function should be 0 at dl(1)
            dintg=DINT_F2(dl(2),0)
            IF(dintg.LT.1.D0) THEN
               DFPA(1)=DFPA(1)/dintg
               DFPA(3)=DFPA(3)/dintg
               DFPA(4)=DFPA(4)/dintg
            ENDIF
            DFPA(4)=DFPA(4)-DBLE(rnd) ! zero crossing
C
            maxf=5000
            CALL DZERO(dl(1),dl(2),dv,dres,1.D-5,maxf,DINT_F2)
            xx=REAL(dv)
            IF(ABS(dres).GT.ABS(dl(2)-dl(1))) THEN
               WRITE(6,FMT=
     +         '('' *** GPXCOSTHR random generator failed '',3D12.5)')
     +                    ,dres,dl
               WRITE(6,FMT='(10D12.4)') (da(i),i=1,3),(DFPA(i),i=1,4)
               WRITE(6,FMT='(10D12.4)') 
     +              DINT_F2(dl(1),0),DINT_F2(dl(2),0)
            ELSE
               IERR=0
            ENDIF
         ENDIF
      ENDIF
C
      IF(IERR.NE.0) GO TO 999
C
      IF(ivar.EQ.1) THEN
         COSTH=xx
      ELSE IF(ivar.EQ.2) THEN   ! calculate the polar angle in CM
         tt=xx
C         t=m1**2+m3**2-2E1*E3+2p1*p3*ct
         COSTH=0.
         qq=(TMX-TMN)/2.
C         write(6,*) 'tmn=',TMN,TMX,qq
         IF(qq.GT.0.) COSTH=(tt-(TMX+TMN)/2.)/qq
      ENDIF
      COSTH=COSTH*csign
C
 999  CONTINUE
C
      END
C
      DOUBLE PRECISION FUNCTION DPOLFMY(N,DA,DX)
C---     Polynomial function
      IMPLICIT NONE
      INTEGER N,i
      DOUBLE PRECISION DA(N),DX,dp,dres
C
      dres=0.D0
      dp=1.D0
      DO i=1,N
         dres=dres+DA(i)*dp
         dp=dp*DX
      ENDDO
      DPOLFMY=dres
      RETURN
      END
C
      DOUBLE PRECISION FUNCTION DINT_F2(DX,IFL)
C---     Integral Function of a*EXP(b*x)+c
      IMPLICIT NONE
      INTEGER IFL
      DOUBLE PRECISION DX
      COMMON/CFUN_COS/ DFPA(10)
      DOUBLE PRECISION DFPA
      INTEGER ntry
      SAVE ntry
C
      DINT_F2=DFPA(1)*EXP(DFPA(2)*DX)+DFPA(3)*DX+DFPA(4)
      IF(IFL.EQ.1) THEN
         ntry=1
      ELSE IF(IFL.EQ.2) THEN
         ntry=ntry+1
      ELSE IF(IFL.EQ.3) THEN
C         WRITE(6,*) ' DZERO calls=',ntry
      ENDIF
      RETURN
      END
