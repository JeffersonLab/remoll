      SUBROUTINE LOWEN_INI(IERR)
C
C---   Low energy photoproduction initialization
C
      IMPLICIT NONE
      INTEGER IERR
C
      INCLUDE 'bg_ctrl.inc'
      INCLUDE 'bg_proc.inc'
      INCLUDE 'bg_partc.inc'
C
      REAL SAIDE,SAIDXSECA,GPXSECP
      LOGICAL HEXIST
C
      INTEGER i,j,nb,ipro,npro,id1,idt,ncth,icth,ihi,lun,iost
     +       ip,lout
      REAL emn,emx,flx1,flx2,flx,xsec,dx,de,e,dcth,cth
     +    ,xlum,ecurr,xsth,targth,frate
C
      CHARACTER tit*132,cpro*2,cenr*4,cline*132
C
      CHARACTER cnampro(MXPROC)*16
      DATA cnampro/'p pi0           '
     +            ,'n pi+           '
     +            ,'p pi+ pi-       '
     +            ,'p rho0          '
     +            ,'Delta++ pi-     '
     +            ,'p pi0 pi0       '
     +            ,'n pi+ pi0       '
     +            ,'p eta           '
     +            ,'p pi+ pi- pi0   '
     +            ,'n pi+ pi+ pi-   '
     +            /
C
      INTEGER itypr(MXOUT,MXPROC)
      DATA itypr/14, 7, 0, 0, 0, 0
     +          ,13, 8, 0, 0, 0, 0
     +          ,14, 8, 9, 0, 0, 0
     +          ,14,80, 0, 0, 0, 0
     +          ,82, 9, 0, 0, 0, 0
     +          ,14, 7, 7, 0, 0, 0
     +          ,13, 8, 7, 0, 0, 0
     +          ,14,17, 0, 0, 0, 0
     +          ,14, 8, 9, 7, 0, 0
     +          ,13, 8, 8, 9, 0, 0
     + /
C
      IERR=1
      lout=6
C
      DO i=1,MXPROC
         CNPROC(i)=cnampro(i)
         DO j=1,MXOUT
            ITYPROC(j,i)=itypr(j,i)
         ENDDO
      ENDDO
C
C
      IF(NHBEA.LT.1) THEN
         WRITE(6,1010) NHBEA
 1010    FORMAT('  *** Initialization error: NHBEA=',I6)
         GO TO 999
      ENDIF
C
      emn=EPH_LIM(1)
      emx=MIN(EPYMIN,EPH_LIM(2))
C
C---       Adjust the emx to the bin boundary
C
      de=(EPH_LIM(2)-EPH_LIM(1))/NHBEA
      nb=INT((emx-emn)/de)
      emx=emn+nb*de
      EPYMIN=emx
C
C---      Initialize the processes
C
      npro=10     ! number of defined processes 
      ncth=100    ! number of bins in the COS(th) distribution
      dcth=2./ncth
      DO ipro=1,npro
         id1=IDLOWEN+10000*ipro
         WRITE(cpro,FMT='(I2)') ipro
         CALL HBOOK1(id1,'X-section for process '//cpro,nb,emn,emx,0.)
C
C---      Define the COS(TH) plots?
C
         IF(ipro.LE.2.OR.     ! SAID
     +      ipro.EQ.8) THEN   ! eta
            CALL HBOOK1(id1+1,'refer for COS(TH) for process '//cpro
     +                 ,nb,emn,emx,0.) ! use this histogram to get energy bin index and use that to access the COS(TH) for process ipro but only works for ipro <= 2 or ipro==8 Rakitha Mon Aug 17 13:12:59 EDT 2015 why??
         ENDIF
C
         DO i=1,nb
            e=emn+(i-0.5)*de
C
C---            Full x-section
C
            IF(ipro.LE.2) THEN
C
C---          SAID is used
C
               xsec=SAIDE(e,ipro,1)  ! SAID cross section, supressed above 2 GeV
               ihi=i
            ELSE
               xsec=GPXSECP(e,ipro) ! x-sec in mb
               ihi=0
C               CALL GPXCOSTH(e,ipro,0.,ihi,xsth)  ! check the COS(th) distribution
            ENDIF
C
            CALL HF1(id1,e,xsec) ! pass energy to get cross section for ipro process index
            CALL HF1(id1+1,e,REAL(ihi)) ! pass energy to get the bin number related to this energy only work for ipro <= 2 why? Rakitha Mon Aug 17 13:12:59 EDT 2015
C
C--- Get the cos(th) distributions
C
            idt=id1+10+ihi
            IF(ihi.NE.0.AND..NOT.HEXIST(idt)) THEN   ! fill the COS(th) distrib if not yet filled
               WRITE(cenr,FMT='(I4)') i
               CALL HBOOK1(idt,'COS(TH), proc '//cpro//' energy '//cenr
     +                     ,ncth,-1.,1.,0.)
               DO icth=1,ncth
                  cth=-1.+(icth-0.5)*dcth
                  IF(ipro.LE.2) THEN
                     xsth=SAIDXSECA(e,cth,ipro,1)
                  ELSE
C                     CALL GPXCOSTH(e,ipro,cth,ihi,xsth) ! get the COS(th) distribution
                  ENDIF
                  CALL HF1(idt,cth,xsth)
               ENDDO
            ENDIF
         ENDDO
C
         IF(ipro.EQ.1) THEN
            CALL HCOPY(id1,IDLOWEN+15,'X-section for all process ')
         ELSE
            CALL HOPERA(id1,'+',IDLOWEN+15,IDLOWEN+15,1.,1.)
         ENDIF
C
      ENDDO
C
      IERR=0
 999  RETURN
      END
