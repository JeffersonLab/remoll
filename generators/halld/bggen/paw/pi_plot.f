      REAL FUNCTION pi_plot(IFL,KGEANT,KPYTH,IDH)
C
C--       Fills IDH with the kin. parameters of all photons from pi0 or eta decay.
c
c            photons in fcal selected with energies > fcal_thresh
c            photons in bcal selected with energies > bcal_thresh

c            idh      with pi0s with photons in fcal only 
c            idh+1 with pi0s with photons in bcal only
c           idh+2  with pi0s with one photon in fcal, one photon in bcal
c           idh+3  with pi0s failing above cuts
c
C       IFL=0 - p
C          =1 - theta (degrees)
C          =2 - p(Y)-theta(x)(degrees) 
c       KGEANT >0 GENAT particle type
C       KTYP>0 - GEANT particle type
C          <=0  use KPYTH - PYTHIA KF type
c       IDH - number of histogram to be filled
c
c     function based on part_kin, but to plot photons from pi0 (or eta) decay.
c           Elton Smith 2/8/11
c
C
      IMPLICIT NONE
      INTEGER IFL,KGEANT,KPYTH,IDH
C
      INCLUDE ?
      LOGICAL HEXIST
C
      INTEGER ip,j,ifirst,ievstart,nfind,ifind,icnt
      REAL thcut, fcal_thresh, bcal_thresh
      INTEGER jj,nkind, ip1, ip2, itopol
      REAL pf1,th1,qq,pf2,th2, ivmass
      LOGICAL fcal1, fcal2, bcal1, bcal2
      DATA ifirst/1/
      data icnt /0/
      DATA ievstart/0/
      DATA thcut /10./
      DATA fcal_thresh, bcal_thresh  /0.2, 0.2/
c      DATA fcal_thresh, bcal_thresh  /0.5, 0.5/
c      DATA fcal_thresh, bcal_thresh  /0.1, 0.06/
c
c     count entries
c
       icnt = icnt + 1
c
c    valid codes are:
c    KGEANT = 7, KPYTH = 111 (pi0)
c    KGEANT = 17, KPYTH = 221 (eta)
c
       if (KGEANT.eq.7)  then
           KPYTH = 111
       elseif (KGEANT.eq.17) then
           KPYTH = 221
        elseif (KPYTH.eq.111) then
           KGEANT = 7
        elseif (KPYTH.eq.221) then
            KGEANT = 17
         else
c
c           invalid codes
cc
             write (6,*) ' *** pi_plot  illegal code KGEANT, KPYTH =' ,
     1                          KGEANT,KPYTH
             pi_plot =0
             return
          endif
c 
C
      IF(ifirst.EQ.1.OR.IDNEVT.EQ.ievstart) THEN
         IF (IDH.NE.0.AND.HEXIST(IDH) .or.HEXIST(IDH+1).or.
     1          HEXIST(IDH+2).or.HEXIST(IDH+3)) THEN
            CALL HRESET(IDH,'    ')
            CALL HRESET(IDH+1,'    ')
            CALL HRESET(IDH+2,'    ')
            CALL HRESET(IDH+3,'    ')
            ievstart=IDNEVT
         ELSE
            WRITE(6,*) ' *** ERROR: no histogram ID=',
     1      IDH,idh+1,idh+2,idh+3
         ENDIF
      ENDIF
      ifirst=0
C
      nfind=0
      itopol =0
C
      DO ip=1,NP
         ifind=0       
c
c          find pi0 or eta
c   
            IF(KPYTH.EQ.ITYP(3,ip)) THEN
               ip1 = ityp(5,ip)
               ip2 = ityp(6,ip)

c              write(6,*) ifind,KGEANT,KPYTH,(ITYP(jj,ip),jj=1,6)
c
c            check decay products are photons
c
         If (ip2.eq.ip1+1) then
         if ( ityp(3,ip1).eq. 22 .and.  ityp(3,ip2).eq.22) then

                nfind=nfind+1
                 ifind = 1
c        
                  qq=0.
                  DO j=1,3
                      qq=qq+POUT(j,ip1)**2
                  ENDDO
                  pf1=SQRT(qq)
                  th1=ACOS(POUT(3,ip1)/pf1)*180./3.1416
c
                  qq=0.
                  DO j=1,3
                      qq=qq+POUT(j,ip2)**2
                  ENDDO
                  pf2=SQRT(qq)
                  th2=ACOS(POUT(3,ip2)/pf2)*180./3.1416

c                 write (6,*)  'ip1, pf1, th1=',ip1,pf1,th1,
c     1                                 ' ip2, pf2,th2=',ip2,pf2,th2
c
c                invariant mass
c
            ivmass = sqrt(2*(pf1*pf2 -  pout(1,ip1)*pout(1,ip2) 
     1                               -  pout(2,ip1)*pout(2,ip2) 
     2                             -  pout(3,ip1)*pout(3,ip2)  ))
c          write (6,*) ' ivmass =', ivmass


c
c              determine topology
c                         nominal: fcal 1-11 deg, bcal 11-126 deg
c
            fcal1 = th1.gt.1 .and. th1.lt.11.and. pf1.gt.fcal_thresh
            fcal2 = th2.gt.1 .and. th2.lt.11.and.pf2.gt.fcal_thresh 
            bcal1 = th1.gt.11 .and. th1.lt.126.and. pf1.gt.bcal_thresh
            bcal2 = th2.gt.11 .and. th2.lt.126.and.pf2.gt.bcal_thresh 
                 if (fcal1  .and. fcal2) then
                             itopol = 1
c            write (6,*) ' fcal th1, th2, pf1,pf2=', th1, th2, pf1,pf2
                elseif (bcal1 .and. bcal2) then
                            itopol = 2
c            write (6,*) ' bcal th1, th2, pf1,pf2=', th1, th2, pf1,pf2
                elseif  (fcal1.and. bcal2 .or. bcal1.and.fcal2) then
                           itopol = 3
c       write (6,*) ' fcal-bcal th1, th2, pf1,pf2=', th1, th2, pf1,pf2
                 else
                           itopol = 4
c             write (6,*) ' None th1, th2, pf1,pf2=', th1, th2, pf1,pf2
                  endif

             endif
                     
  
            endif
            ENDIF

         IF(ifind.NE.0) THEN
C
            IF(IFL.EQ.0) THEN
               if (itopol .eq. 1) then
                      CALL HFILL(IDH,pf1,0.,1.)
                      CALL HFILL(IDH,pf2,0.,1.)
                elseif  (itopol .eq. 2) then
                      CALL HFILL(IDH+1,pf1,0.,1.)
                      CALL HFILL(IDH+1,pf2,0.,1.)
                elseif  (itopol .eq. 3) then
                      CALL HFILL(IDH+2,pf1,0.,1.)
                      CALL HFILL(IDH+2,pf2,0.,1.)
                else
                      CALL HFILL(IDH+3,pf1,0.,1.)
                      CALL HFILL(IDH+3,pf2,0.,1.)
                 endif
c
            ELSE IF(IFL.EQ.1) THEN
               if (itopol .eq. 1) then
                      CALL HFILL(IDH,th1,0.,1.)
                      CALL HFILL(IDH,th2,0.,1.)
                elseif  (itopol .eq. 2) then
                      CALL HFILL(IDH+1,th1,0.,1.)
                      CALL HFILL(IDH+1,th2,0.,1.)
                elseif  (itopol .eq. 3) then
                      CALL HFILL(IDH+2,th1,0.,1.)
                      CALL HFILL(IDH+2,th2,0.,1.)
                else
                      CALL HFILL(IDH+3,th1,0.,1.)
                      CALL HFILL(IDH+3,th2,0.,1.)
                 endif

            ELSE IF(IFL.EQ.2) THEN
               CALL HFILL(IDH,th1,pf1,1.)
            ENDIF
         ENDIF
      ENDDO
C
c      if (nfind.gt.0)   write (6,*) 
c     1   ' icnt, idnevt, nfind, itopol=', icnt, idnevt,nfind, itopol
      pi_plot = nfind*10 + itopol
C
      RETURN
      END
