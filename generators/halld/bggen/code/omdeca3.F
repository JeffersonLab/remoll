      SUBROUTINE OMDECA3(P0,AM,XFAC,POUT)
C
C---   3-body phase space decays/reactions 
C
C---   Input: P0 - initial 4-vector, P0**2 - mass(energy) of the initial state,
C                  defined in the "LAB" frame
C             AM(1:3) - masses of the products
C             XFAC: generate the COS(TH) (of 23 to P0 direction in CM) as EXP(XFAC*COSTH)
C                   =0. - uniform distribution form -1 to 1
C
C      Output: POUT(1:4,1:3) - the secondary 4-momenta
C              
C
C====     Method: dG = const * dm12**2 * dm23**2
C====            1) simulate m12**2,m23**2 in the allowed intervals, independently
C                2) reject kinematically forbidden combinations (no reordering of random numbers)
C                2) using m23**2:
C                   calculate e1a,p1a in CM of 1+2+3
C                   calculate e2b,p2b in CM of 2+3
C                   1 is sent along -Z
C                2) calculate the COS(TH) of m23 decay in its CM, from m12 and e1a,p1a,e2b,p2b 
C                3) if no solution exists - jump to 1)
C                4) rotate the event (3 random angles) 
C
      IMPLICIT NONE
      REAL P0(4),AM(3),XFAC,POUT(4,3)
C
      REAL RNDM
C
      REAL pp(4,3)  ! CM, 1 along -Z, 2,3 - in ZX plane 
     +    ,ppv(4,3) ! CM, in this frame Z is along P0
     +    ,vm(3)    ! direction of the 23 combination in CM, LAB angles
     +    ,am12s,am23s,am12,am23,rnd(2),q,ams(3),ecm,ecms
     +    ,p1a,e1a,p2b,e2b,costh,bet,gam,twopi,phi,phi2,ct,st
     +    ,rot(3,3),p0m,betap(4)
      INTEGER i,j,ntry
C
C--- 
C
      DO i=1,3
         DO j=1,4
            POUT(j,i)=0.
         ENDDO
      ENDDO
      ecms=P0(4)**2-P0(1)**2-P0(2)**2-P0(3)**2
      IF(ecms.LE.0.) THEN
         WRITE(6,*) ' *** OMDECA3 space-like initial vector ',ecms,P0
         GO TO 999
      ENDIF
      ecm=SQRT(ecms)
      IF(ecm.LE.AM(1)+AM(2)+AM(3)) THEN
         WRITE(6,*) ' *** OMDECA3 below threshold ',ecm,AM
         GO TO 999
      ENDIF
      DO i=1,3
         ams(i)=AM(i)**2
      ENDDO
      ntry=0
C
 10   ntry=ntry+1
      IF(ntry.GT.10000) THEN
         WRITE(6,*) ' *** OMDECA3 error - long looping, ntry=',ntry
         GO TO 999
      ENDIF
      DO i=1,2
         rnd(i)=RNDM(rnd(i))
      ENDDO
      q=(AM(1)+AM(2))**2
      am12s=q+rnd(1)*((ecm-AM(3))**2-q)
      q=(AM(2)+AM(3))**2
      am23s=q+rnd(2)*((ecm-AM(1))**2-q)
      am12=SQRT(am12s)
      am23=SQRT(am23s)
C
      q=ecms+ams(1)+ams(2)+ams(3)-am12s
      IF(am23s.GE.q-(AM(1)+AM(3))**2) GO TO 10
      IF(am23s.LE.q-  (ecm-AM(2))**2) GO TO 10
C
      e1a=(ecms+ams(1)-am23s)/2./ecm
      p1a=SQRT(e1a**2-ams(1))
      e2b=(am23s+ams(2)-ams(3))/2./am23
      p2b=SQRT(e2b**2-ams(2))
C
C---    am23 goes along Z
C---    Lorentz boost to am23: 
C
      bet=p1a/(ecm-e1a)
      gam=(ecm-e1a)/am23
C
      costh=(am12s-ams(1)-ams(2)-2.*gam*e2b*(e1a+bet*p1a))/
     +                          (2.*gam*p2b*(p1a+bet*e1a))
      IF(ABS(costh).GT.1.) GO TO 10
C
      DO i=1,3
         DO j=1,4
            pp(j,i)=0.
         ENDDO
      ENDDO
C
      pp(3,1)=-p1a
      pp(4,1)= e1a
      pp(1,2)= p2b*SQRT(1.-costh**2)
      pp(3,2)= gam*(p2b*costh+bet*e2b)
      pp(4,2)= gam*(e2b      +bet*p2b*costh)
      DO i=1,3
         pp(i,3)=-pp(i,1)-pp(i,2)
      ENDDO
      pp(4,3)=ecm-pp(4,1)-pp(4,2)
C
      twopi=4.*ACOS(0.)
C
C---      Rotate 2,3 around Z
C
      phi2=twopi*RNDM(twopi)
      DO i=2,3
         q=pp(1,i)
         pp(1,i)=q*COS(phi2)
         pp(2,i)=q*SIN(phi2)
      ENDDO
C
C---     Random polar angle (apply exponential COSTH-dep, if needed)
C
      IF(ABS(XFAC).GT.0.001) THEN
         ct=LOG(EXP(-XFAC)+RNDM(ct)*(EXP(XFAC)-EXP(-XFAC)))/XFAC
      ELSE
         ct=-1.+RNDM(ct)*2.
      ENDIF
C
      phi=twopi*RNDM(phi)
      st=SQRT(1.-ct**2)
      vm(1)=st*COS(phi)  ! the direction of 23 combination in LAB, CM
      vm(2)=st*SIN(phi)
      vm(3)=ct
C
      CALL OMROTS(vm,rot)  ! rotate the momenta to this frame
      DO i=1,3
         CALL OMROTV(pp(1,i),rot,ppv(1,i))
         ppv(4,i)=pp(4,i)
      ENDDO
C
C---      Rotate to the frame where Z goes along P0 
C
      p0m=SQRT(P0(1)**2+P0(2)**2+P0(3)**2)
      IF(p0m.GT.0.00001) THEN
         CALL OMROTS(P0,rot)
         DO i=1,3
            CALL OMROTV(ppv(1,i),rot,pp(1,i))
            pp(4,i)=ppv(4,i)
         ENDDO
      ELSE
         DO i=1,3
            DO j=1,4
               pp(j,i)=ppv(j,i)
            ENDDO
         ENDDO
      ENDIF
C
C--- Lorentz boost to P0
C
      q=0.
      DO i=1,3
         betap(i)=-P0(i)/P0(4)
         q=q+betap(i)**2
      ENDDO
C
      IF(q.GT.1.E-10) THEN
         betap(4)=P0(4)/ecm
         DO i=1,3
            CALL GLOREN(betap(1),pp(1,i),POUT(1,i))
         ENDDO
      ELSE
         DO i=1,3
            DO j=1,3
               POUT(j,i)=pp(j,i)
            ENDDO
         ENDDO
      ENDIF
C
 999  RETURN
      END

