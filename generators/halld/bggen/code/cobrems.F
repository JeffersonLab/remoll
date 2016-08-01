C This program calculates the spectrum of bremsstrahlung radiation from a
C crystal radiator.  The formalism is that described in the following paper.
C  W. Kaune, G. Miller, W. Oliver, R.W. Williams, and K.K. Young,
C
C   "Inclusive cross sections for pion and proton production by photons
C    using collimated coherent bremsstrahlung", Phys Rev D, vol 11,
C    no 3 (1975) pp. 478-494.
C
C Author: Richard Jones    8-July-1997
C
C #define vector real

      Subroutine cobrems(Emax,Epeak,Dist,colDiam)
      real Emax,Epeak,Dist,colDiam
      include 'cobrems.inc'
      REAL inputs(10)
      integer i
      real c
      dpi=acos(-1d0)
      me=5.11e-4	!electron mass (GeV)
      alpha=7.297e-3    !fine structure constant
      hbarc=1.97e-16    !Planck's constant * speed of light (GeV m)
      Z=6		!atomic number of diamond
      a=3.56e-10	!dimension of diamond unit cell (m)
      Aphonon=0.5e9	!phonon-free recoil constant (GeV**-2)
      betaFF=111*Z**(-1/3.)/me	!cutoff for atomic form-factor (/GeV)
      mospread=50e-6	!crystal r.m.s. mosaic spread
      E=Emax		!electron beam energy (GeV)
      Erms=6.0e-3       !electron beam energy rms spread (GeV)
      emitx=1.0e-8	!electron beam emittance in x (m r)
      emity=0.5e-8	!electron beam emittance in y (m r)
      spot=0.0005	!electron beam spot size at collimator
      D=Dist		!distance from radiator to collimator
      t=20.0e-6		!thickness of radiator (m)
      collim=colDiam	!collimator diameter (m)
      thx=-0.0300/E	!rotation of crystal about x (first)
      thy=0.100		!rotation of crystal about y (second)
C-- require Epeak < Emax
      if (Epeak.ge.Emax) then
        return
      endif
C-- approximate calculation of angle from primary edge energy
      edge=Epeak	!desired position of primary edge
      qtotal=9.8e-6	!Qtot for dominant lattice vector
      qlong=edge/(E-edge)*me**2/(2*E)
      thx=-qlong/qtotal
C-- PDG formula for radiation length, converted to meters
      c=alpha*Z
      radlen=4*nsites*alpha**3*(hbarc/(a*me))**2/a
     +      *( (Z**2)*(log(184.15*Z**(-1/3.))
     +                -(c**2)*(1/(1+c**2) + 0.20206 - 0.0369*(c**2)
     +                        + 0.0083*(c**4) - 0.002*(c**6)))
     +        + Z*log(1194*Z**(-2/3)))
      radlen=1/radlen
      write(6,*)
      write(6,1000)
1000  format('Initialization for coherent bremsstralung calculation')
      write(6,1010) E
1010  format(' electron beam energy:',f12.3,'GeV')
      write(6,1020) 'diamond',t*1e6
1020  format(' radiator crystal: ',a10,', thickness',f8.0,'um')
      write(6,1030) radlen*1e2,mospread*1e6
1030  format(' radiation length:',f8.1,'cm, mosaic spread:',f8.1,'ur')
      write(6,1040) collim/(2*D)*(E/me)
1040  format(' photon beam collimator half-angle:',f12.3,'(m/E)')
      write(6,1050) thx*1e3,thy*1e3
1050  format(' crystal orientation: theta-x',f10.3,'mr',
     +      /'                      theta-y',f10.3,'mr')
      write(6,*)' Collimator diameter: ',colDiam
C  define the unit cell of the radiator crystal
      ucell(1,1)=0
      ucell(2,1)=0
      ucell(3,1)=0
      do i=1,3
        ucell(1,1+i)=ucell(1,1)+0.5
        ucell(2,1+i)=ucell(2,1)+0.5
        ucell(3,1+i)=ucell(3,1)+0.5
        ucell(i,1+i)=ucell(i,1+i)-0.5
      enddo
      ucell(1,5)=0.25
      ucell(2,5)=0.25
      ucell(3,5)=0.25
      do i=1,3
        ucell(1,5+i)=ucell(1,5)+0.5
        ucell(2,5+i)=ucell(2,5)+0.5
        ucell(3,5+i)=ucell(3,5)+0.5
        ucell(i,5+i)=ucell(i,5+i)-0.5
      enddo
C  define the crystal->lab rotation matrix
      rotate(1,1)=1
      rotate(1,2)=0
      rotate(1,3)=0
      rotate(2,1)=0
      rotate(2,2)=1
      rotate(2,3)=0
      rotate(3,1)=0
      rotate(3,2)=0
      rotate(3,3)=1
      call rotmat(rotate,0d0,dpi/2,0d0)	!point (1,0,0) along beam
      call rotmat(rotate,0d0,0d0,dpi/4)	!point (0,1,1) vertically
      call rotmat(rotate,-thx,0d0,0d0)	!the goniometer-x rotation
      call rotmat(rotate,0d0,-thy,0d0)	!the goniometer-y rotation
      write(6,2000) (rotate(1,j),j=1,3)
      write(6,2000) (rotate(2,j),j=1,3)
      write(6,2000) (rotate(3,j),j=1,3)
2000  format(3f12.6)
      end

      real function cohrat(x)
      real x
      include 'cobrems.inc'
      real yc,yi
      yc=dNcdx(x)
      yi=dNidx(x)
      cohrat=(yc+yi)/(yi+1e-30)
      end

      real function dNtdx(x)
      real x
      include 'cobrems.inc'
      dNtdx=dNcdx(x)+dNidx(x)
      end

      real function dNtdx3(x,dRadCol,diamCol)
      real x,dRadCol,diamCol
      include 'cobrems.inc'
      if (dRadCol.gt.0) D=dRadCol
      if (diamCol.gt.0) collim=diamCol
      if (diamCol.lt.0) collim=-2*D*diamCol*me/E
      dNtdx3=dNcdx(x)+dNidx(x)
      end

      real function dNtdk(k)
      real k
      include 'cobrems.inc'
      dNtdk=dNtdx(k/E)/E
      end

      real function dNcdx(x)
      real x
      include 'cobrems.inc'
      real phi
      phi=dpi/4
      dNcdx=2*dpi*dNcdxdp(x,phi)
      end

      real function dNcdx3(x,dRadCol,diamCol)
      real x,dRadCol,diamCol
      include 'cobrems.inc'
      real phi
      if (dRadCol.gt.0) D=dRadCol
      if (diamCol.gt.0) collim=diamCol
      if (diamCol.lt.0) collim=-2*D*diamCol*me/E
      phi=dpi/4
      dNcdx3=2*dpi*dNcdxdp(x,phi)
      end

      real function dNcdxdp(x,phi)
      real x,phi
      include 'cobrems.inc'
      integer h,k,l
      double precision ReS,ImS,S2
      double precision q2,qT2,q(3),qdota
      real xmax,theta2,FF,sum
      integer hmin,kmin,lmin
      real q3min
      integer i
      real sigma0
      sigma0=16*dpi*t*Z**2*alpha**3*E*(hbarc/a**2)*(hbarc/a/me)**4
      q2points=0
      q3min=1
      sum=0
      do h=0,0
        do k=-10,10
          do l=-10,10
c       do k=-2,-2
c         do l=-2,-2
            if (h/2*2.eq.h) then
              if (k/2*2.ne.k) then
                goto 10
              elseif (l/2*2.ne.l) then
                goto 10
              elseif ((h+k+l)/4*4.ne.h+k+l) then
                goto 10
              endif
            elseif (k/2*2.eq.k) then
              goto 10
            elseif (l/2*2.eq.l) then
              goto 10
            endif
            ReS=0
            ImS=0
            do i=1,nsites
              qdota=2*dpi*(h*ucell(1,i) + k*ucell(2,i) + l*ucell(3,i))
              ReS=ReS+cos(qdota)
              ImS=ImS+sin(qdota)
            enddo  
            S2=ReS**2+ImS**2
            if (S2.lt.1e-4) then
              goto 10
            endif
            qnorm=2*dpi*hbarc/a
            q(1)=qnorm*(rotate(1,1)*h + rotate(1,2)*k + rotate(1,3)*l)
            q(2)=qnorm*(rotate(2,1)*h + rotate(2,2)*k + rotate(2,3)*l)
            q(3)=qnorm*(rotate(3,1)*h + rotate(3,2)*k + rotate(3,3)*l)
            q2=q(1)**2+q(2)**2+q(3)**2
            qT2=q(1)**2+q(2)**2
            xmax=2*E*q(3)
            xmax=xmax/(xmax+me**2)
            if ((x.gt.xmax).or.(xmax.gt.1)) then
              goto 10
            else
c             write(6,*) h,k,l,S2
c             write(6,*) q2,xmax
            endif
            if (q(3).lt.q3min) then
              q3min=q(3)
              hmin=h
              kmin=k
              lmin=l
            endif
            theta2=(1-x)*xmax/(x*(1-xmax)) - 1
            FF=1/(1+q2*betaFF**2)
            sum=sum+sigma0*qT2*S2*exp(-Aphonon*q2)*(FF*betaFF**2)**2
     +         * ((1-x)/(x*(1+theta2))**2)
     +         * ((1+(1-x)**2)
     +               - 8*(theta2/(1+theta2)**2)*(1-x)*cos(phi)**2)
     +         * acceptance(theta2)
c    +         * polarization(x,theta2)
C comment out the preceding line to disable polarization -RTJ
            q2points=q2points+1
            q2theta2(q2points)=theta2
            q2weight(q2points)=sum
10          continue
          enddo
        enddo
      enddo
      dNcdxdp=sum
c     if (q3min.lt.1) write(6,*) hmin,kmin,lmin,' best plane at',x
      end

      real function dNidx(x)
      real x
      include 'cobrems.inc'
      integer iter,niter
      real theta2       !numerical integration over d(theta**2) over [0,inf]
      real u,du         !is transformed by u=1/(1+theta**2) to d(u) over [0,1]
      niter=50
      dNidx=0
      if (x.gt.1) then
        return
      endif
      du=1./niter
      do iter=1,niter
        u=(iter-0.5)/niter
        theta2=(1-u)/u
        dNidx=dNidx+dNidxdt2(x,theta2)*du/u**2
      enddo
c     write(6,*) dNidx
      end

C  In the following paper, a closed form is given for the integral that
C  is being performed analytically by dNidx.  I include this second form
C  here in case some time it might be useful as a cross check.
C
C   "Coherent bremsstrahlung in crystals as a tool for producing high
C    energy photon beams to be used in photoproduction experiments at
C    CERN SPS", Nucl. Instr. Meth. 204 (1983) pp.299-310. 
C
C  Note: in this paper they have swapped subscripts for coherent and
C  incoherent intensities.  This is not very helpful to the reader!
C
C  The result is some 15% lower radiation rate than the result of dNidx.
C  I take the latter to be more detailed (because it gives a more
C  realistic behaviour at the endpoint and agrees better with the PDG
C  radiation length for carbon).  Most of this deficiency is remedied
C  by simply replacing Z**2 in the cross section with Z*(Z+zeta) as
C  recommended by Kaune et.al., and followed by the PDG in their fit
C  to radiation lengths.
C
C                              WARNING
C  dNidx and dNBidx give the incoherent radiation rate for crystalline
C  radiators.  If you take the incoherent radiation formulae here and
C  integrate them you will NOT obtain the radiation length for amorphous
C  radiators; it will be overestimated by some 15%.  The reason is that
C  the part of the integral in q-space that is covered by the discrete
C  sum has been subtracted to avoid double-counting with the coherent
C  part.  If you were to spin the crystal fast enough, the coherent
C  spectrum would average out to yield the remaining 15% with a spectral
C  shape resembling the Bethe-Heitler result.

      real function dNBidx(x)
      real x
      include 'cobrems.inc'
      real psiC1,psiC2
      real AoverB2,Tfact
      real zeta
      AoverB2=Aphonon/betaFF**2
      Tfact=-(1+AoverB2)*exp(AoverB2)*EXPINT(AoverB2)
      psiC1=2*(2*log(betaFF*me)+Tfact+2)
      psiC2=psiC1-2/3.
      zeta=log(1440*Z**(-2/3.))/log(183*Z**(-1/3.))
      dNBidx=nsites*t*Z*(Z+zeta)*alpha**3*(hbarc/(a*me))**2/(a*x)
     +      * (psiC1*(1+(1-x)**2) - psiC2*(1-x)*2/3.)
      end

      real function dNidxdt2(x,theta2)
      real x,theta2
      include 'cobrems.inc'
      real MSchiff,delta,zeta
      delta=1.02
      zeta=log(1440*Z**(-2/3.))/log(183*Z**(-1/3.))
      MSchiff=1/(((me*x)/(2*E*(1-x)))**2 + 1/(betaFF*me*(1+theta2))**2)
      dNidxdt2=2*nsites*t*Z*(Z+zeta)*alpha**3*(hbarc/(a*me))**2/(a*x)
     +      *( ((1+(1-x)**2)-4*theta2*(1-x)/(1+theta2)**2)/(1+theta2)**2
     +        *(log(MSchiff) - 2*delta*Z/(Z+zeta))
     +        + 16*theta2*(1-x)/(1+theta2)**4 - (2-x)**2/(1+theta2)**2 )
     +      * acceptance(theta2)
c     write(6,*) dNidxdt2
      end

      real function rpara(x,theta2,phi)
      real x,theta2,phi
      include 'cobrems.inc'
      rpara=0.5*((1+1-x)**2)*(1+theta2)**2
     +     -8*theta2*(1-x)*cos(phi)**2
     +     -8*theta2**2*(1-x)*cos(phi)**2*sin(phi)**2
      end

      real function rortho(x,theta2,phi)
      real x,theta2,phi
      include 'cobrems.inc'
      rortho=0.5*x**2*(1+theta2)**2
     +      +8*theta2**2*(1-x)*cos(phi)**2*sin(phi)**2
      end

      real function polarization(x,theta2)
      real x,theta2
      include 'cobrems.inc'
      polarization=2*(1-x)/((1+theta2)**2*((1-x)**2+1) - 4*theta2*(1-x))
      end

      real function acceptance(theta2)
      real theta2
      include 'cobrems.inc'
      REAL sig(4)
      real u,var0,varMS,thetaC
      real pu,du2,u0,u1,u2
      integer iter,niter
      real theta
Comment out the following lines to enable collimation -RTJ
  	acceptance=1
C        write(6,*) sqrt(theta2)
C	return
Comment out the preceding lines to enable collimation -RTJ
      acceptance=0
      niter=50
      theta=sqrt(theta2)
      thetaC=collim/(2*D)*(E/me)
      var0=(spot/D*(E/me))**2
      varMS=sigma2MS(t)*(E/me)**2
      sig(1)=sqrt(var0)
      sig(2)=sqrt(varMS)
      if (theta.lt.thetaC) then
        u1=thetaC-theta
        if (u1**2/(var0+varMS).gt.20) then
          acceptance=1
          return
        endif
        do iter=1,niter
          u=u1*(iter-0.5)/niter
          u2=u**2
          du2=2*u*u1/niter
          if (varMS/var0.gt.1e-4) then
            pu=(EXPINT(u2/(2*(var0+varMS)))-EXPINT(u2/(2*var0)))
     +        /(2*varMS)
          else
            pu=exp(-u2/(2*var0))/(2*var0)
          endif
          acceptance=acceptance + pu*du2
        enddo
      endif
      u0=abs(theta-thetaC)
      u1=abs(theta+thetaC)
      do iter=1,niter
        u=u0+(u1-u0)*(iter-0.5)/niter
        u2=u**2
        du2=2*u*(u1-u0)/niter
        if (varMS/var0.gt.1e-4) then
          pu=(EXPINT(u2/(2*(var0+varMS)))-EXPINT(u2/(2*var0)))
     +      /(2*varMS)
        else
          pu=exp(-u2/(2*var0))/(2*var0)
        endif
        acceptance=acceptance + pu*du2/dpi
     +    * atan2(sqrt((theta2-(thetaC-u)**2)*((thetaC+u)**2-theta2)),
     +            theta2-thetaC**2+u2)
      enddo
      end

      subroutine rotmat(matrix,thx,thy,thz)
      double precision matrix(3,3),thx,thy,thz
C  Matrix(out) = Rx(thx) Ry(thy) Rz(thz) Matrix(in)
C   with rotations understood in the passive sense
      double precision x,y,z
      double precision sint,cost
      integer i
      if (thz.ne.0) then
        sint=sin(thz)
        cost=cos(thz)
        do i=1,3
          x=matrix(1,i)
          y=matrix(2,i)
          matrix(1,i)=cost*x+sint*y
          matrix(2,i)=-sint*x+cost*y
        enddo
      endif
      if (thy.ne.0) then
        sint=-sin(thy)
        cost=cos(thy)
        do i=1,3
          x=matrix(1,i)
          z=matrix(3,i)
          matrix(1,i)=cost*x+sint*z
          matrix(3,i)=-sint*x+cost*z
        enddo
      endif
      if (thx.ne.0) then
        sint=sin(thx)
        cost=cos(thx)
        do i=1,3
          y=matrix(2,i)
          z=matrix(3,i)
          matrix(2,i)=cost*y+sint*z
          matrix(3,i)=-sint*y+cost*z
        enddo
      endif
      end

      subroutine convol(nbins)
      integer nbins
      include 'cobrems.inc'
      REAL hisx(10000),hisy(10000),sig(4)
      real norm(10000),result(10000)
      real x,x0,x1,dx
      real alph,dalph
      real var0,varMS
      real term
      integer i,ii,j
      x0=hisx(1)
      x1=hisx(nbins)
      var0=(mospread**2+(emitx/spot)**2)
      varMS=sigma2MS(t)
      sig(3)=sqrt(var0)*E/me
      sig(4)=sqrt(varMS)*E/me
C--Here we have to guess which characteristic angle alph inside the crystal
C  is dominantly responsible for the coherent photons in this bin in x.
C  I just use the smallest of the two angles, but this does not work when
C  both angles are small, and you have to be more clever -- BEWARE!!!
C--In any case, fine-tuning below the mosaic spread limit makes no sense.
      alph=min(abs(thx),abs(thy))
      if (alph.eq.0) then
        alph=max(abs(thx),abs(thy))
      else
        alph=max(alph,mospread)
      endif

      do j=1,nbins
        norm(j)=0
        result(j)=0
        do i=-nbins,nbins
          dx=(x1-x0)*(j-i)/nbins
          x=x0+(x1-x0)*(j-0.5)/nbins
          dalph=dx*alph/(x*(1-x))
          if (varMS/var0.gt.1e-4) then
            term=dalph/varMS
     +      *(ERF(dalph/sqrt(2*(var0+varMS))) - ERF(dalph/sqrt(2*var0)))
     +         + sqrt(2/dpi)/varMS
     +      *(exp(-dalph**2/(2*(var0+varMS)))*sqrt(var0+varMS)
     +       -exp(-dalph**2/(2*var0))*sqrt(var0))
          else
            term=exp(-dalph**2/(2*var0))/sqrt(2*dpi*var0)
          endif
          term=term*alph/x
          norm(j)=norm(j)+term
        enddo
      enddo

c     write(6,*) norm

      do i=-nbins,nbins
        if (i.lt.1) then
          ii=1-i
        else
          ii=i
        endif
        do j=1,nbins
          dx=(x1-x0)*(j-i)/nbins
          x=x0+(x1-x0)*(j-0.5)/nbins
          dalph=dx*alph/(x*(1-x))
          if (varMS/var0.gt.1e-4) then
            term=dalph/varMS
     +      *(ERF(dalph/sqrt(2*(var0+varMS))) - ERF(dalph/sqrt(2*var0)))
     +         + sqrt(2/dpi)/varMS
     +      *(exp(-dalph**2/(2*(var0+varMS)))*sqrt(var0+varMS)
     +       -exp(-dalph**2/(2*var0))*sqrt(var0))
          else
            term=exp(-dalph**2/(2*var0))/sqrt(2*dpi*var0)
          endif
          term=term*alph/x
          result(ii)=result(ii)+term*hisy(j)/norm(j)
        enddo
      enddo

      do i=1,nbins
        if (abs(result(i)).gt.1e-35) then
          hisy(i)=result(i)
        else
          hisy(i)=0
        endif
      enddo
      end

      real function sigma2MS(tt)
      real tt
      sigma2MS=sigma2MS_Geant(tt)
      end

      real function sigma2MS_Kaune(tt)
      real tt
      include 'cobrems.inc'
C--Multiple scattering formula of Kaune et.al. 
c  with a correction factor from a multiple-scattering calculation
c  taking into account the atomic and nuclear form factors for carbon.

c--Note by RTJ, Oct. 13, 2008:
c  I think this formula overestimates multiple scattering in thin targets
c  like these diamond radiators, because it scales simply like sqrt(tt).
c  Although the leading behavior is sqrt(tt/radlen), it should increase
c  faster than that because of the 1/theta**2 tail of the Rutherford
c  distribution that makes the central gaussian region swell with increasing
c  number of scattering events.  For comparison, I include below the PDG
c  formula (sigma2MS), the Moliere formula used in the Geant3 simulation
c  of gaussian multiple scattering (sigma2MS_Geant), and a Moliere fit for
c  thin targets taken from reference Phys.Rev. vol.3 no.2, (1958), p.647
c  (sigma2MS_Hanson).  The latter two separate the gaussian part from the
c  tails in different ways, but both agree that the central part is much
c  more narrow than the formulation by Kaune et.al. below.

      carboncor=4.2/4.6
      sigma2MS_Kaune=8*dpi*nsites*alpha**2*Z**2*tt*(hbarc/(E*a))**2/a
     +              *log(183*Z**(-1/3.))
     +              *carboncor
      end

      real function sigma2MS_pdg(tt)
      real tt
      include 'cobrems.inc'
C--The PDG formula instead (with beta=1, charge=1)
c  This formula is said to be within 11% for t > 1e-3 rad.len.
      sigma2MS_pdg=(13.6e-3/E)**2*(tt/radlen)
     +            *(1+0.038*log(tt/radlen))**2
      end

      real function sigma2MS_Geant(tt)
      real tt
      include 'cobrems.inc'
C--Geant3 formula for the rms multiple-scattering angle
c  This formula is based on the theory of Moliere scattering.  It contains
c  a cutoff parameter F that is used for the fractional integral of the
c  scattering probability distribution that is included in computing the
c  rms.  This is needed because the complete distribution of scattering
c  angles connects smoothly from a central gaussian (small-angle
c  multiple-scattering regime) to a 1/theta^2 tail (large-angle Rutherford
c  scattering regime) through the so-called plural scattering region.
      F=0.98 ! probability cutoff in definition of sigma2MS
      density=3.534 ! g/cm^3
      chi2cc=(0.39612e-2)**2*(Z*(Z+1))*(density/12) ! GeV^2/m
      chi2c=chi2cc*(tt/E**2)
      rBohr=0.52917721e-10 ! m
      chi2alpha=1.13*(hbarc/(E*rBohr*0.885))**2
     +         *Z**(2/3.)*(1+3.34*(alpha*Z)**2)
      omega0=chi2c/(1.167*chi2alpha) ! mean number of scatters
      gnu=omega0/(2*(1-F))
      sigma2MS_Geant=chi2c/(1+F**2)*((1+gnu)/gnu*log(1+gnu)-1)
      end

      real function sigma2MS_Hanson(tt)
      real tt
      include 'cobrems.inc'
C--Formulation of the rms projected angle attributed to Hanson et.al.
c  in reference Phys.Rev. vol.3 no.2, (1958), p.647.  This is just Moliere
c  theory used to give the 1/e angular width of the scattering distribution.
c  In the paper, though, they compare it with experiment for a variety of
c  metal foils down to 1e-4 rad.len. in thickness, and show excellent
c  agreement with the gaussian approximation out to 4 sigma or so.  I
c  like this paper because of the excellent agreement between the theory
c  and experimental data.
      density=3.534 ! g/cm^3
      ttingcm=tt*100*density
      Atomicweight=12
      EinMeV=E*1000
      theta2max=0.157*Z*(Z+1)/Atomicweight*(ttingcm/EinMeV**2)
      theta2screen=theta2max*Atomicweight*(1+3.35*(Z*alpha)**2)
     +            /(7800*(Z+1)*Z**(1/3.)*ttingcm)
      BminuslogB=log(theta2max/theta2screen)-0.154
      Blast=1
      do i=1,999
        B=BminuslogB+log(Blast)
        if (B.lt.1.2) then
          B=1.21
          goto 10
        elseif (abs(B-Blast).gt.1e-6) then
          Blast=B
        else
          goto 10
        endif
      enddo
   10 continue
      sigma2MS_Hanson=theta2max*(B-1.2)/2
      end
