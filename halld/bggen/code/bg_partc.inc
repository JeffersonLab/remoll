C
C---  Particle masses
C
      INTEGER MXPART
      PARAMETER (MXPART=100)
      COMMON/BG_PARTC/ AM_PART(MXPART),WG_PART(MXPART)
     +                ,KD_PART(4,MXPART)
      REAL AM_PART      !  (i) - particle mass GeV/c^2, i - GEANT number
     +    ,WG_PART      ! full widths
     +    ,KD_PART      ! (1-3) decay products (one decay allowed)
C                       ! (4) =0 -  decay uniform in theta, =1 - like rho (sin**2), =2 - J/Psi-type
C

 
