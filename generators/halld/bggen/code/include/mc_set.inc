	common /common_mc_set/
     +  mcSet_EneBeam,
     +	mcSet_TarA,
     +	mcSet_TarZ,
     +	mcSet_Q2Min,
     +	mcSet_Q2Max,
     +	mcSet_YMin,
     +	mcSet_YMax,
     +  qedrad,
     +  Model,
     +  genSet_FStruct,
     +  genSet_R,
     +  mcSet_PTarget,
     +  mcSet_PBeam,
     +  mcSet_XMin,
     +  mcSet_XMax
	save /common_mc_set/

	integer
     +  qedrad,
     +  Model,
     +	mcSet_TarA,
     +	mcSet_TarZ

	real
     +  mcSet_EneBeam,
     +	mcSet_Q2Min,
     +	mcSet_Q2Max,
     +	mcSet_YMin,
     +	mcSet_YMax,
     +  mcSet_XMin,
     +  mcSet_XMax

	character*4
     +  genSet_FStruct,
     +  genSet_R,
     +  mcSet_PBeam,
     +  mcSet_PTarget

        common /mcevnt/
     +          weight,
     +          genq2, gennu, genx, geny, genw2,
     +          genthe, genphi, geneprim, genpprim,
     +          genpx, genpy, genpz,
     +          genvx, genvy, genvz
       save /mcevnt/

        real    weight,                                 ! event weight
     +          genq2, gennu, genx, geny, genw2,        ! vertex kinematics
     +          genthe, genphi, geneprim, genpprim,     ! scattered lepton
     +          genpx, genpy, genpz,                    ! scat lepton 3-vector
     +          genvx, genvy, genvz
