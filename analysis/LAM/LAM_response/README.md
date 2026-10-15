# LAM Response Function
This repository contains ROOT macros and LAM lookup tables necessary skim remoll outputs and create a rootfile with a new branch corresponding to the mean PEs for a given event location on the LAM for one of the two PMTs.
---
## Background
Previous versions of these scripts were used to create inputs for optical simulation (QSim). The results from those optical simulations are summarized in the lookup tables here.
---
## Usage
'reroot -l -b -q "skim_and_add_PE.C"'
Edit lines listed at the top of 'skim_and_add_PE.C' as necessary before running the above command.
The output rootfile does not follow the same structure as the original. Rather, it is as follows:
>   T
>
>   |-> x
>
>   |-> y
>
>   |-> p
>   |-> px
>   |-> py
>   |-> pz
>   |-> energy
>   |-> det
>   |-> pid
>   |-> trid
>   |-> rate
>   |-> A
>   |-> Am
>   |-> PE

Other branches can be added by editing 'skim_and_add_PE.C'. The output also collapses the azimuth to a single septant (beam left) to increase statistics and simplify the response map. Becasue the detector information is preserved, the original location of hits could be recovered by doing the inverse transform, but I believe it is easier to work with these particular data in the collapsed coorinates.

---
## Analysis
'plot_generators.C' is provided as an example analysis macro to analyze outputs from three physics generators simulatneously. It plots the response-rate-weigthed histograms for each branch and the sum of the three generators, cutting for electrons and particles the enrgy>1 MeV. Additionally, it plots the 2D response-weighted rate map for the three generators and the sum with the same cuts.