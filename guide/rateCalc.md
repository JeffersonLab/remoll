# Calculating rates/rate weighting

## What is rate?

### Møller/e+P elastic/other generator

Rate is a a number in Hz (but really in electrons * Hz) that quantifies the probability weighted (i.e. cross section) number of electrons at a given beam current for a given interaction.

### Beam generator

Rate is definitionally 1, since our primary electrons are generated prior to the target, and any interactions they experience are handled later in the simulation. Thus, with the beam generator, events that Møller scatter have probability baked in.

## Calculating rate for Møller generated electrons

I'm going to show this using an example. I ran 10M events with the LH2 target, and the GEMs as the active detectors. I split this run over 500 files.

![Rate-weighted r distribuition for detector 300 without z cut.[]{label="fig:view"}](pictures/rate_dist_no_zcut.png)

There's two ways to rate-weight counts

1. `hist->Fill(variable, rate)`
2. `Tree->Draw(variable, rate * cut)`

The second works because the TCut in tree draw is actually just a weight (i.e., if your cut is false it sets weight to zero and vice versa)

But we can’t simply take the integral of this histogram. The GEMs in remoll have boundary hits enabled, we don’t expect scattering or radiation in the gas mixture and see very few events like this, If we take the number of hits like this, we are double counting by a factor of 2 since we see one boundary hit at the entrance and one at the exit.

If we add a z cut on the entrance to the sensitive detector we get:

![Rate-weighted r distribuition for detector 300 with z cut.[]{label="fig:view"}](pictures/rate_dist_zcut.png)

We can now take the integral of this histogram. But what we get is still not the rate at this detector. 

$$\int\frac{dN_{300}}{dr}dr = 3.06e12$$

We still need to correct this number for a few things. Firstly, several files were summed together to make this distribution. Rate doesn't add between files, but rather has to be averaged between files. We need to divide not by the number of jobs we run but the number of jobs that finished and whose data was summed in this distribution. In this case, of the 500 jobs I ran, 477 finished all 20k events.

In the calculation of rate, the number of thrown events is put in at the beginning. Thus, if we have files where some fraction of events ran, the rate is off by $N_{\text{ran}}/N_{\text{thrown}}$. I discarded these jobs to avoid this problem. The beam current of the run is also implicit in the rate, so the number we get is the rate given 65 $\mu A$. Often we want the rate given 1 $\mu A$ to make conversions easier for whoever we sent these rates off to. Thus, our final rate is 

$$\text{Rate} = 3.06e12 \text{ Hz} / 477\text{ jobs} / 65 \mu A /(1e6 Hz/MHz)= \boxed{98.7 \frac{\text{MHz}}{1\mu A}}$$

This general prescription can be followed for any given sensitive detector with the appropriate cuts.

## Calculating rate for Beam generated electrons

The beam generator rate calculation is different from every other generator because one beam event corresponds to one primary beam electron, so no probability weighting by cross section is necessary.

![Rate-weighted x/y distribuition for detector 300 with z cut and beam generator.[]{label="fig:view"}](pictures/rate_dist_beam.png)

It's important here to mention that trid/mtrid cuts should NOT be used with the beam generator. Bremmstrahlung/radiation changes trid in Geant4 meaning a cut on mtrid/trid will lose the Møller scattered electrons since most electrons will radiate prior to scattering.

Following the same procedure above, I get the following distribution. (Again with a z cut). This is from 5000 jobs running 100M events (20k per job). I had 5 jobs fail so I removed those giving me 99.92M events that were actually processed. 1 uA beam corresponds to 6.24e12 electrons/second. Now it’s just solving the following ratio.

$$N_{\text{det in 1s}} = \frac{N_{\text{det simulated}} N_{1\mu A\times s}}{N_{\text{ev simulated}}}$$

$N_{1\mu A\times s}$ is the number of electrons that correspond to 1 second of beam given 1 $\mu A$ beam current. 1 uA of beam current corresponds to 6.12e12 primary electrons/second.

$N_{\text{det simulated}}$ is the number of electrons that are measured in the given detector over all simulated events. Remember that rate is 1 for beam generated electrons so the integral = entries, i.e. 1938 electrons for the above distribution.

$N_{\text{ev simulated}}$ is the number of simulated events. In this case 99.2e6.

Putting this together, we get:

$$\text{Rate} = 1938*6.24e12 / 99.92e6 = \boxed{121.0 \frac{\text{MHz}}{1\mu A}}$$

It's higher than the Møller rate because the beam generator also includes all backgrounds and the e+P elastic events.
