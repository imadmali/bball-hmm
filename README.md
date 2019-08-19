
# Tagging Basketball Events with HMM

This case study shows how to implement Hidden Markov Models (HMM) in Stan using basketball player tracking data for the purpose of tagging events. The document `doc/hmm.html` discusses what a HMM is, how we fit HMMs in Stan, and how we apply this class of models to basketball data. We used a small sample of historical NBA player tracking data (see https://github.com/sealneaward/nba-movement-data for the raw data).

The workflow is separated into three parts (or model types):
1. Example HMM
2. Tagging a Drive Event
3. Defensive Assignment

An outline of the project directory is provided below.

* `hmm_example_fit.R`
  * Simple HMM with 2 states and 1 output
* `hmm_example_bad_fit.R`
  * Above model without enforced ordering of parameters
* `drive_0.R`
  * Drive HMM with exponential likelihood
* `drive_1.R`
  * Drive HMM with normal likelihood
* `defense_0a.R`
  * Defensive assignment HMM example with fixed convex combination parameters
* `defense_0b.R`
  * Defensive assignment HMM example with estimated convex combination parameters
* `defense_1_low_res.R`
  * Defensive assignment HMM on player tracking data with estimated convex combination parameters
* `defense_2_low_res.R`
  * Defensive assignment HMM on player tracking data with fixed convex combination parameters
* `graphics.R`
  * Functions for plotting court/players/etc
* `data/`
  * Data scripts and data files
* `doc/`
  * Documentation
* `media/`
  * Video clips and plots of model output
* `models/`
  * Stan models
* `results/`
  * Stan model fits
