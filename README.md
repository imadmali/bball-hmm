
# Tagging Basketball Events with HMM

This case study shows how to implement Hidden Markov Models (HMM) in Stan using basketball player tracking data for the purpose of tagging events. The document `doc/hmm.html` discusses what a HMM is, how we fit HMMs in Stan, and how we apply this class of models to basketball data. We used a small sample of historical NBA player tracking data (see https://github.com/sealneaward/nba-movement-data for the raw data).

The workflow is separated into three parts:
1. Example HMM
2. Tagging a Drive Event
3. Defensive Assignment

An outline of the project directory is provided below.

* `hmm_example_fit.R`
* `hmm_example_bad_fit.R`
* `drive_0.R`
* `drive_1.R`
* `defense_0a.R`
* `defense_0b.R`
* `defense_2_low_res.R`
* `defense_2_low_res.R`
* `graphics.R`
* `data/` data scripts and data files
* `doc/` rmarkdown documentation
* `media/` video clips and plot of the model output
* `models/` stan models
* `results/` stan model fits
