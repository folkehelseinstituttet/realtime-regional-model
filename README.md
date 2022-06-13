# A real-time regional model for COVID-19: probabilistic situational awareness and forecasting

This repository contains code published together with the paper "A real-time regional model for COVID-19: probabilistic situational awareness and forecasting".

Code included in this repository:

1. Code to run the test of Split-ABC method (split_abc folder)
2. Code to calculate hospitalisation parameters (hosp_params folder)
3. Code used to compare forecasts (validation folder)

The implementation of the main transmission model can be found in: https://github.com/sykdomspulsen-org/spread. We have used the latest version of the asymmetric_mobility_se1e2iiar model implemeted there.

We do not own the data used for the modelling and we can therefore not share the data directly. Some data on national level hospitalisations and regional data on positive cases are available in https://github.com/folkehelseinstituttet/surveillance_data together with covid-19 surveillance data from Norway. For possible access to other data on hospitalisations or positive cases please contact the Norwegian Institute of Public Health and for access to mobility data contact Telenor.

## Code to run Split ABC method
This code has been run on a cluster using a slurm workload manager 

- toy_reg_presplit.R and toy_reg_postsplit.R : main calibration scripts
- run_model_fylkelevel.R: contains auxiliary functions
- util.R: contains auxiliary functions
- fylke_datasync.csv: synthetic hospitalisations data
- labDatasync.csv: synthetic test data
- sep_county_matrices.csv: contains the position of the changepoints
- expand_hospitaliation_jcl5_fylkelevel.R: hospitalization module that generates hospitalizations and positive tests from the incidence.
- se1e2iiar_pop.rds: Population per municipality

Other data needed (not provided):
- andelerPerDagFylke28PreDagersVinduWeightNational50.csv: Smoothed age distribution of the positive tests
- seeding.RDS: imported cases
- special_matrix_oct1.RDS: mobility data

In addition, the fylkes contained in the folder ./postsplit/previous/ are the initial conditions for the postsplit calibration:

Initial_states.RDS : Content of the different compartments at date= split date-15 for each of the “particles”/samples.
R17.txt: Calibration file of the presplit step.



## Code for hospitalisation parameters

## Code for validation of forecasts
The paper_evaluations.R file expects to receive the latest data and results from various calibrations at different time-points and then calculates CRPS and Energy scores and coverage of different prediction intervals. 