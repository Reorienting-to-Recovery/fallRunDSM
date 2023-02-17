# Reorienting to Recovery Use of and Adaptations to the Fall Chinook Salmon Science Integration Team Model


# SIT DSM Model: 
Please refer to [SIT model repository](https://github.com/CVPIA-OSC/fallRunDSM) to see full documenation on the SIT DSM model. 

# Reorienting to Recovery 

## Usage

### Package Installation

The `fallRunDSM` package depends on a number of packages developed by the [CVPIA Open Science Collaborative](https://github.com/CVPIA-OSC) and additional packages developed by the CVPIA SIT and adapted for the Reorienting to Recovery process. To install `fallRunDSM`, additional `CVPIA-OSC` packages, and `Reorienting-to-Recovery` packages use the `remotes::install_github()` function.

``` r
# install.packages("remotes")
remotes::install_github("Reorienting-to-Recovery/fallRunDSM")
remotes::install_github("CVPIA-OSC/DSMscenario") # TODO update once we have a R2R scenario package

# optional - need if calibrating model
remotes::install_github("CVPIA-OSC/DSMCalibrationData")

# optional - need if wanting to explore or modify flow, habitat, and temperature inputs
remotes::install_github("CVPIA-OSC/DSMflow")
remotes::install_github("Reorienting-to-Recovery/DSMhabitat")
remotes::install_github("CVPIA-OSC/DSMtemperature")
```

### Run Model

The `fall_run_model()` is a Fall Run Chinook life cycle model origionally developed and used for [CVPIA's Structured Decision Making Process](http://cvpia.scienceintegrationteam.com/). This repository is focused on using the model to model Reorienting to Recovery performance metrics. Running the model simulates Fall Run Chinook population dynamics across 31 watersheds in California over a 20 year period.

The following code runs the fall run model with Reorienting to Recovery Baseline inputs:

``` r
# seed the model
fall_run_seeds <- fall_run_model(mode = "seed",
                                 params = r_to_r_baseline_params)

# run the 20 year simulation
results <- fall_run_model(mode = "simulate",
                          seeds = fall_run_seeds,
                          params = r_to_r_baseline_params)
```

## Details on Supporting Data

### Dependencies

The `fallRunDSM` package uses data from several other packages within the [CVPIA Open Science Collaborative](https://github.com/CVPIA-OSC). These relationships are visualized in the dependency graph below.

<img src="man/figures/dependencyChain.svg" width="100%"/>

### Flow, Habitat, and Temperature Data

All data used in the `fallRunDSM` is passed in as a argument to `fall_run_model()` from a params data list option that is composed of data objects from the following packages:

-   **Flow Data**: View detailed documentation of flow data inputs at [DSMflow](https://cvpia-osc.github.io/DSMflow/). Flow inputs to the `fallRunDSM` are generated using CalSim 2 data.
-   **Habitat Data**: View detailed documentation of habitat data inputs at [DSMhabitat](https://cvpia-osc.github.io/DSMhabitat/). Modeling details for each stream can be viewed [here](https://cvpia-osc.github.io/DSMhabitat/reference/habitat_data.html#modeling-details-for-streams). Additional habitat modifications were completed as part of the Reorienting to Recovery project. See TODO - add habitat docs. 
-   **Temperature Data**: View detailed documentation of temperature data inputs at [DSMtemperature](https://cvpia-osc.github.io/DSMtemperature/). Modeling details for each stream can be viewed [here](https://cvpia-osc.github.io/DSMtemperature/reference/stream_temperature.html#watershed-modeling-details).


### Calibration Data

We prepared additional datasets in the `DSMCalibration` package for model calibration:

1.  [GrandTab](https://wildlife.ca.gov/Conservation/Fishes/Chinook-Salmon/Anadromous-Assessment) estimated escapement data for the years 1998-2017. The GrandTab data is prepared as `DSMCalibrationData::grandtab_observed` and is used to measure the difference between model predictions and observed escapements. Grandtab data is additionally prepared as `DSMCalibrationData::grandtab_imputed` and is used to calculate the number of juveniles during the 20 year simulation.

2.  Proxy years are used to select Habitat, Flow, and Temperature data for 1998-2017 to correspond with the years of GrandTab escapement data. The data inputs to the DSM are for years 1980-1999. We selected proxy years for 1998-2017 from the 1980-1999 model inputs by [comparing the DWR water year indices](https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST).

For a detailed overview of the calibration process see the [calibration markdown.](https://cvpia-osc.github.io/fallRunDSM/articles/calibration-2021.html)

