# Untitled Shiny app for the stepwise skipjack 2022 assessment

Finlay Scott, 14/04/2022

## Structure

Two folders:

* R - for code to explore and process the outputfiles into something useful for the app
* app - the files for the Shiny app


## Generating the data

Note that the app does a minimal amount of data processing. This is to keep it more responsive,
Instead of passing a load of hefty rep, par and tag files, the data for the plots is pregenerated.

The code for generating data files used by the app is  R/stepwise_app_data_preparation.R.
By sourcing that script a bunch of .Rdata files *should* appear in the app/data folder.

At the top of that script is a vector of models. By adjusting this, more models can potentially be added (if all the files are in the model folder).

### Files needed in each model folder

Each model folder should have the following files:

length.fit (for the catch size distribution plots), frq, rep, par, obsX and predX (for the catchability plots, where X is 1 to 31, or thereabouts)

## Notes on plots

* SB/SBF0 - possible to add more detailed region selector (not just all regions or combined). Could also add a 2012 line to help with TRP discussions?
* SB - possible to add region selector. At the moment all regions plus combined is shown. Also possible to add option so same scale across panels to see distribution of biomass across regions. 


## File location

MFCL work is being conducted here:
[https://github.com/PacificCommunity/ofp-sam-skj22/tree/main/stepwise2022][https://github.com/PacificCommunity/ofp-sam-skj22/tree/main/stepwise2022]

