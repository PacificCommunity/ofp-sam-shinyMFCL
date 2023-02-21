#------------------------------------------------------------------
# Preparing the data for the YFT review 2022 app.
# Basic approach is to go through the 'basedir' model folder
# and hoover out the data we need.
#------------------------------------------------------------------

# Packages
# It may be advisable to use a version of FLR4MFCL that matches the MFCL runs,
# e.g. to ensure that the movement matrices are being read in correctly (from / to)
library(FLR4MFCL)
library(data.table)

# Helper functions
source("find_biggest.R")
source("read_length_fit_file.R")

# Model folder
basedir <- "//penguin/assessments/bet/2023/model_runs/stepwise/"
tagfile <- "bet.tag"
frqfile <- "bet.frq"

# Fisheries
index_fisheries <- 33:41

# Output folder
dir.create("../app/data", showWarnings=FALSE)

# Generate the fishery map
source("fishery_map.R")
# Load the fishery map - assumed to be the same for all models
load("../app/data/fishery_map.Rdata")

models <- dir(basedir)
models <- models[models!="12_Age10LW_HopefulReport"]  # exclude this one
models <- models[models!="12_Age10LW_Mixed"]          # exclude this one

# Model description
model_description <- data.frame(
  model=models,
  model_description=c(
    "before important model changes are made",
    "ungroup fleet selectivities between regions",
    "add JPTP tagging data, change mixing period to 2 quarters",
    "increase max age from 7 to 10 years",
    "add otolith data, deterministic von Bertalanffy for all ages",
    "maturity at length converted to age using smooth splines",
    "spawning fraction removed from the calculation of reproductive potential",
    "use size composition downweighting divisor of 60",
    "set selectivity at < 30 cm to zero for purse seine and pole-and-line"
  ))

#------------------------------------------------------------------
# Data checker
# Each model folder needs to have the following files:
# length.fit
# *.frq
# *.tag
# *.par
# *.rep
# temporary_tag_report
# (only catch conditioned models have the obsX and predX files)

needed_files <- c(tagfile, "length.fit", "temporary_tag_report", frqfile, "test_plot_output")
for (model in models){
  model_files <- list.files(paste0(basedir, model))
  # Also check for a par and rep
  parfiles <- model_files[grep(".par$", model_files)]
  if(length(parfiles) == 0){
    cat("Missing par file in model", model, ". Dropping model.\n")
    models <- models[!(models %in% model)]
  }
  repfiles <- model_files[grep("par.rep$", model_files)]
  if(length(repfiles) == 0){
    cat("Missing rep file in model", model, ". Dropping model.\n")
    models <- models[!(models %in% model)]
  }
  if(!all(needed_files %in% model_files)){
    missing_file <- needed_files[!(needed_files %in% model_files)]
    cat("Missing files in model", model, ":", missing_file, ". Dropping model.\n")
    models <- models[!(models %in% model)]
  }
}

#------------------------------------------------------------------
# Data for catch size distribution plots
# This involves going through the length.fit files and processing the data
# The function to read and process the data is here:

cat("** Catch size distribution stuff\n")
lfits_dat <- lapply(models, function(x){
  cat("Processing model: ", x, "\n")
  filename <- paste(basedir, x, "length.fit", sep="/")
  read_length_fit_file(filename=filename, model_name=x)}
  )
lfits_dat <- rbindlist(lfits_dat)
# Bring in the fishery map
lfits_dat <- merge(lfits_dat, fishery_map)
# Bring in the model description - might not need
lfits_dat <- merge(lfits_dat, model_description, by="model")

# Save it in the app data directory
save(lfits_dat, file="../app/data/lfits_dat.Rdata")

#------------------------------------------------------------------
# Movement
# We want to extract the diff_coffs_age_period from a par object.
# Par objects are big, so it is easier to read in a bit of the par object (MFCLRegion).
# Thanks object-oriented programming and overloading!

cat("\n** Movement stuff\n")
move_coef <- list()
for (model in models){
  cat("Model: ", model, "\n")
  biggest_par <- find_biggest_par(file.path(basedir, model))
  reg <- read.MFCLRegion(biggest_par)
  dcap <- diff_coffs_age_period(reg)
  move_coef[[eval(model)]] <- as.data.table(dcap)
}

move_coef <- rbindlist(move_coef, idcol="model")
move_coef$age <- as.numeric(move_coef$age)
# Tidy up
move_coef$move <- paste("From R", move_coef$from, " to R", move_coef$to, sep="")
move_coef$from <- paste("R", move_coef$from, sep="")
move_coef$to <- paste("R", move_coef$to, sep="")
move_coef$Season <- as.numeric(move_coef$period)
setnames(move_coef, old=c("age", "to", "from"), new=c("Age", "To", "From"))

move_coef <- merge(move_coef, model_description)

# This could probably be reduced because the movement is not age-structured (so far)
save(move_coef, file="../app/data/move_coef.Rdata")

#------------------------------------------------------------------
# General stuff including stock recruitment, SB and SBSBF0 data.

cat("\n** General stuff\n")

srr_dat <- list()
srr_fit_dat <- list()
rec_dev_dat <- list()
biomass_dat <- list()
sel_dat <- list()
m_dat <- list()
mat_age_dat <- list()
mat_length_dat <- list()
cpue_dat <- list()
status_tab_dat <- list()

for (model in models){
  cat("Model: ", model, "\n")
  biggest_rep <- find_biggest_rep(file.path(basedir, model))
  rep <- read.MFCLRep(biggest_rep)

  # SRR stuff
  adult_biomass <- as.data.table(adultBiomass(rep))[, c("year", "season", "area", "value")]
  recruitment <- as.data.table(popN(rep)[1,])[, c("year", "season", "area", "value")]
  setnames(adult_biomass, "value", "sb")
  setnames(recruitment, "value", "rec")
  pdat <- merge(adult_biomass, recruitment)
  pdat[, c("year", "season") := .(as.numeric(year), as.numeric(season))]
  srr_dat[[model]] <- pdat

  # Get the BH fit
  # Need to pick a suitable max SB
  # Sum over areas and assume annualised (mean over years)
  # pdattemp <- pdat[, .(sb=sum(sb)), by=.(year, season)]
  # pdattemp <- pdattemp[, .(sb=mean(sb)), by=.(year)]
  # max_sb <- max(pdattemp$sb) * 1.2 # Just add another 20% on
  max_sb <- 20e6 # Just pick a massive number and then trim using limits
  sb <- seq(0, max_sb, length=100)
  # Extract the BH params and make data.frame of predicted recruitment
  # Note that this is predicted ANNUAL recruitment, given a SEASONAL SB
  # The data in the popN that we take recruitment from is SEASONAL
  # There is then some distribution
  params <- c(srr(rep)[c("a", "b")])
  bhdat <- data.frame(sb=sb, rec=(sb*params[1]) / (params[2]+sb))
  srr_fit_dat[[model]] <- bhdat

  # Get the rec devs
  biggest_par <- find_biggest_par(file.path(basedir, model))
  par <- read.MFCLPar(biggest_par)
  rdat <- as.data.table(region_rec_var(par))[, c("year", "season", "area", "value")]
  rdat[, c("year", "season") := .(as.numeric(year), as.numeric(season))]
  rdat[, "ts" := .(year + (season-1)/4 + 1/8)]
  rec_dev_dat[[model]] <- rdat

  # Get SBSBF0 and SB - mean over seasons
  sbsbf0_region <- as.data.table(SBSBF0(rep, combine_areas=FALSE))
  sbsbf0_all <- as.data.table(SBSBF0(rep, combine_areas=TRUE))
  sbsbf0dat <- rbindlist(list(sbsbf0_region, sbsbf0_all))
  setnames(sbsbf0dat, "value", "SBSBF0")

  sb_region <- as.data.table(SB(rep, combine_areas=FALSE))
  sb_all <- as.data.table(SB(rep, combine_areas=TRUE))
  sbdat <- rbindlist(list(sb_region, sb_all))
  setnames(sbdat, "value", "SB")

  sbf0_region <- as.data.table(SBF0(rep, combine_areas=FALSE))
  sbf0_all <- as.data.table(SBF0(rep, combine_areas=TRUE))
  sbf0dat <- rbindlist(list(sbf0_region, sbf0_all))
  setnames(sbf0dat, "value", "SBF0")

  sbdat <- data.table(sbdat, SBF0=sbf0dat$SBF0, SBSBF0=sbsbf0dat$SBSBF0)
  sbdat <- sbdat[, c("year","area","SB","SBF0","SBSBF0")]
  sbdat[area=="unique", area := "All"] # change in place - data.table for the win
  sbdat[, year := as.numeric(year)]
  biomass_dat[[model]] <- sbdat

  # Selectivity by age class (in quarters)
  sel <- as.data.table(sel(rep))[, .(age, unit, value)]
  sel[, c("age", "unit") := .(as.numeric(age), as.numeric(unit))]
  setnames(sel, "unit", "fishery")
  # Bring in lengths
  mean_laa <- c(aperm(mean_laa(rep), c(4,1,2,3,5,6)))
  sd_laa <- c(aperm(sd_laa(rep), c(4,1,2,3,5,6)))
  # Order sel for consecutive ages
  setorder(sel, fishery, age)
  nfisheries <- length(unique(sel$fishery))
  sel$length <- rep(mean_laa, nfisheries)
  sel$sd_length <- rep(sd_laa, nfisheries)
  sel[, c("length_upper", "length_lower") := .(length + 1.96*sd_length, length - 1.96*sd_length)]
  sel_dat[[model]] <- sel

  # Natural mortality
  m <- m_at_age(rep)
  m <- data.table(age=1:length(m), m=m)
  m_dat[[model]] <- m

  # Maturity in the par files
  # Needs the lfits_dat to have been generated above
  modeltemp <- model
  lfittemp <- lfits_dat[model == modeltemp]
  lenbin <- sort(unique(lfittemp$length))
  # Length
  mat_length <- data.table(length = lenbin, mat = mat_at_length(par))
  mat_length_dat[[model]] <- mat_length
  # Age
  mat_age <- mat(par)
  mat_age <- data.table(age = 1:length(mat_age), mat = mat_age)
  mat_age_dat[[model]] <- mat_age

  # CPUE obs and pred - noting that this information is only applicable for some models
  cpue <- as.data.table(cpue_obs(rep))
  cpue_pred <- as.data.table(cpue_pred(rep))
  setnames(cpue, "value", "cpue_obs")
  cpue[, cpue_pred := cpue_pred$value]
  setnames(cpue, "unit", "fishery")
  cpue[, ts := .(as.numeric(year) + (as.numeric(season)-1)/4)]
  # Trim out only the index fisheries
  cpue <- cpue[fishery %in% index_fisheries]
  cpue[, fishery := as.numeric(fishery)] # for merging with fishery_map
  # Transform by taking exp()
  cpue[, c("cpue_obs", "cpue_pred") := .(exp(cpue_obs), exp(cpue_pred))]
  cpue_dat[[model]] <- cpue

  # Summary table
  sbsbf0latest <- c(SBSBF0latest(rep))
  status_tab <- data.table(
    "Final SB/SBF0latest" = sbsbf0latest[length(sbsbf0latest)],
    "SB/SBF0 (2012)" = c(SBSBF0(rep)[,"2012"]),
    MSY = MSY(rep),
    BMSY=BMSY(rep),
    FMSY=FMSY(rep))
  status_tab_dat[[model]] <- status_tab
}

srr_dat <- rbindlist(srr_dat, idcol="model")
srr_fit_dat <- rbindlist(srr_fit_dat, idcol="model")
rec_dev_dat <- rbindlist(rec_dev_dat, idcol="model")
biomass_dat <- rbindlist(biomass_dat, idcol="model")
m_dat <- rbindlist(m_dat, idcol="model")
mat_age_dat <- rbindlist(mat_age_dat, idcol="model")
mat_length_dat <- rbindlist(mat_length_dat, idcol="model")
sel_dat <- rbindlist(sel_dat, idcol="model")
sel_dat <- merge(sel_dat, fishery_map)
cpue_dat <- rbindlist(cpue_dat, idcol="model")
status_tab_dat <- rbindlist(status_tab_dat, idcol="Model")

# Look at difference - better for evaluating fit?
# Don't do this for the annual data
cpue_dat[, diff := .(cpue_obs - cpue_pred)]
# Scale by total catchability by fishery and model
cpue_dat[, scale_diff := diff / mean(cpue_obs, na.rm=TRUE),
         by=.(model, fishery)]

save(status_tab_dat, cpue_dat, mat_age_dat, mat_length_dat, biomass_dat, srr_dat, srr_fit_dat, rec_dev_dat, sel_dat, m_dat,
     file="../app/data/other_data.Rdata")

#-----------------------------------

# Likelihood table
cat("\n** Likelihood table\n")
ll_tab_dat <- list()
for (model in models){
  cat("Model: ", model, "\n")
  # Load the likelihood and par files
  ll <- read.MFCLLikelihood(paste(basedir, model, "test_plot_output", sep="/"))
  biggest_par <- find_biggest_par(file.path(basedir, model))
  par <- read.MFCLPar(biggest_par)
  # Get LL summary
  ll_summary <- summary(ll)
  # Build data.table with correct names
  lldf <- data.table(
    "BH steepness" = subset(ll_summary, component=="bhsteep")$likelihood,
    "Effort devs" = subset(ll_summary, component=="effort_dev")$likelihood,
    "Catchability devs" = subset(ll_summary, component=="catchability_dev")$likelihood,
    "Length comp." = subset(ll_summary, component=="length_comp")$likelihood,
    "Weight comp." = subset(ll_summary, component=="weight_comp")$likelihood,
    "Tag data" = subset(ll_summary, component=="tag_data")$likelihood,
    "Total" = subset(ll_summary, component=="total")$likelihood,
    "Max. gradient" = max_grad(par),
    "No. parameters" = n_pars(par)
  )
  ll_tab_dat[[model]] <- lldf
}

ll_tab_dat <- rbindlist(ll_tab_dat, idcol="Model")

save(ll_tab_dat, file="../app/data/ll_tab_data.Rdata")

#-----------------------------------
# Tag plot data - complicated

cat("\n** Tagging stuff\n")
tagrep_dat <- list()
for (model in models){
  cat("Model: ", model, "\n")
  biggest_par <- find_biggest_par(file.path(basedir, model))
  par <- read.MFCLPar(biggest_par)

  # Tag releases from the *.tag file
  # The recaptures slot contains the observed recaptures but not used here
  # We use temporary_tag_report file which has the predicted and observed recaptures
  tagobs <- read.MFCLTag(paste(basedir, model, "/", tagfile, sep=""))
  tag_releases <- data.table(releases(tagobs))
  # Summarise release numbers by release event, i.e. sum the length distributions
  tag_releases <- tag_releases[, .(rel.obs = sum(lendist, na.rm=TRUE)),
                               by=.(program, rel.group, region, year, month)]
  setnames(tag_releases, c("region", "year", "month"), c("rel.region", "rel.year", "rel.month"))
  # Bring in the mixing period - needs a par file
  tag_releases$mixing_period <- flagval(par, (-10000 - tag_releases$rel.group + 1),1)$value
  # What is the mixing period in terms of years?
  no_seasons <- dimensions(par)["seasons"]
  tag_releases$mixing_period_years <- tag_releases$mixing_period / no_seasons
  # Add a time step
  tag_releases$rel.ts <- tag_releases$rel.year + (tag_releases$rel.month-1)/12 + 1/24
  # setorder(tag_releases, rel.group, rel.ts)

  # Temporary tag report
  # Includes the predicted tag recoveries disaggregated to a very low level, release and recapture
  tagrep <- read.temporary_tag_report(paste(basedir, model, "/temporary_tag_report", sep=""))
  tagrep <- data.table(tagrep)

  # Bring in recapture fishery and region
  fm2 <- fishery_map
  colnames(fm2)[colnames(fm2) == "fishery"] <- "recap.fishery"
  tagrep <- merge(tagrep, fm2[, c("recap.fishery", "region", "tag_recapture_group", "tag_recapture_name")])
  tagrep$recap.ts <- tagrep$recap.year + (tagrep$recap.month-1)/12 + 1/24

  # Bring in tagging program, and rel.ts from the tag release data (from the skj.tag file)
  # tag_releases has one row for each tag release group (269 of them) giving the region, year and month of that release
  # tagrep <- merge(tagrep, tag_releases[, c("rel.group","program", "rel.ts")], by="rel.group")
  # Potentially drop some columns here
  tagrep <- merge(tagrep, tag_releases, by="rel.group")
  # There are a lot of columns that maybe we don't need here
  # Add period at liberty
  tagrep[,"period_at_liberty" := ((recap.ts - rel.ts) * no_seasons)]
  tagrep[, rel.ts.after.mix := rel.ts + mixing_period_years]

  # Drop observations that are within the mixing period
  tagrep <- tagrep[!(recap.ts < rel.ts.after.mix),]

  # Summarise the three plots - or do it at end - might need to do it at the end as number of models increases
  tagrep_dat[[model]] <- tagrep
}

tagrep_dat <- rbindlist(tagrep_dat, idcol="model")

# Data for tag returns by time plot
# Summarise returns by recapture group
tag_returns_time <- tagrep_dat[, .(recap.pred = sum(recap.pred, na.rm=TRUE), recap.obs = sum(recap.obs, na.rm=TRUE)),
                               by=.(tag_recapture_group, tag_recapture_name, recap.ts, model)]
# To ensure plotting is OK we need each fishery to have a full complement of time series
padts <- expand.grid(recap.ts = seq(from=min(tag_returns_time$recap.ts), to=max(tag_returns_time$recap.ts), by=1/no_seasons),
                     tag_recapture_name = sort(unique(tag_returns_time$tag_recapture_name)),
                     model = sort(unique(tag_returns_time$model)))
padts <- merge(padts, fishery_map[c("tag_recapture_group", "tag_recapture_name")])
tag_returns_time <- merge(tag_returns_time, padts, all=TRUE)
# Any NAs need to set to 0
tag_returns_time[is.na(tag_returns_time$recap.pred), "recap.pred" := 0]
tag_returns_time[is.na(tag_returns_time$recap.obs), "recap.obs" :=0 ]
# Get scaled diff for residuals plot
tag_returns_time[, "diff":= recap.obs - recap.pred]

tag_returns_time <- tag_returns_time[, .(recap.obs, recap.pred, model=model, recap.ts=recap.ts, diff = diff / mean(recap.obs, na.rm=TRUE)),
                                     by=.(tag_recapture_name, tag_recapture_group)]

# Data for attrition plot
tagrep_dat[, "diff" := recap.obs - recap.pred]
tag_attrition <- tagrep_dat[, .(recap.obs=sum(recap.obs, na.rm=TRUE), recap.pred=sum(recap.pred, na.rm=TRUE), diff = sum(diff, na.rm=TRUE)),
                            by=.(model, period_at_liberty, region, program)]
# Need to pad out time series to avoid missing missing periods at liberty
padts <- expand.grid(period_at_liberty = seq(from=min(tag_attrition$period_at_liberty), to=max(tag_attrition$period_at_liberty), by=1),
                     program = sort(unique(tag_attrition$program)),
                     region = sort(unique(tag_attrition$region)))
tag_attrition <- merge(tag_attrition, padts, by=colnames(padts), all=TRUE)
# 1. Number of tag returns (y) against period at liberty
# For the observed and predicted recaptures, NA is essentially 0, i.e. there were no recaptures, so set to 0
tag_attrition[is.na(recap.pred), recap_pred := 0]
tag_attrition[is.na(recap.obs), recap_obs := 0]
tag_attrition[is.na(diff), diff := 0]

# Data for tag return proportions
tag_returns_prop <- tagrep_dat[, .(recap.pred = sum(recap.pred, na.rm=TRUE), recap.obs=sum(recap.obs, na.rm=TRUE)),
                               by=.(model, rel.region, region, recap.month)]
tag_returns_prop_sum <- tagrep_dat[, .(recap.pred.sum = sum(recap.pred, na.rm=TRUE), recap.obs.sum=sum(recap.obs, na.rm=TRUE)),
                                   by=.(rel.region, recap.month)]
# Merge together and get the proportion of recaptures,
# i.e. the total number of tags from region 1 that were recaptured, found out the proportion that was recaptured in each region
tag_returns_prop <- merge(tag_returns_prop, tag_returns_prop_sum)
tag_returns_prop[, c("pred.prop", "obs.prop") := .(recap.pred/recap.pred.sum, recap.obs/recap.obs.sum)]
# Plot the difference between predicted and observed proportion of tags returned by region of release
tag_returns_prop[, "diff_prop" := obs.prop - pred.prop]
tag_returns_prop[, "rel.region.name" := paste("Release region ", rel.region, sep="")]
tag_returns_prop[, "Quarter" := as.factor((recap.month+1) / 3)]

save(tag_returns_time, tag_attrition, tag_returns_prop,
     file="../app/data/tag_data.Rdata")
