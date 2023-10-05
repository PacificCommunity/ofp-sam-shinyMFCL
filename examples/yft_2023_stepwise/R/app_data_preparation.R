#------------------------------------------------------------------
# Preparing the data for the app.
# Basic approach is to go through the 'basedir' model folder
# and hoover out the data we need.
#------------------------------------------------------------------

# Packages
# It may be advisable to use a version of FLR4MFCL that matches the MFCL runs,
# to ensure that the movement matrices are being read in correctly (from / to)
library(FLR4MFCL)
library(data.table)

# Helper functions
source("read_length_fit_file.R")

# Model folder
basedir <- "//penguin/assessments/yft/2023/model_runs/stepwise_shortnames"
basedir <- "c:/x/yft/stepwise_shortnames"
tagfile <- "yft.tag"
frqfile <- "yft.frq"
age_lengthfile <- "yft.age_length"

# Specify models to plot
models <- dir(basedir)
models <- models[dir.exists(file.path(basedir, models))]

# Fisheries
index_fisheries <- 33:37  # harmless to include non-existent fisheries

# Output folder
dir.create("../app/data", showWarnings=FALSE)

# Generate the fishery map
source("fishery_map.R")
# Load the fishery map - assumed to be the same for all models
load("../app/data/fishery_map.RData")

#------------------------------------------------------------------
# Data checker
# Each model folder needs to have the following files:
# *.tag
# length.fit
# weight.fit
# temporary_tag_report
# *.frq
# test_plot_output
# *.par
# *.rep
# Optional:
# *.age_length

needed_files <- c(tagfile, "length.fit", "weight.fit", "temporary_tag_report",
                  frqfile, "test_plot_output")
for (model in models){
  model_files <- dir(file.path(basedir, model))
  # Also check for a par and rep
  parfiles <- model_files[grep(".par$", model_files)]
  if(length(parfiles) == 0){
    cat("Missing par file in model '", model, "'. Dropping model.\n", sep="")
    models <- models[!(models %in% model)]
  }
  repfiles <- model_files[grep("par.rep$", model_files)]
  if(length(repfiles) == 0){
    cat("Missing rep file in model '", model, "'. Dropping model.\n", sep="")
    models <- models[!(models %in% model)]
  }
  if(!all(needed_files %in% model_files)){
    missing_file <- needed_files[!(needed_files %in% model_files)]
    cat("Missing files in model '", model, "': ",
        paste(missing_file, collapse=", "), ". Dropping model.\n", sep="")
    models <- models[!(models %in% model)]
  }
}

#------------------------------------------------------------------
# Otolith data - read once instead of n times
# 'here' is an integer pointing to the first model dir containing *.age_length
here <- match(TRUE, file.exists(file.path(basedir, models, age_lengthfile)))
if(!is.na(here))
{
  cat("** Reading otoliths\n")
  cat("Processing", age_lengthfile, "... ")
  oto_dat <- read.MFCLALK(file.path(basedir, models[here], age_lengthfile),
                          file.path(basedir, models[here], "length.fit"))
  oto_dat <- ALK(oto_dat)
  oto_dat <- data.frame(year=rep(oto_dat$year, oto_dat$obs),
                        month=rep(oto_dat$month, oto_dat$obs),
                        fishery=rep(oto_dat$fishery, oto_dat$obs),
                        species=rep(oto_dat$species, oto_dat$obs),
                        age=rep(oto_dat$age, oto_dat$obs),
                        length=rep(oto_dat$length, oto_dat$obs))
  oto_dat <- type.convert(oto_dat, as.is=TRUE)
  save(oto_dat, file="../app/data/oto_dat.RData")
  cat("done\n\n")
}
# oto_dat.RData is only created if *.age_length was found in some model dir

#------------------------------------------------------------------
# Data for length composition plots
# This involves going through the length.fit files and processing the data
# The function to read and process the data is here:

cat("** Length composition stuff\n")
lfits_dat <- lapply(models, function(x){
  cat("Processing model: ", x, "\n", sep="")
  filename <- file.path(basedir, x, "length.fit")
  read_length_fit_file(filename, model_name=x)})
lfits_dat <- rbindlist(lfits_dat)
# Bring in the fishery map
lfits_dat <- merge(lfits_dat, fishery_map)

# Save it in the app data directory
save(lfits_dat, file="../app/data/lfits_dat.RData")

#------------------------------------------------------------------
# Data for weight composition plots
# This involves going through the weight.fit files and processing the data
# The function to read and process the data is here:

cat("\n** Weight composition stuff\n")
wfits_dat <- lapply(models, function(x){
  cat("Processing model: ", x, "\n", sep="")
  filename <- file.path(basedir, x, "weight.fit")
  read_length_fit_file(filename, model_name=x)})
wfits_dat <- rbindlist(wfits_dat)
# Bring in the fishery map
wfits_dat <- merge(wfits_dat, fishery_map)

# Save it in the app data directory
save(wfits_dat, file="../app/data/wfits_dat.RData")

#------------------------------------------------------------------
# Movement
# We want to extract the diff_coffs_age_period from a par object.
# Par objects are big, so it is easier to read in a bit of the par object
# (MFCLRegion).
# Thanks object-oriented programming and overloading!

cat("\n** Movement stuff\n")
move_coef <- list()
for (model in models){
  cat("Model: ", model, "\n", sep="")
  final_par <- finalPar(file.path(basedir, model))
  first_year <- firstYear(file.path(basedir, model))
  reg <- read.MFCLRegion(final_par, first.yr=first_year)
  dcap <- diff_coffs_age_period(reg)
  move_coef[[eval(model)]] <- as.data.table(dcap)
}

move_coef <- rbindlist(move_coef, idcol="model")
move_coef$age <- as.numeric(move_coef$age)
# Tidy up
move_coef$move <- paste0("From R", move_coef$from, " to R", move_coef$to)
move_coef$from <- paste0("R", move_coef$from)
move_coef$to <- paste0("R", move_coef$to)
move_coef$Season <- as.numeric(move_coef$period)
setnames(move_coef, old=c("age", "to", "from"), new=c("Age", "To", "From"))

# This could probably be reduced because
# the movement is not age-structured (so far)
save(move_coef, file="../app/data/move_coef.RData")

#------------------------------------------------------------------
# General stuff including stock recruitment, SB and SBSBF0 data.

cat("\n** General stuff\n")
srr_dat <- list()
srr_fit_dat <- list()
rec_dev_dat <- list()
biomass_dat <- list()
sel_dat <- list()
growth_dat <- list()
m_dat <- list()
mat_age_dat <- list()
mat_length_dat <- list()
cpue_dat <- list()
status_tab_dat <- list()

for (model in models){
  cat("Model: ", model, "\n", sep="")
  final_rep <- finalRep(file.path(basedir, model))
  rep <- read.MFCLRep(final_rep)

  # SRR stuff
  adult_biomass <- as.data.table(
    adultBiomass(rep))[, c("year", "season", "area", "value")]
  recruitment <- as.data.table(
    popN(rep)[1,])[, c("year", "season", "area", "value")]
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
  final_par <- finalPar(file.path(basedir, model))
  first_year <- firstYear(file.path(basedir, model))
  par <- read.MFCLPar(final_par, first.yr=first_year)
  rdat <- as.data.table(
    region_rec_var(par))[, c("year", "season", "area", "value")]
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
  sbdat[area=="unique", area := "All"] # change in place, data.table for the win
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
  sel[, c("length_upper", "length_lower") :=
          .(length + 1.96*sd_length, length - 1.96*sd_length)]
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

  # CPUE obs and pred - noting that this information is
  # only applicable for some models
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
  sbsbf0 <- as.numeric(SBSBF0(rep))
  sbsbf0recent <- as.numeric(SBSBF0recent(rep))
  status_tab <- data.table(
    "Final SB/SBF0instant" = tail(sbsbf0, 1),
    "Final SB/SBF0recent" = tail(sbsbf0recent, 1),
    "SB/SBF0 (2012)" = as.numeric(SBSBF0(rep)[,"2012"]),
    MSY = MSY(rep),
    BMSY = BMSY(rep),
    FMSY = FMSY(rep))
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

save(status_tab_dat, cpue_dat, mat_age_dat, mat_length_dat,
     biomass_dat, srr_dat, srr_fit_dat, rec_dev_dat, sel_dat, m_dat,
     file="../app/data/other_data.RData")

#-----------------------------------

# Likelihood table
cat("\n** Likelihood table\n")
ll_tab_dat <- list()
for (model in models){
  cat("Model: ", model, "\n", sep="")
  # Load the likelihood and par files
  ll <- read.MFCLLikelihood(file.path(basedir, model, "test_plot_output"))
  final_par <- finalPar(file.path(basedir, model))
  first_year <- firstYear(file.path(basedir, model))
  par <- read.MFCLParBits(final_par, first.yr=first_year)  # ParBits is fast
  # Get LL summary
  ll_summary <- summary(ll)
  row.names(ll_summary) <- ll_summary$component
  # Build data.table with correct names
  lldf <- data.table(
    Npar = n_pars(par),
    ObjFun = obj_fun(par),
    CPUE = ll_summary["cpue", "likelihood"],
    Length = ll_summary["length_comp", "likelihood"],
    Weight = ll_summary["weight_comp", "likelihood"],
    Age = ll_summary["age", "likelihood"],
    Tags = ll_summary["tag_data", "likelihood"],
    Recruitment = ll_summary["bhsteep", "likelihood"],
    Effort_devs = ll_summary["effort_dev", "likelihood"],
    Catchability_devs = ll_summary["catchability_dev", "likelihood"],
    Total = ll_summary["total", "likelihood"],
    Penalties = NA_real_,  # calculate after this loop
    Gradient = max_grad(par)
  )
  # If the Shiny app includes results from models older than MFCL 2.1.0.0,
  # then we need to correct the ObjFun calculation
  final_rep <- finalRep(file.path(basedir, model))
  ver <- grep("MULTIFAN-CL version number", readLines(final_rep), value=TRUE)
  ver <- gsub(".*: ", "", ver)
  if(numeric_version(ver) < numeric_version("2.1.0.0"))
    lldf[, ObjFun := -ObjFun]  # before 2.1.0.0, the objfun was backwards
  ll_tab_dat[[model]] <- lldf
}

ll_tab_dat <- rbindlist(ll_tab_dat, idcol="Model")
ll_tab_dat[, Catchability_devs := NULL]     # all zeroes
ll_tab_dat[CPUE == 0, CPUE := Effort_devs]  # use Effort_devs when CPUE is 0
ll_tab_dat[, Effort_devs := NULL]
# Combine Recruitment with other Penalties
ll_tab_dat[, Penalties := ObjFun - Total + Recruitment]
ll_tab_dat[, Recruitment := NULL]
ll_tab_dat[, Total := NULL]  # intermediate calculations, includes Recruitment

save(ll_tab_dat, file="../app/data/ll_tab_data.RData")

#-----------------------------------
# Tag plot data - complicated

cat("\n** Tagging stuff\n")
tagrep_dat <- list()
for (model in models){
  cat("Model: ", model, "\n", sep="")
  final_par <- finalPar(file.path(basedir, model))
  first_year <- firstYear(file.path(basedir, model))
  par <- read.MFCLPar(final_par, first.yr=first_year)

  # Tag releases from the *.tag file
  # The recaptures slot contains the observed recaptures but not used here
  # We use temporary_tag_report file which has the
  # predicted and observed recaptures
  tagobs <- read.MFCLTag(file.path(basedir, model, tagfile))
  tag_releases <- data.table(releases(tagobs))
  # Summarise release numbers by release event, sum the length distributions
  tag_releases <- tag_releases[, .(rel.obs = sum(lendist, na.rm=TRUE)),
                               by=.(program, rel.group, region, year, month)]
  setnames(tag_releases, c("region", "year", "month"),
           c("rel.region", "rel.year", "rel.month"))
  # Bring in the mixing period - needs a par file
  tag_releases$mixing_period <- flagval(
    par, (-10000 - tag_releases$rel.group + 1),1)$value
  # What is the mixing period in terms of years?
  no_seasons <- dimensions(par)["seasons"]
  tag_releases$mixing_period_years <- tag_releases$mixing_period / no_seasons
  # Add a time step
  tag_releases$rel.ts <- tag_releases$rel.year +
    (tag_releases$rel.month-1)/12 + 1/24
  # setorder(tag_releases, rel.group, rel.ts)

  # Temporary tag report
  # Includes the predicted tag recoveries disaggregated to a very low level,
  # release and recapture
  first_year <- firstYear(file.path(basedir, model))
  tagrep <- read.temporary_tag_report(
    file.path(basedir, model, "temporary_tag_report"), year1=first_year)
  tagrep <- data.table(tagrep)

  # Bring in recapture fishery and region
  fm2 <- fishery_map
  colnames(fm2)[colnames(fm2) == "fishery"] <- "recap.fishery"
  tagrep <- merge(tagrep, fm2[, c("recap.fishery", "region",
                                  "tag_recapture_group", "tag_recapture_name")])
  tagrep$recap.ts <- tagrep$recap.year + (tagrep$recap.month-1)/12 + 1/24

  # Bring in tagging program, and rel.ts from the tag release data
  # (from the skj.tag file)
  # tag_releases has one row for each tag release group (269 of them) giving the
  # region, year and month of that release
  # tagrep <- merge(tagrep, tag_releases[, c("rel.group","program", "rel.ts")],
  #                 by="rel.group")
  # Potentially drop some columns here
  tagrep <- merge(tagrep, tag_releases, by="rel.group")
  # There are a lot of columns that maybe we don't need here
  # Add period at liberty
  tagrep[,"period_at_liberty" := ((recap.ts - rel.ts) * no_seasons)]
  tagrep[, rel.ts.after.mix := rel.ts + mixing_period_years]

  # Drop observations that are within the mixing period
  tagrep <- tagrep[!(recap.ts < rel.ts.after.mix),]

  # Summarise the three plots - or do it at end - might need to
  # do it at the end as number of models increases
  tagrep_dat[[model]] <- tagrep
}

tagrep_dat <- rbindlist(tagrep_dat, idcol="model")

# Data for tag returns by time plot
# Summarise returns by recapture group
tag_returns_time <- tagrep_dat[, .(recap.pred = sum(recap.pred, na.rm=TRUE),
                                   recap.obs = sum(recap.obs, na.rm=TRUE)),
                               by=.(tag_recapture_group, tag_recapture_name,
                                    recap.ts, model)]
# To ensure plotting is OK we need each fishery to have
# a full complement of time series
padts <- expand.grid(recap.ts = seq(from=min(tag_returns_time$recap.ts),
                                    to=max(tag_returns_time$recap.ts),
                                    by=1/no_seasons),
                     tag_recapture_name =
                       sort(unique(tag_returns_time$tag_recapture_name)),
                     model = sort(unique(tag_returns_time$model)))
padts <- merge(padts, fishery_map[c("tag_recapture_group",
                                    "tag_recapture_name")])
tag_returns_time <- merge(tag_returns_time, padts, all=TRUE)
# Any NAs need to set to 0
tag_returns_time[is.na(tag_returns_time$recap.pred), "recap.pred" := 0]
tag_returns_time[is.na(tag_returns_time$recap.obs), "recap.obs" :=0 ]
# Get scaled diff for residuals plot
tag_returns_time[, "diff":= recap.obs - recap.pred]

tag_returns_time <-
  tag_returns_time[, .(recap.obs, recap.pred, model=model, recap.ts=recap.ts,
                       diff = diff / mean(recap.obs, na.rm=TRUE)),
                   by=.(tag_recapture_name, tag_recapture_group)]

# Data for attrition plot
tagrep_dat[, "diff" := recap.obs - recap.pred]
tag_attrition <- tagrep_dat[, .(recap.obs=sum(recap.obs, na.rm=TRUE),
                                recap.pred=sum(recap.pred, na.rm=TRUE),
                                diff = sum(diff, na.rm=TRUE)),
                            by=.(model, period_at_liberty, region, program)]
# Need to pad out time series to avoid missing missing periods at liberty
padts <- expand.grid(
  period_at_liberty = seq(from=min(tag_attrition$period_at_liberty),
                          to=max(tag_attrition$period_at_liberty), by=1),
  program = sort(unique(tag_attrition$program)),
  region = sort(unique(tag_attrition$region)))
tag_attrition <- merge(tag_attrition, padts, by=colnames(padts), all=TRUE)
# 1. Number of tag returns (y) against period at liberty
# For the observed and predicted recaptures, NA is essentially 0,
# i.e. there were no recaptures, so set to 0
tag_attrition[is.na(recap.pred), recap_pred := 0]
tag_attrition[is.na(recap.obs), recap_obs := 0]
tag_attrition[is.na(diff), diff := 0]

# Data for tag return proportions
tag_returns_prop <- tagrep_dat[, .(recap.pred = sum(recap.pred, na.rm=TRUE),
                                   recap.obs=sum(recap.obs, na.rm=TRUE)),
                               by=.(model, rel.region, region, recap.month)]
tag_returns_prop_sum <-
  tagrep_dat[, .(recap.pred.sum = sum(recap.pred, na.rm=TRUE),
                 recap.obs.sum=sum(recap.obs, na.rm=TRUE)),
             by=.(rel.region, recap.month)]
# Merge together and get the proportion of recaptures,
# i.e. the total number of tags from region 1 that were recaptured,
# found out the proportion that was recaptured in each region
tag_returns_prop <- merge(tag_returns_prop, tag_returns_prop_sum)
tag_returns_prop[, c("pred.prop", "obs.prop") :=
                     .(recap.pred/recap.pred.sum, recap.obs/recap.obs.sum)]
# Plot the difference between pred and obs proportion of tags returned
# by region of release
tag_returns_prop[, "diff_prop" := obs.prop - pred.prop]
tag_returns_prop[, "rel.region.name" := paste("Release region", rel.region)]
tag_returns_prop[, "Quarter" := as.factor((recap.month+1) / 3)]

save(tag_returns_time, tag_attrition, tag_returns_prop,
     file="../app/data/tag_data.RData")
