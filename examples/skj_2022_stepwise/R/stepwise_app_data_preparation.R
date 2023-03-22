#------------------------------------------------------------------
# Preparing the data for the stepwise skipjack 2022 assessment app.
# Basic approach is to go through the results folders in the repository
# https://github.com/PacificCommunity/ofp-sam-skj22
# and hoover out the data we need.
#
# Finlay Scott, 25/04/2022
# Soundtracks:
# Throes of Joy in the Jaws of Defeatism - Napalm Death
# Black Mill Tapes Vol. 5: The Lost Tapes - Pye Corner Audio
#------------------------------------------------------------------

# Libraries
# Make sure you get FLR4MFCL compiled after 24/04/2022 to ensure that
# the movement matrices are being read in correctly (from / to)
library(FLR4MFCL)
library(data.table)

# Location of the local copy of https://github.com/PacificCommunity/ofp-sam-skj22 
basedir <- "C:/Work/MFCL/ofp-sam-skj22/stepwise2022/"
# The models you want to include - check with CCJ and JH as some should be excluded.
# Also some have missing files so don't work:

# If you haven't run the code to generate the fishery map you should do so
source("fishery_map.R")
# Load the fishery map - assumed to be the same for all models
load("../app/data/fishery_map.Rdata")


models <- paste0("M",c(1,2,3,4,5,6,"7new", 8, "9x"))

# Model description taken from the README of the results repository.
model_description <- data.frame(model = models, 
                                 model_description = c(
                                   "2019 Diagnostic case using the new MFCL 2.0.8.4",
                                   "Convert M1 to catch conditioning",
                                   "M2 + VAST PL surveys (ungrouped) + PS index for region 5 and 6 ungrouped. Eff_fm is estimated",
                                   "M3 + PL surveys grouped (regions 1, 2, 3, 4, 7 & 8) + PS surveys 5 & 6 ungrouped",
                                   "M4 + Orthogonal Polynomial Recruitment (OPR)",
                                   "M4 + New movement option",
                                   "M4 + Dirichilet multinomial-no RE + growth estimation",
                                   "M7new + new movement + 2yrs equilib ini pop Model will be run from scratch, and the diagnostic will be reviewed using a new shiny app (similar to Hierophant)", 
                                   "M5 + M7"
                                   ))

#------------------------------------------------------------------
# Data checker
# Each model folder needs to have the following files:
# length.fit
# skj2.frq 
# obsX (X = 1: nfisheries)
# predX (X = 1: nfisheries)
# skj.tag
# *.par
# *.rep
# temporary_tag_report

# Only catch conditioned models have the obsX and predX files.
# e.g. M1 does not have it - skipped in the relevant section below.
needed_files <- c("skj.tag", "length.fit", "temporary_tag_report", "skj2.frq", "test_plot_output")
for (model in models){
  model_files <- list.files(paste0("C:/Work/MFCL/ofp-sam-skj22/stepwise2022/", model))
  # Also check for a par and rep
  
  parfiles <- model_files[grep(".par$", model_files)]
  if(length(parfiles)==0){
    cat("Missing par file in model", model, ".  Dropping model.\n")
    models <- models[!(models %in% model)]
  }
  repfiles <- model_files[grep("par.rep$", model_files)]
  if(length(repfiles)==0){
    cat("Missing rep file in model", model, ".  Dropping model.\n")
    models <- models[!(models %in% model)]
  }
  
  if (!all(needed_files %in% model_files)){
    missing_file <- needed_files[!(needed_files %in% model_files)]
    #models <- models
    cat("Missing files in model", model, ":", missing_file, ".  Dropping model.\n")
    models <- models[!(models %in% model)]
  }
}


#------------------------------------------------------------------
# Data for catch size distribution plots
# This involves going through the length.fit files are processing the data
# The function to read and process the data is here:
# Currently it does not use data.table
source("read_length_fit_file.R")

cat("Catch distribution stuff\n")
lfits_dat <- lapply(models, function(x) {
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
# Data for catchability (not really catchability but it sort of is) plots.
# This involves reading and the compiling the many obsX and predX files - gets a bit ugly.
# We need to load a freq file to figure out when each fishery is operating.
# However, some models have frq files in a weird (windows?) format that doesn't load.
# For these an extra frq2 file has been included.

cat("Catchability stuff\n")
# This was probably done as a nested lapply as it is hard to debug. Nevermind.
catchability <- lapply(models, function(model){
  cat("Model: ", model, "\n")
  files <- list.files(paste(basedir, model, sep="/"))
  # If there is a frq2 load that one - otherwise - crash out and complain
  frq <- read.MFCLFrq(paste(basedir, "M2", "skj2.frq", sep="/"))
  rfrq <- realisations(frq)
  #rfrq$season <- (rfrq$month-2)/12
  rfrq$season <- (rfrq$month-1)/12 + 1/24
  rfrq$ts <- rfrq$year + rfrq$season
  
  # Get a list of the all obsX and predX files
  # Use little hat ^ to match the beginning of the string
  obsfiles <- files[grep("^obs", files)]
  predfiles <- files[grep("^pred", files)]
  
  # What to do if missing these files?
  if(length(obsfiles) == 0 | length(predfiles) == 0){
    cat("No obsX or predX files\n")
    return()
  }
  # Only include those with a numeric (some sneaky tag ones in there)
  obsfiles <- obsfiles[grep("([0-9]+).*$", obsfiles)] 
  predfiles <- predfiles[grep("([0-9]+).*$", predfiles)] 
  file_number <- 1:length(obsfiles)
  # Process each file - assumed to be numbered by fishery
  obspredtemp <- lapply(file_number, function(x){
    cat("Model: ", model, " File number: ", x, "\n")
    # Files are not always by fishery number due to character ordering
    fishery_number <-  as.numeric(unlist(strsplit(obsfiles[x], "obs"))[2])
    obsfilename <- obsfiles[x]
    obstemp <- read.table(paste(basedir, model, obsfilename, sep="/"), sep="", col.names=c("ts", "obs_log_q"))
    # Hack - because some fisheries have no effort
    # Make sure you drop the effort == -1
    rfrqtemp <- subset(rfrq, fishery==fishery_number & effort != -1.0)
    # If there is no effort data in the frq - skip those obs and pred files - seem to contain some data for four time steps
    if(dim(rfrqtemp)[1] == 0){
      return(data.frame())
    }
    # Make sure order is by time step
    rfrqtemp <- rfrqtemp[order(rfrqtemp$fishery, rfrqtemp$ts),]
    predfilename <- predfiles[x]
    predtemp <- read.table(paste(basedir, model, predfilename, sep="/"), sep="", col.names=c("ts", "pred_log_q"))
    out <- cbind(obstemp, pred_log_q = predtemp$pred_log_q)
    out$fishery <- fishery_number 
    out <- out[order(out$ts),]
    out$ts <- rfrqtemp$ts
    out$year <- rfrqtemp$year
    out$season<- rfrqtemp$season
    return(out)
  }) # End of file lapply
  obspredtemp <- do.call("rbind", obspredtemp)
  obspredtemp$model <- model
  return(obspredtemp)
})
# Bind it into a data.table
catchability <- rbindlist(catchability)
catchability <- merge(catchability, fishery_map)
catchability <- merge(catchability, model_description, by="model")
catchability[,c("obs_q", "pred_q") := .(exp(obs_log_q), exp(pred_log_q))]

# Make another data object without the seasonality by simply averaging over years - a bad idea?
catchability_annual <- catchability[, .(obs_q = mean(obs_log_q), pred_q = mean(pred_log_q)), by=.(year, fishery, fishery_name, model, model_description, region)]

# Look at difference - better for evaluating fit?
# Don't do this for the annual data
catchability[,diff := .(obs_q - pred_q)]
# Scale by total catchability by fishery and model
catchability[, scale_diff :=  diff / mean(obs_q, na.rm=TRUE), by=.(model, fishery)]

# Save the catchability objects
save(catchability, catchability_annual, file="../app/data/catchability_data.Rdata")

#------------------------------------------------------------------
# Movement
# We want to extract the diff_coffs_age_period from a par object.
# Par objects are big so it is easier to read in a bit of the par object (MFCLRegion).
# Thanks object oriented programming and overloading!

cat("Movement stuff\n")
move_coef <- list()
for (model in models){
  cat("Model: ", model, "\n")
  # ID the par file
  lf <- list.files(paste(basedir, model, sep="/"))
  parfiles <- lf[grep(".par$", lf)]
  
  # Find the biggest par file
  # Careful model 9x has a weird par file - gives a warning
  #biggest_par <- as.character(max(as.numeric(gsub(".par", "", parfiles)), na.rm=TRUE))
  # Just take the first two characters - assume numeric
  biggest_par <- as.character(max(as.numeric(substr(parfiles,1,2))))
  if(length(biggest_par)==1){
    biggest_par <- paste0("0",biggest_par)
  }
  biggest_par <- paste0(biggest_par,".par")
  reg <- read.MFCLRegion(paste(basedir, model, biggest_par, sep="/"))
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

# This could probably be reduced because the movement is not age structured (so far)

save(move_coef, file="../app/data/move_coef.Rdata")

#------------------------------------------------------------------
# General stuff including stock recruitment, SB and SBSBF0 data.

cat("General stuff\n")

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

index_fisheries <- 32:39

for (model in models){
  cat("Model: ", model, "\n")
  # ID the rep
  # ID the par file
  lf <- list.files(paste(basedir, model, sep="/"))
  parfiles <- lf[grep(".par$", lf)]
  # Find the biggest par file
  # Careful model 9x has a weird par file
  #biggest_par <- as.character(max(as.numeric(gsub(".par", "", parfiles)), na.rm=TRUE))
  biggest_par <- as.character(max(as.numeric(substr(parfiles,1,2))))
  if(length(biggest_par)==1){
    biggest_par <- paste0("0",biggest_par)
  }
  repfile <- paste0("plot-", biggest_par, ".par.rep")
  # Load the rep
  rep <- read.MFCLRep(paste(basedir, model, repfile, sep="/"))
  #par <- read.MFCLPar(paste(basedir, model, parfile, sep="/"))
  
  # SRR stuff
  adult_biomass <- as.data.table(adultBiomass(rep))[,c("year","season","area", "value")]
  recruitment <- as.data.table(popN(rep)[1,])[,c("year","season","area", "value")]
  setnames(adult_biomass, "value", "sb")
  setnames(recruitment, "value", "rec")
  pdat <- merge(adult_biomass, recruitment)
  pdat[,c("year", "season") := .(as.numeric(year), as.numeric(season))]
  srr_dat[[model]] <- pdat
  
  # Get the BH fit
  # Need to pick a suitable max SB
  # Sum over areas and assume annualised (mean over years)
  #pdattemp <- pdat[, .(sb=sum(sb)), by=.(year, season)]
  #pdattemp <- pdattemp[, .(sb=mean(sb)), by=.(year)]
  #max_sb <- max(pdattemp$sb) * 1.2 # Just add another 20% on
  max_sb <- 20e6 # Just pick a massive number and then trim using limits
  sb <- seq(0, max_sb, length=100)
  # Extract the BH params and make data.frame of predicted recruitment
  # Note that this is predicted ANNUAL recruitment, given a SEASONAL SB
  # The data in the popN that we take recruitment from is SEASONAL
  # There is then some distribution
  params <- c(srr(rep)[c("a","b")])
  bhdat <- data.frame(sb=sb, rec=(sb*params[1]) / (params[2] + sb))
  srr_fit_dat[[model]] <- bhdat
  
  # Get the rec devs
  parfile <- paste0(biggest_par, ".par")
  #par <- read.MFCLPar(parfile)
  par <- read.MFCLPar(paste(basedir, model, parfile, sep="/"))
  rdat <- as.data.table(region_rec_var(par))[,c("year","season", "area", "value")]
  rdat[,c("year", "season") := .(as.numeric(year), as.numeric(season))]
  rdat[,"ts" := .(year + (season-1) / 4 + 1/8)]
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
  sbdat <- merge(sbdat, sbsbf0dat)
  sbdat <- sbdat[,c("year","area","SB","SBSBF0")]
  sbdat[area=="unique", area := "All"] # Change in place - data.table for the win
  sbdat[,year := as.numeric(year)]
  biomass_dat[[model]] <- sbdat
  
  
  # Selectivity by age class (in quarters)
  sel <- as.data.table(sel(rep))[,.(age,unit,value)]
  sel[,c("age", "unit") := .(as.numeric(age), as.numeric(unit))]
  setnames(sel, "unit", "fishery")
  # Bring in lengths
  mean_laa <- c(aperm(mean_laa(rep),c(4,1,2,3,5,6)))
  sd_laa <- c(aperm(sd_laa(rep),c(4,1,2,3,5,6)))
  # Order sel for consecutive ages
  setorder(sel, fishery, age)
  nfisheries <- length(unique(sel$fishery))
  sel$length <- rep(mean_laa, nfisheries)
  sel$sd_length <- rep(sd_laa, nfisheries)
  sel[,c("length_upper", "length_lower") := .(length + 1.96 * sd_length, length - 1.96 * sd_length)]
  sel_dat[[model]] <- sel
  
  # Natural mortality
  m <- m_at_age(rep)
  m <- data.table(age=1:length(m), m = m)
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
  cpue[,cpue_pred := cpue_pred$value]
  setnames(cpue, "unit", "fishery")
  cpue[, ts := .(as.numeric(year) + (as.numeric(season)-1)/4)]
  # Trim out only the index fisheries - loses M1 and M2
  cpue <- cpue[fishery %in% index_fisheries]
  cpue[,fishery := as.numeric(fishery)] # For merging with fishery_map
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
cpue_dat[,diff := .(cpue_obs - cpue_pred)]
# Scale by total catchability by fishery and model
cpue_dat[, scale_diff :=  diff / mean(cpue_obs, na.rm=TRUE), by=.(model, fishery)]

save(status_tab_dat, cpue_dat, mat_age_dat, mat_length_dat, biomass_dat, srr_dat, srr_fit_dat, rec_dev_dat, sel_dat, m_dat, file = "../app/data/other_data.Rdata")

#-----------------------------------

# Likelihood table
cat("Likelihood table")
ll_tab_dat <- list()
for (model in models){
  cat("Model: ", model, "\n")
  # Load the likelihood and par files
  ll <- read.MFCLLikelihood(paste(basedir, model, "test_plot_output", sep="/"))
  lf <- list.files(paste(basedir, model, sep="/"))
  parfiles <- lf[grep(".par$", lf)]
  # Find the biggest par file
  biggest_par <- as.character(max(as.numeric(substr(parfiles,1,2))))
  if(length(biggest_par)==1){
    biggest_par <- paste0("0",biggest_par)
  }
  biggest_par <- paste0(biggest_par,".par")
  par <- read.MFCLPar(paste(basedir, model, biggest_par, sep="/"))
  
  # Get LL summary
  ll_summary <- summary(ll)  

  # Original method that requires renaming columns 
  #lldf <- matrix(ll_summary$likelihood, nrow=1, dimnames=list(NULL,ll_summary$component))
  #lldf <- as.data.frame(lldf)
  ## Add max gradient
  #lldf$max_grad <-  max_grad(par)
  ## Number of parameters
  #lldf$npars <-  n_pars(par)

  # Or build data.table with correct names here
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

save(ll_tab_dat, file = "../app/data/ll_tab_data.Rdata")

#-----------------------------------
# Tag plot data - complicated

# M5, M6 and M7 missing temporary tag report
cat("Tagging stuff\n")
tagrep_dat <- list()
for (model in models){
  cat("Model: ", model, "\n")
  #srr_dat[[model]] <- pdat
  
  # Needs a par file
  # ID the par file
  lf <- list.files(paste(basedir, model, sep="/"))
  parfiles <- lf[grep(".par$", lf)]
  # Find the biggest par file
  # Careful model 9x has a weird par file
  #biggest_par <- as.character(max(as.numeric(gsub(".par", "", parfiles)), na.rm=TRUE))
  biggest_par <- as.character(max(as.numeric(substr(parfiles,1,2))))
  if(length(biggest_par)==1){
    biggest_par <- paste0("0",biggest_par)
  }
  biggest_par <- paste0(biggest_par,".par")
  par <- read.MFCLPar(paste(basedir, model, biggest_par, sep="/"))
  #rep <- read.MFCLRep(paste(basedir, model, "plot-07.par.rep", sep="/"))

  # Tag releases from the *.tag file
  # The recaptures slot contains the observed recaptures but not used here - we use temporary_tag_report file
  # which has the predicted and observed recaptures
  tagobs <- read.MFCLTag(paste(basedir, model, "/skj.tag", sep=""))
  tag_releases <- data.table(releases(tagobs))
  # Summarise release numbers by release event, i.e. sum the length distributions
  tag_releases <- tag_releases[, .(rel.obs = sum(lendist, na.rm=TRUE)), by=.(program, rel.group, region, year, month)]
  setnames(tag_releases, c("region", "year", "month"), c("rel.region", "rel.year", "rel.month"))
  # Bring in the mixing period - needs a par file
  tag_releases$mixing_period <- flagval(par, (-10000 - tag_releases$rel.group + 1),1)$value
  # What is the mixing period in terms of years?
  no_seasons <- dimensions(par)["seasons"]
  tag_releases$mixing_period_years <- tag_releases$mixing_period / no_seasons
  # Add a time step
  tag_releases$rel.ts <- tag_releases$rel.year + (tag_releases$rel.month-1)/12 + 1/24
  #setorder(tag_releases, rel.group, rel.ts)
  
  # Temporary tag report - includes the predicted tag recoveries disaggregated to a very low level - release and recapture
  tagrep <- read.temporary_tag_report(paste(basedir, model, "/temporary_tag_report", sep=""))
  tagrep <- data.table(tagrep)

  # Bring in recapture fishery and region
  fm2 <- fishery_map
  colnames(fm2)[colnames(fm2)=="fishery"] <- "recap.fishery"
  tagrep <- merge(tagrep, fm2[,c("recap.fishery", "region", "tag_recapture_group", "tag_recapture_name")])
  tagrep$recap.ts <- tagrep$recap.year + (tagrep$recap.month-1)/12 + 1/24

  # Bring in tagging program, and rel.ts from the tag release data (from the skj.tag file)
  # tag_releases has one row for each tag release group (269 of them) giving the region, year and month of that release
  #tagrep <- merge(tagrep, tag_releases[,c("rel.group","program", "rel.ts")], by="rel.group")
  # Potentially drop some columns here
  tagrep <- merge(tagrep, tag_releases, by="rel.group")
  # There are a lot of columns that maybe we don't need here
  # Add period at liberty
  tagrep[,"period_at_liberty" := ((recap.ts - rel.ts) * no_seasons)]
  tagrep[, rel.ts.after.mix := rel.ts + mixing_period_years]

  # Drop observations that are within the mixing period  
  # Do we do this for all plots - probably
  tagrep <- tagrep[!(recap.ts < rel.ts.after.mix),]
  
  # Summarise the three plots - or do it at end - might need to do it at the end as number of models increases
  tagrep_dat[[model]] <- tagrep
  
}

tagrep_dat <- rbindlist(tagrep_dat, idcol="model")

# Data for tag returns by time plot
# Summarise returns by recapture group
tag_returns_time <- tagrep_dat[,.(recap.pred = sum(recap.pred, na.rm=TRUE), recap.obs = sum(recap.obs, na.rm=TRUE)), by=.(tag_recapture_group, tag_recapture_name, recap.ts, model)]  
object.size(tag_returns_time)/1e6
# To ensure plotting is OK we need each fishery to have a full complement of time series
padts <- expand.grid(recap.ts = seq(from=min(tag_returns_time$recap.ts), to=max(tag_returns_time$recap.ts), by= 1 / no_seasons), tag_recapture_name = sort(unique(tag_returns_time$tag_recapture_name)), model=sort(unique(tag_returns_time$model)))
padts <- merge(padts, fishery_map[c("tag_recapture_group", "tag_recapture_name")])
tag_returns_time <- merge(tag_returns_time, padts, all=TRUE)#, by=colnames(padts))
# Any NAs need to set to 0
tag_returns_time[is.na(tag_returns_time$recap.pred), "recap.pred":=0]
tag_returns_time[is.na(tag_returns_time$recap.obs), "recap.obs":=0]
# Get scaled diff for residuals plot
tag_returns_time[,"diff":= recap.obs - recap.pred]

tag_returns_time <- tag_returns_time[,.(recap.obs, recap.pred, model=model, recap.ts=recap.ts, diff = diff / mean(recap.obs, na.rm=TRUE)), by=.(tag_recapture_name, tag_recapture_group)]



# Data for attrition plot
tagrep_dat[,"diff" := recap.obs - recap.pred]
tag_attrition <- tagrep_dat[, .(recap.obs=sum(recap.obs, na.rm=TRUE), recap.pred=sum(recap.pred, na.rm=TRUE), diff = sum(diff, na.rm=TRUE)) ,by=.(model, period_at_liberty, region, program)]
# Need to pad out time series to avoid missing missing periods at liberty
padts <- expand.grid(period_at_liberty = seq(from=min(tag_attrition$period_at_liberty), to=max(tag_attrition$period_at_liberty), by= 1), program = sort(unique(tag_attrition$program)), region = sort(unique(tag_attrition$region)))
tag_attrition <- merge(tag_attrition, padts, by=colnames(padts), all=TRUE)
# 1. Number of tag returns (y) against period at liberty
# For the observed and predicted recaptures, NA is essentially 0,
# i.e. there were no recaptures, so set to 0
tag_attrition[is.na(recap.pred), recap_pred:= 0]
tag_attrition[is.na(recap.obs), recap_obs:= 0]
tag_attrition[is.na(diff), diff := 0]

# Data for tag return proportions
tag_returns_prop <- tagrep_dat[,.(recap.pred = sum(recap.pred, na.rm=TRUE), recap.obs=sum(recap.obs, na.rm=TRUE)), by=.(model, rel.region, region, recap.month)]
tag_returns_prop_sum <- tagrep_dat[,.(recap.pred.sum = sum(recap.pred, na.rm=TRUE), recap.obs.sum=sum(recap.obs, na.rm=TRUE)), by=.(rel.region, recap.month)]
# Merge together and get the proportion of recaptures 
# i.e. the total number of tags from region 1 that were recaptured, found out the proportion that was recaptured in each region
tag_returns_prop<- merge(tag_returns_prop, tag_returns_prop_sum)
tag_returns_prop[,c("pred.prop", "obs.prop") := .(recap.pred/recap.pred.sum, recap.obs/recap.obs.sum)]
# Plot of the difference between predicted and observed proportion of tags returned by region of release
tag_returns_prop[,"diff_prop" := obs.prop - pred.prop]
tag_returns_prop[,"rel.region.name" := paste("Release region ", rel.region, sep="")]
tag_returns_prop[,"Quarter" := as.factor((recap.month+1)/3)]

save(tag_returns_time, tag_attrition, tag_returns_prop, file = "../app/data/tag_data.Rdata")
     









