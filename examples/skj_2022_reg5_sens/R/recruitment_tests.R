# Recruitment tests
library(FLR4MFCL)
library(ggplot2)
library(data.table)

basedir <- "C:/Work/MFCL/ofp-sam-skj22/stepwise2022/"

# Want to plot
# Stock recruitment relationship
# Recruitment distribution
# Recruitment deviates

#-----------------------------------------------------------------------------
# Plot SRR
#-----------------------------------------------------------------------------
# In Hierophant
# From diags4MFCL - see below
#p <- plot.srr(rep.list=rep_list[mc], show.legend = FALSE, palette.func=hierophant_palette, all.model.names=model_names, axis=input$gridfactor, selected_model=input$model_select)

# Data is Rec and SSB by Year 

# We need reps

# In SALSA
# Just recruitment over time
model <- "M2"
# What rep file

# ID the par file
lf <- list.files(paste(basedir, model, sep="/"))
parfiles <- lf[grep(".par$", lf)]
# Find the biggest par file
# Careful model 9x has a weird par file
biggest_par <- as.character(max(as.numeric(gsub(".par", "", parfiles)), na.rm=TRUE))
if(length(biggest_par)==1){
  biggest_par <- paste0("0",biggest_par)
}
repfile <- paste0("plot-", biggest_par, ".par.rep")
parfile <- paste0(biggest_par, ".par")

rep <- read.MFCLRep(paste(basedir, model, repfile, sep="/"))
par <- read.MFCLPar(paste(basedir, model, parfile, sep="/"))

#********************
# Key question - do we do all the extraction and calculation in advance, or do it in app with
# the rep files. Depends on other plots
#********************




#adult_biomass <- as.data.frame(areaSums(adultBiomass(rep)))[,c("year","season","data")]
#recruitment <- as.data.frame(areaSums(popN(rep)[1,]))[,c("year","season","data")]
# Alternative with data.table
adult_biomass <- as.data.table(areaSums(adultBiomass(rep)))[,c("year","season","value")]
recruitment <- as.data.table(areaSums(popN(rep)[1,]))[,c("year","season","value")]
setnames(adult_biomass, "value", "sb")
setnames(recruitment, "value", "rec")
# Basically plotting year to year
# This might not be strictly correct as the recruitment function is fitted using
# some kind of rolling window of SB (see that report on bias correction)
pdat <- merge(adult_biomass, recruitment)
pdat[,c("year", "season") := .(as.numeric(year), as.numeric(season))]
  
# For plotting - not include in extraction?
# Data for the BH shape
max_sb <- max(pdat$sb) * 1.2 # Just add another 20% on
sb <- seq(0, max_sb, length=100)
# Extract the BH params and make data.frame of predicted recruitment
# Note that this is predicted ANNUAL recruitment, given a SEASONAL SB
# The data in the popN that we take recruitment from is SEASONAL
# There is then some distribution
params <- c(srr(rep)[c("a","b")])
bhdat <- data.frame(sb=sb, rec=(sb*params[1]) / (params[2] + sb))

# Is it an annual SRR?
# If so take mean SB and sum Rec - see notes above
flagval(par, 2, 182)$value # 1 - annualised SRR fit
# The Beverton-Holt stock-recruitment relationship is fitted to total "annualised" recruitments and average annual biomass
flagval(par, 2, 183)$value # 0 - allocate annual SRR by season
if(flagval(par, 2, 182)$value == 1){
  pdat = pdat[,.(sb=mean(sb,na.rm=TRUE),rec=sum(rec,na.rm=TRUE)),by=.(year)]
}

# Now plot it
# Single model in each panel
sb_units <- 1000
rec_units <- 1000000
# Label formatting
xlab <- paste0("Adult biomass (mt; ",format(sb_units, big.mark=",", trim=TRUE,scientific=FALSE),"s)")
ylab <-  paste0("Recruitment (N; ",format(rec_units,big.mark=",", trim=TRUE,scientific=FALSE),"s)")

p <- ggplot(pdat, aes(x=sb/sb_units, y=rec/rec_units))
# The points
p <- p + geom_point(aes(fill=year),shape=21,color="black",size=2)
p <- p + scale_fill_viridis_c("Year")
# The fitted model
p <- p + geom_line(data=bhdat, aes(x=sb/sb_units, y=rec/rec_units), size=1.2)

p <- p + ylim(c(0,NA)) + xlim(c(0,NA))
p <- p + theme_bw()
p <- p + xlab(xlab) + ylab(ylab)
p

# Multiple models in same panel

#------------------------------------------------------------------------------
# Recruitment distribution
#------------------------------------------------------------------------------

# Same data as SRR plot?  - as in just popN

# Average recruitment over time series by region and quarter
av_rec <- yearMeans(popN(rep)[1,])
# Total recruitment of whole time series
total_rec <- c(areaSums(seasonSums(yearMeans(popN(rep)[1,]))))
prop_rec <- av_rec / total_rec
prop_rec <- as.data.table(prop_rec)[,c("season", "area", "value")]

# How to combine with srr data?


# Violin plot - all models together
# Or bar plot - each model is a coloured bar

p <- ggplot(prop_rec, aes(x=season, y=value))
#p <- p + geom_violin()
p <- p + geom_point(alpha=0.25) # Maybe overlay the original data?
p <- p + facet_wrap(~area, ncol = 8)
p <- p + xlab("Quarter") + ylab("Proportion of total recruitment")
p <- p + theme_bw()
p
  #p <- p + ggplot2::theme(legend.position = "none")
  #p <- p + ggplot2::scale_fill_manual("Model",values=colour_values)


#-------------------------------------------
# Recruitment devs
#-------------------------------------------


  
  # Check args
  #par.list <- check.par.args(par=par.list, par.names=par.names)
  #par.names <- names(par.list)
  # Grab the region_rec_var per Model

# Data is devs by year / season / area
# So not like the plot_srr data

pdat <- as.data.table(region_rec_var(par))[,c("year","season", "area", "value")]
pdat[,c("year", "season") := .(as.numeric(year), as.numeric(season))]
pdat[,"ts" := .(year + (season-1) / 4 + 1/8)]

# Then need to work out how best to plot it
# One model at a time?
# Combine Year + Season into TS?


# By season
p <- ggplot(pdat, aes(x=year, y=value))
p <- p + geom_point()
p <- p + geom_smooth(colour="red", method = 'loess', formula = 'y~x', na.rm=TRUE, se=FALSE)
p <- p + facet_grid(season~area)
p <- p + theme_bw()
p
# Add smoothers for all lines and no points if multiple models

# Can plot each model with a separate smoother and no points

# By timestep
#p <- ggplot(pdat, aes(x=ts, y=value))
#p <- p + geom_point()
#p <- p + geom_smooth(colour="red", method = 'loess', formula = 'y~x', na.rm=TRUE, se=FALSE)
#p <- p + facet_wrap(~area)
#p <- p + theme_bw()
#p



#------------------------------------------------------------------

basedir <- "C:/Work/MFCL/ofp-sam-skj22/stepwise2022/"
models <- paste0("M", c(2,3,4,5,6,7))

srr_dat <- list()
srr_fit_dat <- list()
rec_dev_dat <- list()
for (model in models){
  cat("Model: ", model, "\n")
  # ID the rep
  # ID the par file
  lf <- list.files(paste(basedir, model, sep="/"))
  parfiles <- lf[grep(".par$", lf)]
  # Find the biggest par file
  # Careful model 9x has a weird par file
  biggest_par <- as.character(max(as.numeric(gsub(".par", "", parfiles)), na.rm=TRUE))
  if(length(biggest_par)==1){
    biggest_par <- paste0("0",biggest_par)
  }
  repfile <- paste0("plot-", biggest_par, ".par.rep")
  # Load the rep
  rep <- read.MFCLRep(paste(basedir, model, repfile, sep="/"))
  #par <- read.MFCLPar(paste(basedir, model, parfile, sep="/"))
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
  
}
srr_dat <- rbindlist(srr_dat, idcol="model")
srr_fit_dat <- rbindlist(srr_fit_dat, idcol="model")
rec_dev_dat <- rbindlist(rec_dev_dat, idcol="model")

save(srr_dat, srr_fit_dat, rec_dev_dat, file = "../app/data/srr_data.Rdata")

# Then do distribution and srr fit from the same srr_dat
# Or do additional processing (meaning etc) and save as separate data objects?

# plot srr
# How do we handle this? Hard wire it in the plot
# This is only used for comparing to the BH relationship
#if(flagval(par, 2, 182)$value == 1){
#  pdat = pdat[,.(sb=mean(sb,na.rm=TRUE),rec=sum(rec,na.rm=TRUE)),by=.(year)]
#}

# Make pdat after 
pdat <- srr_dat[,.(sb=sum(sb, na.rm=TRUE), rec=sum(rec, na.rm=TRUE)), by=.(model,year,season)]
# Assume that annualised relationship i.e. flagval(par, 2, 182) == 1

# Is it an annual SRR?
#flagval(par, 2, 182)$value # 1 - annualised SRR fit
# The Beverton-Holt stock-recruitment relationship is fitted to total "annualised" recruitments and average annual biomass
pdat = pdat[,.(sb=mean(sb,na.rm=TRUE),rec=sum(rec,na.rm=TRUE)),by=.(model,year)]

# Now plot it
# Single model in each panel
sb_units <- 1000
rec_units <- 1000000
# Label formatting
xlab <- paste0("Adult biomass (mt; ",format(sb_units, big.mark=",", trim=TRUE,scientific=FALSE),"s)")
ylab <-  paste0("Recruitment (N; ",format(rec_units,big.mark=",", trim=TRUE,scientific=FALSE),"s)")

xmax <- max(pdat$sb) * 1.2 / sb_units

# Option 1
p <- ggplot(pdat, aes(x=sb/sb_units, y=rec/rec_units))
# The points
p <- p + geom_point(aes(fill=year),shape=21,color="black",size=2)
p <- p + scale_fill_viridis_c("Year")
# The fitted model
p <- p + geom_line(data=srr_fit_dat, aes(x=sb/sb_units, y=rec/rec_units), size=1.2)
p <- p + facet_wrap(~model)
p <- p + ylim(c(0,NA)) + xlim(c(0,xmax))
p <- p + theme_bw()
p <- p + xlab(xlab) + ylab(ylab)
p

# Do all lines and points on same plot and colour by model
# Option 2
p <- ggplot(pdat, aes(x=sb/sb_units, y=rec/rec_units))
# The points
p <- p + geom_point(aes(fill=model),shape=21,color="black",size=2)
# The fitted model
p <- p + geom_line(data=srr_fit_dat, aes(x=sb/sb_units, y=rec/rec_units, colour=model), size=1.2)
p <- p + ylim(c(0,NA)) + xlim(c(0,xmax))
p <- p + theme_bw()
p <- p + xlab(xlab) + ylab(ylab)
p

# These look good!

# Recruitment distribution

# Average recruitment over time series by region and quarter
av_rec <- srr_dat[,.(av_rec = mean(rec)), by=.(model, season, area)]
# Total average recruitment of whole time series
total_rec <- av_rec[,.(total_rec = sum(av_rec)), by=.(model)]
pdat <- merge(av_rec, total_rec)
pdat[, c("prop_rec", "season") := .(av_rec/total_rec, as.character(season))]
# Sanity check
#pdat[,.(sum = sum(prop_rec)), by=.(model)]


# Violin plot - all models together
# Or bar plot - each model is a coloured bar

p <- ggplot(pdat, aes(x=season, y=prop_rec))
p <- p + geom_violin(aes(fill=area))
#p <- p + geom_point(aes())#alpha=0.25) # Maybe overlay the original data?
p <- p + geom_point()#alpha=0.25) # Maybe overlay the original data?
p <- p + facet_wrap(~area, ncol = 8)
p <- p + xlab("Quarter") + ylab("Proportion of total recruitment")
p <- p + theme_bw()
p

# Or bar charts - nice
p <- ggplot(pdat, aes(x=season, y=prop_rec))
p <- p + geom_bar(aes(fill=model), stat="identity", position="dodge")
p <- p + facet_wrap(~area, ncol = 2)
p <- p + xlab("Quarter") + ylab("Proportion of total recruitment")
p <- p + theme_bw()
p


# Recruitment devs - what's happening with M6
p <- ggplot(rec_dev_dat[model %in% c("M6", "M7")], aes(x=year, y=value))
#p <- ggplot(rec_dev_dat[model %in% c("M6")], aes(x=year, y=value))
p <- p + geom_point(aes(fill=model, colour=model), alpha=0.5)
p <- p + geom_smooth(aes(colour=model), method = 'loess', formula = 'y~x', na.rm=TRUE, se=FALSE)
p <- p + facet_grid(season~area)
p <- p + theme_bw()
p



