# Tag return data
library(FLR4MFCL)
library(ggplot2)
library(data.table)

# Load the fishery map - assumed to be the same for all models
#source("fishery_map.R") # To recreate and save it
load("../app/data/fishery_map.Rdata")

basedir <- "C:/Work/MFCL/ofp-sam-skj22/stepwise2022/"

#-------------------------------------------------------------

model <- "M2"
# Looking at tag data stuff
# John reckons the stuff in the temporary_tag_report file is in the par rep file.
# But I think we need release group to make the tag attrition plot.
# Also, FLR4MFCL does not currently read in the tag data from the par rep file.

# Needs a par file too
# ID the par file
lf <- list.files(paste(basedir, model, sep="/"))
parfiles <- lf[grep(".par$", lf)]
# Find the biggest par file
# Careful model 9x has a weird par file
biggest_par <- as.character(max(as.numeric(gsub(".par", "", parfiles)), na.rm=TRUE))
if(length(biggest_par)==1){
  biggest_par <- paste0("0",biggest_par)
}
biggest_par <- paste0(biggest_par,".par")
par <- read.MFCLPar(paste(basedir, model, biggest_par, sep="/"))
rep <- read.MFCLRep(paste(basedir, model, "plot-07.par.rep", sep="/"))

# Tag releases from the *.tag file
# The recaptures slot contains the observed recaptures but not used here - we use temporary_tag_report file
# which has the predicted and observed recaptures
sp <- "skj"
tagobs <- read.MFCLTag(paste(basedir, model, "/",sp,".tag", sep=""))
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
setorder(tag_releases, rel.group, rel.ts)
# Added model again here - can avoid?
#tag_releases$model <- model
  
# Temporary tag report - includes the predicted tag recoveries disaggregated to a very low level - release and recapture
tagrep <- read.temporary_tag_report(paste(basedir, model, "/temporary_tag_report", sep=""))
tagrep <- data.table(tagrep)

# Bring in recapture fishery and region
colnames(fishery_map)[colnames(fishery_map)=="fishery"] <- "recap.fishery"
tagrep <- merge(tagrep, fishery_map[,c("recap.fishery", "region", "tag_recapture_group", "tag_recapture_name")])
tagrep$recap.ts <- tagrep$recap.year + (tagrep$recap.month-1)/12 + 1/24
# Drop columns we don't need for space
#tagrep[,c("recap.year", "recap.month", "recap.fishery"):=NULL]

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


tagrep$model <- model
object.size(tagrep) / 1e6
# 14 Mb per model - huge - need to summarise further



test1 <- tagrep[,.(recap.pred = sum(recap.pred, na.rm=TRUE), recap.obs = sum(recap.obs, na.rm=TRUE)), by=.(tag_recapture_group, tag_recapture_name, recap.ts, model)]  
test2 <- tagrep[,.(recap.pred = sum(recap.pred, na.rm=TRUE), recap.obs = sum(recap.obs, na.rm=TRUE)), by=.(tag_recapture_group, tag_recapture_name, recap.ts, model, program)]  
test3 <- tagrep[,.(recap.pred = sum(recap.pred, na.rm=TRUE), recap.obs = sum(recap.obs, na.rm=TRUE)), by=.(tag_recapture_group, tag_recapture_name, recap.ts, model, program, rel.region)]  
object.size(test3)/1e6 # 1.5 Mb


# We need the recapture group and name
# Currently hard wired into the fishery map with the assumption that this doesn't change between models


# Is this the only tag data we need?
# What plots do we want and is all information contained in this
# Tag returns - time series
# Sum recap pred and obs by recapture group and timestep 
#   1. Observed - predicted recaptures (scaled) by fishery / tag group for each model
#   2. Actual numbers of returns (observed and predicted) for a single model
# Tag attrition
# Sum recap pred and obs by period_at_liberty, region and program

#   1. Periods at liberty (x-axis) against Observed - predicted recaptures (scaled), by All tags, Region, Program for each model
#   2. Periods at liberty (x-axis) against Number of returns (Observed and predicted), by All tags, Region, Program for each model
# Tag return proportion
#   1. Observed proportion - predicted proportion of returns by region and quarter
# Sum recap pred and obs by rel region, region and recap month

dim(tagrep)

test <- tagrep[,.(recap.pred = sum(recap.pred, na.rm=TRUE), recap.obs = sum(recap.obs, na.rm=TRUE)), by=.(model, tag_recapture_group, tag_recapture_name, recap.ts, period_at_liberty, region, program, rel.region, recap.month)]

test1 <- tagrep[,.(recap.pred = sum(recap.pred, na.rm=TRUE), recap.obs = sum(recap.obs, na.rm=TRUE)), by=.(model, tag_recapture_group, tag_recapture_name, recap.ts)]
test2 <- tagrep[,.(recap.pred = sum(recap.pred, na.rm=TRUE), recap.obs = sum(recap.obs, na.rm=TRUE)), by=.(model, period_at_liberty, region, program)]
test3 <- tagrep[,.(recap.pred = sum(recap.pred, na.rm=TRUE), recap.obs = sum(recap.obs, na.rm=TRUE)), by=.(model, region, rel.region, recap.month)]

object.size(test)/1e6 # 7.2 Mb
object.size(test1)/1e6 # 0.2 Mb
object.size(test2)/1e6 # 0.3 Mb
object.size(test3)/1e6 # 0 Mb

# Need to summarise three different ways for the 3 tag plots

#-------------------------------------------------------------------

# Tag returns over time
  
# Summarise returns by recapture group
# Do in app?
pdat <- tagrep[,.(recap.pred = sum(recap.pred, na.rm=TRUE), recap.obs = sum(recap.obs, na.rm=TRUE)), by=.(tag_recapture_group, tag_recapture_name, recap.ts, model)]  
object.size(pdat)/1e6 # 0.2 Mb
  
  # To ensure plotting is OK we need each fishery to have a full complement of time series
  # observations, even if NA.
  # This is a right pain in the bum - must be an easier way
  # Need to pad out time series
#  no_seasons <- length(unique(tagrelease.list[[1]]$rel.month)) # unsafe, how best to get no seasons?

padts <- expand.grid(recap.ts = seq(from=min(pdat$recap.ts), to=max(pdat$recap.ts), by= 1 / no_seasons), tag_recapture_name = sort(unique(pdat$tag_recapture_name)), model=sort(unique(pdat$model)))
padts <- merge(padts, fishery_map[c("tag_recapture_group", "tag_recapture_name")])

#padts <- expand.grid(recap.ts = seq(from=min(pdat$recap.ts), to=max(pdat$recap.ts), by= 1 / no_seasons), tag_recapture_group = sort(unique(pdat$tag_recapture_group)), model=sort(unique(pdat$model)))
#padts <- merge(padts, fishery_map[c("tag_recapture_group", "tag_recapture_name")])

# Bring in recapture name and group
#padts <- merge(padts, fishery_map[c("tag_recapture_group", "tag_recapture_name")])
pdat <- merge(pdat, padts, all=TRUE)#, by=colnames(padts))

# Any NAs need to set to 0
pdat[is.na(pdat$recap.pred), "recap.pred":=0]
pdat[is.na(pdat$recap.obs), "recap.obs":=0]

# Subsetter
#pdat <- pdat[(tag_recapture_group %in% recapture.groups) & (Model %in% model_names),]
# Drop longlines
ll_fisheries <- c(3,6,9,17,21,23,27,31)
ll_recapture_group <- c(3,6,9,16,19,21,24,27)
  
# Plot 1. Time series of actuals
#p <- ggplot(pdat, aes(x=recap.ts, y=recap.obs))
p <- ggplot(pdat[!(tag_recapture_group %in% ll_recapture_group)], aes(x=recap.ts, y=recap.obs))
p <- p + geom_point(colour="red", na.rm=TRUE)
p <- p + geom_line(ggplot2::aes(y=recap.pred), na.rm=TRUE)
p <- p + facet_wrap(~tag_recapture_name, scales="free")
p <- p + xlab("Time") + ggplot2::ylab("Tag recaptures")
p

# Plot 2. Observed - predicted - scaled
pdat[,"diff":= recap.obs - recap.pred]
pdat <- pdat[,.(model=model, recap.ts=recap.ts, diff = diff / mean(recap.obs, na.rm=TRUE)), by=.(tag_recapture_name)]
ylab <- "Obs. - pred. recaptures (scaled)"

# Spoof up approriate y ranges for each facet using geom_blank()
#dummydat <- pdat[,.(y = c(max(abs(diff), na.rm=T), -max(abs(diff), na.rm=T))), by=.(tag_recapture_name)]
#dummydat$x <- rep(c(min(pdat$recap.ts), max(pdat$recap.ts)), nrow(dummydat)/2)
    
p <- ggplot(pdat, ggplot2::aes(x=recap.ts, y=diff))
p <- p + geom_smooth(aes(colour=model), method = 'loess', formula = 'y~x', na.rm=TRUE, se=FALSE)
p <- p + scale_color_manual("Model",values=colour_values)
p <- p + facet_wrap(~tag_recapture_name, scales="free")
p <- p + geom_hline(aes(yintercept=0.0), linetype=2)
p <- p + xlab("Time") + ggplot2::ylab(ylab)
#if (show.legend==FALSE){
#  p <- p + theme(legend.position="none") 
#}

#-------------------------------------------------------------------
# Tag attrition
# Loses the time step

# Is region release or recapture region

#grouping_names <- c("Model", "period_at_liberty", "region","program")
#pdat <- tagrep[, .(recap.obs=sum(recap.obs, na.rm=TRUE), recap.pred=sum(recap.pred, na.rm=TRUE)) ,by=mget(grouping_names)]

# Could use the pdat for the tag returns over time plot

pdat <- tagrep[, .(recap.obs=sum(recap.obs, na.rm=TRUE), recap.pred=sum(recap.pred, na.rm=TRUE)) ,by=.(model, period_at_liberty, region, program)]
object.size(pdat)/1e6
# Start plot here

# Grouping by user choice
if (facet=="none"){
  grouping_names <- c("model", "period_at_liberty")
}
if (facet=="program"){
  grouping_names <- c("model", "period_at_liberty", "program")
}
if (facet=="region"){
  grouping_names <- c("model", "period_at_liberty", "region")
}
pdat <- pdat[, .(recap.obs=sum(recap.obs, na.rm=TRUE), recap.pred=sum(recap.pred, na.rm=TRUE)) ,by=mget(grouping_names)]

pdat[, diff:=recap.obs - recap.pred]
# To scale the difference don't group by period at liberty - keep other choices
grouping_names <- grouping_names[grouping_names!="period_at_liberty"] 
mean_recaptured <- pdat[,.(mean_obs_recap=mean(recap.obs, na.rm=TRUE)), by=mget(grouping_names)]
pdat <- merge(pdat, mean_recaptured)
pdat[, diff:= diff / mean_obs_recap]
ylab <- "Obs. - pred. recaptures (scaled)"

if (facet %in% c("none", "region")){
  pdat$program <- "All programs"
}
if (facet %in% c("none", "program")){
  pdat$region <- "All recapture regions"
}
  
# Need to pad out time series to avoid missing missing periods at liberty
padts <- expand.grid(period_at_liberty = seq(from=min(pdat$period_at_liberty), to=max(pdat$period_at_liberty), by= 1), program = sort(unique(pdat$program)), region = sort(unique(pdat$region)))
pdat <- merge(pdat, padts, by=colnames(padts), all=TRUE)

# 1. Number of tag returns (y) against period at liberty
# For the observed and predicted recaptures, NA is essentially 0,
# i.e. there were no recaptures, so set to 0
pdat[is.na(pdat$recap.pred), "recap.pred"] <- 0
pdat[is.na(pdat$recap.obs), "recap.obs"] <- 0
p <- ggplot(pdat, aes(x=period_at_liberty))
p <- p + geom_point(aes(y=recap.obs), colour="red")
p <- p + geom_line(aes(y=recap.pred))
if(facet=="program"){
  p <- p + facet_wrap(~program, scales="free")
}
if(facet=="region"){
  p <- p + facet_wrap(~region, scales="free")
}
p <- p + xlab("Periods at liberty (quarters)")
p <- p + ylab("Number of tag returns")
p <- p + ylim(c(0,NA))
  

# 2. Residuals - can by multiple models
colour_values <- palette.func(selected.model.names = model_names, ...)
# Start...
p <- ggplot(pdat, aes(x=period_at_liberty, y=diff))
p <- p + geom_smooth(aes(colour=model), method = 'loess', formula = 'y~x', na.rm=TRUE, se=FALSE)
p <- p + scale_color_manual("model",values=colour_values)
p <- p + geom_hline(aes(yintercept=0.0), linetype=2)
if(facet=="program"){
  p <- p + facet_wrap(~program, scales="free")
}
if(facet=="region"){
  p <- p + facet_wrap(~region, scales="free")
}
    p <- p + ylab(ylab)
    p <- p + xlab("Periods at liberty (quarters)")
  }

#-------------------------------------------------------------------
# Tag return proportion

# Sum recaptures by release region and month
recap_reg <- tagrep[,.(recap.pred = sum(recap.pred, na.rm=TRUE), recap.obs=sum(recap.obs, na.rm=TRUE)), by=c("rel.region", "region", "recap.month")]
recap_reg_sum <- tagrep[,.(recap.pred.sum = sum(recap.pred, na.rm=TRUE), recap.obs.sum=sum(recap.obs, na.rm=TRUE)), by=c("rel.region", "recap.month")]
  
# Merge together and get the proportion of recaptures 
# i.e. the total number of tags from region 1 that were recaptured, found out the proportion that was recaptured in each region
recap_reg <- merge(recap_reg, recap_reg_sum)
recap_reg[,c("pred.prop", "obs.prop") := .(recap.pred/recap.pred.sum, recap.obs/recap.obs.sum)]

  # Plot of the difference between predicted and observed proportion of tags returned by region of release
recap_reg[,"diff_prop" := obs.prop - pred.prop]
recap_reg[,"rel.region.name" := paste("Release region ", rel.region, sep="")]
recap_reg[,"Quarter" := as.factor((recap.month+1)/3)]
# Save here
# Plot from here

pdat <- recap_reg
no_grps <- length(unique(pdat$rel.region.name))
ylims <- pdat[,.(maxval = c(max(abs(diff_prop)), -max(abs(diff_prop)))), by=.(rel.region.name)]
dummydat <- data.frame(y=ylims$maxval, x=rep(c(min(pdat$region), max(pdat$region)), no_grps), rel.region.name = ylims$rel.region.name)
dummydat$y <- dummydat$y * 1.1



# Point plot
if(plot.type == "point"){
  p <- ggplot(pdat, aes(x=as.factor(region), y=diff_prop))
  p <- p + geom_point(aes(colour = Quarter), size=4)
  p <- p + facet_wrap(~rel.region.name, ncol=2, scales="free")
  p <- p + geom_hline(aes(yintercept=0.0), linetype=2)
  p <- p + xlab("Recapture region")
  p <- p + ylab("Observed proportion - predicted proportion")
  p <- p + geom_blank(data=dummydat, aes(x=x, y=y))
}

# Bar plot
if(plot.type == "bar"){
  p <- ggplot(pdat, aes(x=as.factor(region), y=diff_prop))
  p <- p + geom_bar(stat="identity", aes(fill = Quarter), colour="black", position=position_dodge())
  p <- p + facet_wrap(~rel.region.name, ncol=2, scales="free")
  p <- p + xlab("Recapture region")
  p <- p + geom_hline(aes(yintercept=0.0))
  p <- p + ylab("Observed proportion - predicted proportion")
  p <- p + geom_blank(data=dummydat, aes(x=x, y=y))
}

#-------------------------------------------------------------------
