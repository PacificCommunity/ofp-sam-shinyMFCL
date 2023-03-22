# CPUE (observed - predicted)
# Based on SALSA
library(FLR4MFCL)
library(data.table)
library(ggplot2)

source("fishery_map.R")

basedir <- "C:/Work/MFCL/ofp-sam-skj22/stepwise2022/"

# M2 doesn't have survey / index fisheries.
# So no CPUE plot.

model <- "M9X"


frq <- read.MFCLFrq(paste(basedir, model, "skj2.frq", sep="/"))
rfrq <- realisations(frq)

# Load the par and rep files
lf <- list.files(paste(basedir, model, sep="/"))
parfiles <- lf[grep(".par$", lf)]
# Find the biggest par file
biggest_par <- as.character(max(as.numeric(substr(parfiles,1,2))))
if(length(biggest_par)==1){
  biggest_par <- paste0("0",biggest_par)
}
biggest_par <- paste0(biggest_par,".par")
par <- read.MFCLPar(paste(basedir, model, biggest_par, sep="/"))
# Load the rep
repfile <- paste0("plot-", biggest_par, ".rep")
rep <- read.MFCLRep(paste(basedir, model, repfile, sep="/"))

# According to John H (email 12/05/2022) use just cpue_obs and cpue_pred slots in rep
# slotNames
slotNames(rep)
dim(cpue_obs(rep))
# Seasonal CPUE - how to annualise..? keep seasonal
# Use catch pred and catch obs to back calc effort?
cpue <- as.data.table(cpue_obs(rep))
cpue_pred <- as.data.table(cpue_pred(rep))
setnames(cpue, "value", "cpue_obs")
cpue[,cpue_pred := (cpue_pred$value)]
setnames(cpue, "unit", "fishery")
cpue[, ts := .(as.numeric(year) + (as.numeric(season)-1)/4)]

# These are on the log scale?
# Only plot the index fisheries
p <- ggplot(cpue[fishery %in% 32:39], aes(x=ts))
p <- p + geom_point(aes(y=exp(cpue_obs)), colour="steelblue1")
p <- p + geom_line(aes(y=exp(cpue_pred)), colour="tomato")
p <- p + facet_wrap(~fishery, scales="free")
p <- p + ylab("CPUE") + xlab("Year")
p <- p + theme_bw()
p

# Look at CPUE obs and pred files
cobs <- readLines(paste(basedir, model, "cpue_obs", sep="/"))
cobs <- lapply(cobs, function(x){
  # Split by " " and drop first four elements as these are the fishery X bits
  x <- as.numeric(unlist(strsplit(x, " "))[-(1:4)])
  # Drop the NAs
  x <- x[!is.na(x)]
  return(x)
})

cpred <- readLines(paste(basedir, model, "cpue_pred", sep="/"))
cpred <- lapply(cpred, function(x){
  # Split by " " and drop first four elements as these are the fishery X bits
  x <- as.numeric(unlist(strsplit(x, " "))[-(1:4)])
  # Drop the NAs
  x <- x[!is.na(x)]
  return(x)
})

# Are these the same as in the rep file?
plot(cpue[fishery==32]$cpue_obs,  cobs[[1]])
plot(cpue[fishery==32]$cpue_pred, cpred[[1]])

plot(cpue[fishery==33]$cpue_obs,  cobs[[2]])
plot(cpue[fishery==33]$cpue_pred, cpred[[2]])
# Yes
# So fuck it - use the ones in the rep file
  
# I'd still like to understand what is happening in the M2 files - but there are no index fisheries. How does that even work then?

# Get the grouped fishery obs and pred too?

read_grouped_cpue_file <- function(name="grouped_cpue_obs"){
  alldat <- readLines(paste(basedir, model, name, sep="/"))
  out <- data.frame()
  for (i in 1:length(alldat)){
    raw <- unlist(strsplit(alldat[i], " "))
    group <- raw[3]
    dat <- as.numeric(raw)[-(1:4)]
    dat <- dat[!is.na(dat)]
    dftemp <- data.frame(group=group, cpue=dat)
    out <- rbind(out, dftemp)
  }
  return(out)
}

testobs <- read_grouped_cpue_file(name="grouped_cpue_obs")
testpred <- read_grouped_cpue_file(name="grouped_cpue_pred")
# But what is the time series? eh? Too hard
  



## Try in the cpue_obs and cpue_pred files?
## What is happening in these files?
## Note difference in file naming convention - M2 has different file structure - ignore file
#cobs <- scan(file=paste(basedir, "M2", "cpue.obs", sep="/")) # M2
## M2 had 6296
## Don't know what these are for
###Hxcept half are indices and half are values?
#idids <- seq(from=1, to=length(cobs), by=2)
#valids <- seq(from=2, to=length(cobs), by=2)
#ids <- cobs[idids]
#table(ids)
#vals <- cobs[valids] # 3148 values
### Look at time series of index fisheries and have a reckon - but no index fisheries in frq
## 
## 188 time steps in the freq








# Stop
#--------------------------------------------------------------------------------
# Add timestep column for plotting - ignoring week


rfrq$ts <- rfrq$year + (rfrq$month-1)/12 + 1/24  # month is mid-month
# Tidy up missing values
rfrq$effort[rfrq$effort < 0] <- NA
rfrq$catch[rfrq$catch< 0] <- NA
# Get observed CPUE - simple
rfrq$obs_cpue <- rfrq$catch / rfrq$effort


# Get model-predicted CPUE
# Get effort devs from the par file and apply them to the observed effort
# model predicted CPUE = effort * exp(edevs)
edevs <- unlist(effort_dev_coffs(par))
# Problem is that for catch conditioned models the effort_dev_coffs are 0
any(abs(edevs) > 0)
# For M1 (old style) we're OK
# For M2+  all are 0

# What is model predicted CPUE in the catch conditioned case
# Use catch dev coffs?
cdevs <- unlist(catch_dev_coffs(par))
any(abs(cdevs) > 0)

# So...


#edevs <- unlist(lapply(par_list, function(x) return(unlist(effort_dev_coffs(x)))))
  #edevs <- effort_dev_coffs(par)
  
  # Bring edevs into freq - requires dims to be the same as the fishing realisations
  if (length(edevs) != dim(frqreal)[1]){
    stop("Length of effort devs does not match nrows of fishing realisations.")
  }
  
  # Force the order to be same as the effort devs so we can just unlist edevs in
  frqreal <- frqreal[order(frqreal$model, frqreal$fishery, frqreal$ts), ]
  frqreal$edev <- edevs
  frqreal$est_effort <- frqreal$effort * exp(frqreal$edev)
  frqreal$est_cpue <- frqreal$catch / frqreal$est_effort

  # Normalise the CPUE
  # Add by normalisation columns by reference
  frqreal[, c("norm_obs_cpue", "norm_est_cpue") := .(obs_cpue / mean(obs_cpue, na.rm=TRUE), est_cpue / mean(est_cpue, na.rm=TRUE)), by=.(model,fishery)]
  
  # Subset out the required fisheries
  pdat <- frqreal[fishery %in% fisheries,]
  # Name the fisheries
  if(length(fisheries) != length(fishery_names)){
    stop("fisheries should be the same length as fishery_names")
  }
  fishery_names <- data.frame(fishery = fisheries, fishery_names = fishery_names)
  pdat <- merge(pdat, fishery_names)



#--------------------------------------------------------------

# From SALSA plots.R

get_pred_obs_cpue <- function(frqreal_list, par_list, fisheries, fishery_names){
  
  frqreal <- rbindlist(frqreal_list, idcol="model")
  #par <- rbindlist(par_list, idcol="model")
  
  # Add timestep column for plotting - ignoring week
  frqreal$ts <- frqreal$year + (frqreal$month-1)/12 + 1/24  # month is mid-month
  # Tidy up missing values
  frqreal$effort[frqreal$effort < 0] <- NA
  frqreal$catch[frqreal$catch< 0] <- NA
  # Get observed CPUE - simple
  frqreal$obs_cpue <- frqreal$catch / frqreal$effort
  
  # Get model-predicted CPUE
  # Get effort devs from the par file and apply them to the observed effort
  # model predicted CPUE = effort * exp(edevs)
  edevs <- unlist(lapply(par_list, function(x) return(unlist(effort_dev_coffs(x)))))
  #edevs <- effort_dev_coffs(par)
  
  # Bring edevs into freq - requires dims to be the same as the fishing realisations
  if (length(edevs) != dim(frqreal)[1]){
    stop("Length of effort devs does not match nrows of fishing realisations.")
  }
  
  # Force the order to be same as the effort devs so we can just unlist edevs in
  frqreal <- frqreal[order(frqreal$model, frqreal$fishery, frqreal$ts), ]
  frqreal$edev <- edevs
  frqreal$est_effort <- frqreal$effort * exp(frqreal$edev)
  frqreal$est_cpue <- frqreal$catch / frqreal$est_effort

  # Normalise the CPUE
  # Add by normalisation columns by reference
  frqreal[, c("norm_obs_cpue", "norm_est_cpue") := .(obs_cpue / mean(obs_cpue, na.rm=TRUE), est_cpue / mean(est_cpue, na.rm=TRUE)), by=.(model,fishery)]
  
  # Subset out the required fisheries
  pdat <- frqreal[fishery %in% fisheries,]
  # Name the fisheries
  if(length(fisheries) != length(fishery_names)){
    stop("fisheries should be the same length as fishery_names")
  }
  fishery_names <- data.frame(fishery = fisheries, fishery_names = fishery_names)
  pdat <- merge(pdat, fishery_names)
  
  return(pdat)
}




# From SALSA
output$plot_pred_obs_cpue <- renderPlot({
  
  # Just IDX fisheries
  fishgrps <- "IDX"
  fisheries <- unique(unlist(fishgrp_fisheries[fishgrps]))
  sub_fishery_map <- fishery_map[fishery_map$fishery %in% fisheries,]
  sub_fisheries <- sub_fishery_map$fishery
  sub_fishery_names <- sub_fishery_map$fishery_name
  
  # Data calculated above - just subset
  pdat <- subset(pred_obs_cpue, model %in% input$model_select & fishery_names %in% sub_fishery_names)
  
  p <- ggplot(pdat, aes(x=ts))
  p <- p + geom_point(aes(y=norm_obs_cpue, colour=penalty), na.rm=TRUE)
  p <- p + geom_line(aes(y=norm_est_cpue), na.rm=TRUE)
  p <- p + facet_wrap(~fishery_names, scales="free")
  p <- p + ylim(c(0,NA))
  p <- p + scale_colour_gradient(low = 'royalblue', high = 'lightskyblue1')
  p <- p + ggthemes::theme_few()
  p <- p + xlab("Time") + ylab("Normalised CPUE")
  p <- p + theme(legend.position = "none")
  return(p)
  
})
