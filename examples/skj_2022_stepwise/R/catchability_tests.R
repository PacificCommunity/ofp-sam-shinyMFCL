# Catchability plots
# Using the obsXX and predXX files for the catch conditioned models.
# Email from John - via Claudio - 06/04/2022

# I thought I would show you the fits for the effort: fishing mortality relationships for all the fisheries that have effort data. This is essentially plotting catchability. They are the obsxx and predxx files that are produced for each fishery. The fisheries that have constant catchability (and hence the predictions are flat, but with seasonality) have the red border. These are providing the indices of abundance. The fits are pretty nice overall. In particular, the fits for the PS fisheries may be of interest from an effort creep perspective. This would be one of the plots that I would suggest go in the hierophant. It may be useful to also plot the de-seasonalised plot perhaps to better show the trends.

# Catchability tests
library(FLR4MFCL)
library(ggplot2)
library(data.table)

#source("fishery_map.R")
load("../app/data/fishery_map.Rdata")

#One at a time
#basedir <- "C:/Work/MFCL/ofp-sam-skj22/stepwise2022/"
#model <- "M2"

#files <- list.files(paste(basedir, model, sep="/"))
## Use little hat ^ to match the beginning of the string
#obsfiles <- files[grep("^obs", files)]
#predfiles <- files[grep("^pred", files)]
#
## Read in example
#obs <- read.table(paste(basedir, model, obsfiles[12], sep="/"), sep="", col.names=c("ts", "log_q"))
#pred <- read.table(paste(basedir, model, predfiles[12], sep="/"), sep="", col.names=c("ts", "log_q"))
## 185 lines - timesteps?
## q is on log scale?
#
## Read in all from a single model and smush into data.frame
#obs <- lapply(obsfiles, function(x) read.table(paste(basedir, model, x, sep="/"), sep="", col.names=c("ts", "log_q")))
#names(obs) <- obsfiles
#obs <- data.table::rbindlist(obs, idcol="obsfile")
#obs$type <- "obs"

# Where can we get time ranges of the different fisheries from?
# A catch rep?
# A par file


# Do them all

basedir <- "C:/Work/MFCL/ofp-sam-skj22/stepwise2022/"
models <- list.dirs(basedir, recursive = FALSE, full.names=FALSE)


#frq  <- read.MFCLFrq(paste(basedir, "M2", "skj.frq", sep="/")) # Weird Windows formatting
#frq  <- read.MFCLFrq(paste(basedir, "M2", "skj2.frq", sep="/")) # Where did skj2 come from?

#test <- read.MFCLFrq("Z:/skj_MSE/OMs/testMFCL2080/A0B0C0D0E0F0/proj.frq")

# All models and fisheries
models <- paste0("M", 2:7)
models <- paste0("M", 2:12)
models[models == "M9"] <- "M9X"

#models <- "M8" # empty
#models <- "M9X"
models <- paste0("M",c(2,3,4,5,6,7,"9X",10,11,12))

model <- "M11"

catchability <- lapply(models, function(model){
  cat("Model: ", model, "\n")
  files <- list.files(paste(basedir, model, sep="/"))
  #if (file.exists(paste(basedir, model, "skj2.frq", sep="/"))){
  #  frq <- read.MFCLFrq(paste(basedir, model, "skj2.frq", sep="/"))
  # Careful - same frq for all models - only used for year range
  frq <- read.MFCLFrq(paste(basedir, "M2", "skj2.frq", sep="/"))
  #} else {
  #  frq <- read.MFCLFrq(paste(basedir, model, "skj.frq", sep="/"))
  #}
  rfrq <- realisations(frq)
  rfrq$season <- (rfrq$month-2)/12
  rfrq$ts <- rfrq$year + rfrq$season
  # Use little hat ^ to match the beginning of the string
  obsfiles <- files[grep("^obs", files)]
  predfiles <- files[grep("^pred", files)]
  # Only include those with a numeric
  obsfiles <- obsfiles[grep("([0-9]+).*$", obsfiles)] 
  predfiles <- predfiles[grep("([0-9]+).*$", predfiles)] 
  
  file_number <- 1:length(obsfiles)
  # Process each fishery
  obspredtemp <- lapply(file_number, function(x){
    cat("Model: ", model, " File number: ", x, "\n")
    # Files are not always by fishery number due to character ordering
    fishery_number <-  as.numeric(unlist(strsplit(obsfiles[x], "obs"))[2])
    obsfilename <- obsfiles[x]
    obstemp <- read.table(paste(basedir, model, obsfilename, sep="/"), sep="", col.names=c("ts", "obs_log_q"))
    # Hack - because some fisheries have no effort
    # Make sure you drop the effort == -1
    rfrqtemp <- subset(rfrq, fishery==fishery_number & effort != -1.0)
    # If there is no effort data in the frq - skip
    if(dim(rfrqtemp)[1] == 0){
      return(data.frame())
    }
    # Make sure order is by timestep
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

catchability[,c("obs_q", "pred_q") := .(exp(obs_log_q), exp(pred_log_q))]

# Remove seasonality
catchability_annual <- catchability[, .(obs_q = mean(obs_log_q), pred_q = mean(pred_log_q)), by=.(year, fishery, fishery_name, model, region)]


# Look at difference - don't see the trend in catchability - but better for evaluating fit?
# See catch pred - obs

catchability[,diff := .(obs_q - pred_q)]

# Scale by total catchability by fishery and Model
catchability[, scale_diff :=  diff / mean(obs_q, na.rm=TRUE), by=.(model, fishery)]

# Save the catchability objects
save(catchability, catchability_annual, file="../app/data/catchability_data.Rdata")


# Experimental plot
ps_fisheries <- c(2,5,8,14,15,19,20,25,26,29,30)
ps_ass_fisheries <- c(14, 19, 25, 29)
ps_unass_fisheries <- c(15, 20, 26, 30)
pl_fisheries <- c(1,4,7,13, 18, 24, 28)
fisheries <- ps_ass_fisheries
models <- c("M2", "M7")
models <- "M7"

# Plot
p <- ggplot(subset(catchability, fishery %in% fisheries & model %in% models), aes(x=ts))
p <- p + geom_point(aes(y=exp(obs_log_q)), colour="steelblue")
p <- p + geom_line(aes(y=exp(pred_log_q)), colour="tomato")
p <- p + facet_grid(fishery_name ~ model, scales="free")
p <- p + theme_bw()
p <- p + xlab("Year") + ylab("Catchability")
p

# Plot
p <- ggplot(subset(catchability_annual, fishery %in% fisheries & model %in% models), aes(x=year))
p <- p + geom_point(aes(y=obs_q), colour="steelblue")
p <- p + geom_line(aes(y=pred_q), colour="tomato")
p <- p + facet_grid(fishery_name ~ model, scales="free")
p <- p + theme_bw()
p <- p + xlab("Year") + ylab("Catchability")
p






  
models <- c("M2", "M7")
p <- ggplot(subset(catchability, fishery %in% fisheries & model %in% models), aes(x=ts, y=scale_diff))
p <- p + geom_point(na.rm=TRUE, alpha=0.6)
# Add a smoother
p <- p + geom_smooth(colour="red", method = 'loess', formula = 'y~x', na.rm=TRUE, se=FALSE)
p <- p + facet_grid(fishery_name ~ model, scales="free")
p <- p + theme_bw()
p <- p + xlab("Year") + ylab("Obs. - pred. catchability (scaled)")
p



  

