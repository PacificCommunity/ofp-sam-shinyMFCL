# Catch size distribution tests
library(FLR4MFCL)
library(ggplot2)

source("read_length_fit_file.R")
source("fishery_map.R")

#fisheries <- unique(unlist(fishgrp_fisheries[fishgrps]))
#sub_fishery_map <- fishery_map[fishery_map$fishery %in% fisheries,]
#sub_fisheries <- sub_fishery_map$fishery
#sub_fishery_names <- sub_fishery_map$fishery_name

# lfit_list?
# plot.overall.composition.fit - in diags4MFCL

basedir <- "C:/Work/MFCL/ofp-sam-skj22/stepwise2022/"
model <- "M7"
lfit_file <- paste(basedir, model, "length.fit", sep="/")

#lfit <- length.fit.preparation(filename = lfit_file)
lfit <- read_length_fit_file(filename = lfit_file)

# Original - minus that whole save thing
#plot.overall.composition.fit(lfit=lfit, fisheries=fisheries, fishery_names = fishery_names)

lfit <- merge(lfit, fishery_map)

# The subset can be done in the app
fisheries <- 1:5
pdat <- subset(lfit, fishery %in% fisheries)
# No need for a function to make the plot? - just do in app 
# The real value is the lfit generation
bar_width <- pdat$length[2] - pdat$length[1]
p <- ggplot(pdat, aes(x=length))
# Observed as barchart
p <- p + geom_bar(aes(y=obs), fill="steelblue", colour="black", stat="identity", width=bar_width)
# Predicted as red line
p <- p + geom_line(aes(y=pred), colour="red", size=2)
p <- p + facet_wrap(~fishery_name, scales="free")
p <- p + xlab("Length (cm)") + ylab("Samples")
p <- p + theme_bw()
# Tighten the axes
#p <- p + scale_y_continuous(expand = c(0, 0))
#p <- p + scale_x_continuous(expand = c(0, 0))
p

# Can we do multiple models?
#models <- paste0("M", 1:7)

models <- paste0("M",c(2,3,4,5,6,7,"9X"))#,10,11,12))


lfits <- lapply(models, function(x) {
  filename <- paste(basedir, x, "length.fit", sep="/")
  read_length_fit_file(filename=filename, model_name=x)}
)
lfits <- do.call("rbind", lfits)

lfits <- merge(lfits, fishery_map)

save(lfits, file="../app/data/lfits.Rdata")

# This looks nice - keep this one

# Same Observed - different predicted
ps_fisheries <- c(2,5,8,14,15,19,20,25,26,29,30)
fisheries <- 1:5
pdat <- subset(lfits, fishery %in% ps_fisheries)
# No need for a function to make the plot? - just do in app 
# The real value is the lfit generation
#bar_width <- pdat$length[2] - pdat$length[1]
bar_width <- diff(unique(sort(pdat$length)))[1]
#p <- ggplot(pdat, aes(x=length))
m2dat <- subset(pdat, model_name == "M2")
p <- ggplot(m2dat, aes(x=length))
# Observed as barchart
#p <- p + geom_bar(data=subset(pdat, model_name == "M2"), aes(y=obs), fill="steelblue", colour="black", stat="identity", width=bar_width)
p <- p + geom_bar(aes(y=obs), fill="steelblue", colour="black", stat="identity", width=bar_width)
# Predicted as red line
#p <- p + geom_line(aes(y=pred, colour=model_name), size=2)
p <- p + geom_line(data=pdat, aes(y=pred, colour=model_name), size=1)
p <- p + facet_wrap(~fishery_name, scales="free", ncol=3)
#p <- p + facet_grid(fishery_name~model_name, scales="free")
p <- p + xlab("Length (cm)") + ylab("Samples")
p <- p + theme_bw()
# Tighten the axes
#p <- p + scale_y_continuous(expand = c(0, 0))
#p <- p + scale_x_continuous(expand = c(0, 0))
p


#-------------------------------------------------------------------
# STOP


## Function to read and process the length.fit file
## This basically all taken from Yukio's code plot_overall.composition.fit.gg.r
## Some small edits from me
#
## Checked 19/04/2022 - looks OK
#
## Run this outside of the app to generate a list or data.table of length fits
#
##' Read in and process length.fit file
##' 
##' Hoovers out the observed ann predicted composite (i.e. summed over time) catches at length from the length.fit file.
##' It should also work with the weight.fit file but hasn't yet been tested.
##' Does not handle multispecies.
##' @param filname The filename of the length.fit file, including the location.
##' @export length.fit.preparation
##' @rdname length.fit.preparation 
##' @name length.fit.preparation
##' @import FLR4MFCL
#length.fit.preparation <- function(filename){
#  # Read the whole dang thing - it's pretty huge
#  dat <-readLines(filename)
#  # This whole first section is sort of metadata stuff
#  # Pulling out length vectors, number of fisheries etc
#  # Get the version number - needed because older versions have different spacing
#  version<-ifelse(strsplit(dat[1],split=" +")[[1]][1]=='#', strsplit(dat[1],split=" +")[[1]][3], 1)
#  # Determine the number of fisheries from file header
#  # Could possibly interrogate from dat but we'll use Yukio's code for now
#  Nfsh <- scan(filename, nlines=1, skip=ifelse(version==1,2,3)) - 1
#  # Determine the number of lines in the matrix for each fishery, from file header
#  Nskips <- scan(filename, nlines=1, skip=ifelse(version==1,4,5))
#  # Get parameters no. bins, first bin size, bin width
#  size.pars <- scan(filename, nlines=1, skip=ifelse(version==1,1,2))  
#  # Construct the size bins from the file header
#  sizebins <- seq(from=size.pars[2], by=size.pars[3], length.out=size.pars[1])
#  # Figure the number of species in the length.fit and stop if more than 1
#  fishSpPtr <- ifelse(version>=2, scan(filename,nlines=1,skip=7), NA)
#  nsp <- ifelse(version>=2,length(unique(fishSpPtr)),1)
#  # Change this to stop can only do with single species / sex model for now
#  if(nsp>1){
#    stop("More than 1 species/sex in the file. Not able to handle yet. Stopping.")
#  }
#  # Default is all of them, 1:31
#  VecFsh <-1:Nfsh
#  # Identify the lines of the observed size frequencies for the fisheries
#  # These are lines under the # fishery totals bit at the bottom of the file
#  LineKeep <- (VecFsh-1) * (Nskips + 6) + 1
#  
#  # Now we start processing the data
#  # All the blocks in the file with # fishery 1 etc, are observed and predicted
#  # numbers at length at age in each time step (see manual)
#  # Only want the stuff at bottom of the file which has the data summed over time
#  # Read in the file as text - run time could be reduced by only reading in from '# fishery totals' down but no skiplines argument in readLines - will have a hunt
#  # Remove all unwanted data above the fishery totals
#  dat <- dat[(grep("totals",dat)+4):length(dat)]
#
#  # Get the observed data
#  # This is the only observed data we want keep - pulls out vector for the fishery then skips down to the next fishery and grabs vector, etc. etc.
#  # This should match the freq file? It does
#  dat.obs <- dat[LineKeep]
#  # Get it in the right format and transpose
#  # To a matrix length class (rows), fishery (column)
#  #dat.obs <- as.data.frame(t(read.table(text=dat.obs, nrows=length(LineKeep))))
#  dat.obs <- t(read.table(text=dat.obs, nrows=length(LineKeep)))
#  # Do the same for the predicted
#  dat.pred <- dat[LineKeep+1]
#  dat.pred <- t(read.table(text=dat.pred, nrows=length(LineKeep)))
#  
#  # Put together into data.frame
#  out <- data.frame(fishery=rep(VecFsh,each=size.pars[1]),
#                    length=rep(sizebins, Nfsh),
#                    obs = c(dat.obs),
#                    pred = c(dat.pred))
#  return(out)
#}
#
#
##---------------------------------------------------------
## The plot
#
## Orig code in diags4MFCL package
##' Plot composite catch-at-length data for each fishery.
##' 
##' Plot composite (all time periods combined) observed and predicted catch-at-length for each fishery.
##' The table comes from the length.fit file and has already been processed using the
##' \code{length.fit.preparation()} function (which is based on Yukio's code).
##' This may also work with weight.fit data but has not been tested yet.
##' 
##' @param lfit A data.frame of observed and predicted composite catch-at-length data (generated using \code{length.fit.preparation()}).
##' @param fisheries The numbers of the fisheries to plot.
##' @param fishery_names The names of the fisheries to plot.
##' @param save.dir Path to the directory where the outputs will be saved
##' @param save.name Name stem for the output, useful when saving many model outputs in the same directory
##' @param ncol Number of columns to plot across. Default is ggplot2 default.
##' @param xlab Label for the xaxis.
##' @export
##' @import FLR4MFCL
##' @import magrittr
##plot.overall.composition.fit <- function(lfit, fisheries, fishery_names, save.dir, save.name, ncol=NULL,xlab="Length (cm)"){
#plot.overall.composition.fit <- function(lfit, fisheries, fishery_names, ncol=NULL,xlab="Length (cm)"){
#  # Subset out the desired fisheris
#  pdat <- subset(lfit, fishery %in% fisheries)
#  
#  # Bring in the fishery names
#  fishery_names_df <- data.frame(fishery = fisheries, fishery_names = fishery_names)
#  pdat <- merge(pdat, fishery_names_df)
#  
#  bar_width <- pdat$length[2] - pdat$length[1]
#  # Plot up
#  p <- ggplot(pdat, aes(x=length))
#  # Observed as barchart
#  p <- p + geom_bar(aes(y=obs), fill="blue", colour="blue", stat="identity", width=bar_width)
#  # Predicted as red line
#  p <- p + geom_line(aes(y=pred), colour="red", size=1)
#  if(is.null(ncol))
#  {
#      p <- p + facet_wrap(~fishery_names, scales="free")
#  } else {
#      p <- p + facet_wrap(~fishery_names, scales="free", ncol=ncol)
#  }
#  p <- p + xlab("Length (cm)") + ylab("Samples")
#  p <- p + ggthemes::theme_few()
#  # Tighten the axes
#  p <- p + scale_y_continuous(expand = c(0, 0))
#  p
#  
##  save_plot(save.dir, save.name, plot=p, width = 16, height = 9)
#  
#  return(p)
#}
