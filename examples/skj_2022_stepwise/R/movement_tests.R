#--------------------------------------------------------------------------
# Experimenting with movement plot

# Stolen wholesale from Rob's mi-wantem-luk repo:
# https://github.com/PacificCommunity/mi-wantem-luk/tree/main/R
# chord.r
# sankey.r

# Notes:
# Check that from / to are the right way round. Might need to fix the labels in FLR4MFCL.
# Might need a transpose in the chord and sankey plot code.
# Check tag movement too - I think that this is righ  t

# 14/04/2022

#--------------------------------------------------------------------------

# Check orientation of movement matrices (diff_coffs_age_period())
# Suspect that 'From' should sum to 1

library("FLR4MFCL")
#https://github.com/mattflor/chorddiag
#devtools::install_github("mattflor/chorddiag")
# Or
#install.github("mattflor/chorddiag")
#install.packages("igraph")
#install.packages("networkD3")
#library("chorddiag")

# Alt
#https://github.com/PacificCommunity/mi-wantem-luk
#devtools::install_github("PacificCommunity/mi-wantem-luk")
library(miWantemLuk)
# Gives a load of conflicts connected to the tidyverse - might be easier to just steal the 
# R code from the package (see below)

# Load some test data from ofp-sam-skj22
# C:\Work\MFCL\ofp-sam-skj22\stepwise2022\M1
# The Git repo
basedir <- "C:/Work/MFCL/ofp-sam-skj22/stepwise2022/"
model <- "M7"

# devtools::install_github("mattflor/chorddiag")

# MFCL tag data
tag  <- read.MFCLTag(paste(basedir, model, "skj.tag", sep="/"))
frq  <- read.MFCLFrq(paste(basedir, model, "skj.frq", sep="/"))
par  <- read.MFCLPar(paste(basedir, model, "07.par", sep="/")) # 2.7 Mb per model
reg  <- read.MFCLRegion(paste(basedir, model, "07.par", sep="/")) # 75 kb per model
# diff_coffs_age_period(par) - 36 kb per model

# Check these are the right way round - from / to
diff_coffs_age_period(par)

# Looks awesome
# Is SKJ movement estimated by age
# Single age and season - possible to combine over ages / seasons?
chordMovePlot(par, age=1, season=1)
# Is from / to correct?
chordMovePlot(tag, frq)

# Also looks awesome
# Single age and season - possible to combine over ages / seasons?
sankeyMovePlot(par, age=20, season=1)
sankeyMovePlot(reg, age=20, season=1)
sankeyMovePlot(tag, frq)
# What do the numbers mean?

# But the problem with these is needing to use the entire par, frq and tag files.
# Can we skinny down?
# Can just use MFCLRegion (see above)

#---------------------------------------------------------------------------------

# Movement lines similar to SALSA
# Needs move_coef. Probably just the diff_coffs_age_period
# But maybe no age or season structure to movement so can simplify
# Looking at the data, season is different but ages are the same?
# Will this always be the case?


# Ugly mess of dplyr and data table shite
#library(magrittr)    
library(data.table)
get_move_coefs <- function(par_list){
  move_coef <- lapply(par_list, function(x) {
          dcap <- diff_coffs_age_period(x)
          move_coef_temp <- as.data.table(dcap)
          #move_coef_temp <- as_tibble(dcap, rownames = "to") %>%
          #  pivot_longer(cols = -from, names_sep = "\\.", names_to = c("from", "age", "period")) 
          return(move_coef_temp)
        })
  move_coef <- rbindlist(move_coef, idcol="model")
  move_coef$age <- as.numeric(move_coef$age)
  # I don't think we need this anymore
  #move_coef %<>% rename(to=from, from=to)
  # Check from sums to 1
  #move_coef %>% group_by(from, age, period) %>% summarise(value = sum(value)) # Should sum to 1
  #move_coef %>% group_by(to, age, period) %>% summarise(value = sum(value)) # Should not sum to 1
  move_coef$move <- paste("From R", move_coef$from, " to R", move_coef$to, sep="")
  #move_coef$from <- paste("From R", move_coef$from, sep="")
  #move_coef$to <- paste("To R", move_coef$to, sep="")
  move_coef$from <- paste("R", move_coef$from, sep="")
  move_coef$to <- paste("R", move_coef$to, sep="")
  #move_coef$Season <- as.factor(move_coef$period)
  move_coef$Season <- as.numeric(move_coef$period)
  setnames(move_coef, old=c("age", "to", "from"), new=c("Age", "To", "From"))
  #move_coef %<>% rename(Age=age, From=from, To=to)
  return(move_coef)
}

# These appear to be the right way round now
diff_coffs_age_period(reg)[,,1,1]
sum(diff_coffs_age_period(reg)[,1,1,1])

move_coef <- get_move_coefs(list(reg))
# This does not look right
subset(move_coef, From == "R1" & Age == 1 & Season==1)
sum(subset(move_coef, From == "R1" & Age == 1 & Season==1)$value)

# Looks OK


# Shite
#p <- ggplot(subset(move_coef, model==1), aes(x=Age, y=value))
#p <- p + geom_line(aes(colour=Season), size=1.1)
#p <- p + facet_wrap(~move, ncol=4, dir="v")
#p <- p + ylim(c(0,1))
#p <- p + xlab("Age class") + ylab("Movement coefficient")
#p <- p + theme_bw()
#p

# Assume that movement is same across ages
# 8 x 8 movement grid, 4 seasons
# subset(move_coef, To == 1 & From == 1)

p <- ggplot(subset(move_coef, model=="M2" & Age==1 & Season==1), aes(x=To, y=value))
p <- p + geom_bar(aes(fill=To), stat="identity")
p <- p + facet_wrap(~From, ncol=2)
p
# Bars sum to 1

# Are seasons different
subset(move_coef, model=="M2" & From == "R1" & Age==1 & Season==1)
# Yes - fuck
subset(move_coef, model=="M2" & From == "R1" & Age==1 & Season==2)
# Ages? - No
subset(move_coef, model=="M2" & From == "R1" & Age==2 & Season==1)

# So we have From / To / Season - 3D
p <- ggplot(subset(move_coef, model=="M2" & Age==1), aes(x=To, y=value))
p <- p + geom_bar(aes(fill=To), stat="identity")
#p <- p + facet_grid(Season~From)
p <- p + facet_grid(From~Season)
p

# Add this one
p <- ggplot(subset(move_coef, model=="M2" & Age==1), aes(x=Season, y=value))
p <- p + geom_bar(aes(fill=To), stat="identity")
#p <- p + facet_grid(Season~From)
p <- p + facet_grid(To~From)
p

p <- ggplot(subset(move_coef, Age==1 & model %in% c("M2", "M6")), aes(x=Season, y=value))
p <- p + geom_bar(aes(fill=model), stat="identity", position="dodge")
#p <- p + facet_grid(Season~From)
p <- p + facet_grid(To~From)
p


p <- ggplot(subset(move_coef, model=="M2" & Age==1), aes(x=Season, y=value))
p <- p + geom_bar(aes(fill=To), position="stack", stat="identity")
p <- p + facet_wrap(~From)
p



# Can we do a stacked bar chart - total height = 1? Colour coded by To region?


#p <- ggplot(subset(move_coef, model==1 & Age==1 & Season==1), aes(x=From, y=value))
#p <- p + geom_bar(aes(fill=To), stat="identity")
#p <- p + facet_wrap(~To)
#p

# What do these numbers even mean?
#subset(move_coef, To == 1 & Season==1) # All ages different
#subset(move_coef, From == 1 & Season==1) # All ages the same
#
#library(dplyr)
#move_coef %>% group_by(Season, Age, From) %>% summarise(value = sum(value)) # Sums to 1
## Everything leaving sums to 1
#sum(subset(move_coef, From == 1 & Season==1 & Age == 1)$value)
#subset(move_coef, From == 1 & Season==1 & Age == 1) # So here, 34% leave 1 and go to 3, 25% leave 1 and go to 1


#move_coef %>% group_by(Season, Age, To ) %>% summarise(value = sum(value)) # Does not sum to 1
# Everything going into a region does not sum to 1

# Chord diagram
# Could save a list of diff_coffs_age_period
# As well as the data.table above to make the bar chart?


#m <- diff_coffs_age_period(reg)[,,1,1]
## Wrong
#chorddiag(m)
## Right - from needs to be in the rows, to the columns
#chorddiag(t(m))

# Or make a matrix from the data.table?
#dat <- move_coef[model==1 & Age == 1 & Season == 1]
#ddat <- dcast(dat, To ~ From, value.var = "value")
#dcast(dat, To ~ From)

# Possible to include
# Note that subset is contained in the dcast - it's fast
ddat <- dcast(move_coef, From ~ To, subset=.(model==1 & Age == 1 & Season == 1), value.var = "value")
pdat <- as.matrix(ddat, rownames="From")
#pdat <- signif(pdat, 3)
pdat <- round(pdat, 3)
chorddiag(pdat, tickInterval = 0.05)


# THIS BIT HERE
# Get a list of the par files
#get_move_coefs <- function(par_list){

basedir <- "C:/Work/MFCL/ofp-sam-skj22/stepwise2022/"
#model <- "M7"
models <- paste0("M",c(2,3,4,5,6,7,"9X"))

move_coef <- list()
for (model in models){
  cat("Model: ", model, "\n")
  # ID the par file
  lf <- list.files(paste(basedir, model, sep="/"))
  parfiles <- lf[grep(".par$", lf)]
  # Find the biggest one
  # Careful model 9x has a weird par file
  biggest_par <- as.character(max(as.numeric(gsub(".par", "", parfiles)), na.rm=TRUE))
  if(length(biggest_par)==1){
    biggest_par <- paste0("0",biggest_par)
  }
  biggest_par <- paste0(biggest_par,".par")
  reg <- read.MFCLRegion(paste(basedir, model, biggest_par, sep="/"))
  #reglist[[eval(model)]] <- reg
  dcap <- diff_coffs_age_period(reg)
  move_coef[[eval(model)]] <- as.data.table(dcap)
}


move_coef <- rbindlist(move_coef, idcol="model")
move_coef$age <- as.numeric(move_coef$age)
move_coef$move <- paste("From R", move_coef$from, " to R", move_coef$to, sep="")
move_coef$from <- paste("R", move_coef$from, sep="")
move_coef$to <- paste("R", move_coef$to, sep="")
move_coef$Season <- as.numeric(move_coef$period)
setnames(move_coef, old=c("age", "to", "from"), new=c("Age", "To", "From"))

save(move_coef, file="../app/data/move_coef.Rdata")

ddat <- dcast(move_coef, From ~ To, subset=.(model=="M2" & Age == 1 & Season == 1), value.var = "value")
pdat <- as.matrix(ddat, rownames="From")
#pdat <- signif(pdat, 3)
pdat <- round(pdat, 3)
chorddiag(pdat, tickInterval = 0.05)

# How to plot multiple ones?

par(mfrow=c(2,2))
chorddiag(pdat, tickInterval = 0.05)


            
#----------------------------------------------------------------
# Chord plot - stolen from Rob

#' chordMovePlot
#'
#' Produces a Chord plot of movement 
#'
#' @param obj:    An object either of class MFCLRegion or of class MFCLTag
#' @param frq:    An object of MFCLFrq. Defaults to NULL but required if obj=MFCLTag
#' @param age:    By default the first age is plotted
#' @param season: By default the first season is plotted
#'
#'
#' @return A plot of movement between assessment regions as determined either by the tag data or the estimated movement from the assessment.
#' 
#' 
#' @export
#' @docType methods
#' @rdname par-methods
#'


setGeneric('chordMovePlot', function(obj, ...) standardGeneric('chordMovePlot')) 


setMethod("chordMovePlot", signature(obj="MFCLRegion"), 
          function(obj, age=1, season=1, ...){
            
            par <- obj
            args <- list(...)
            
            m       <- diff_coffs_age_period(par)[,,age,season]
            regions <- paste("Region", 1:dim(m)[1])
            
            
            dimnames(m) <- list(from=regions, to=regions)
            #groupColors <- c("#000000", "#FFDD89", "#957244", "#F26223","#000000", "#FFDD89", "#957244", "#F26223")
            rnd <- sample(1:dim(diff_coffs_age_period(par))[1], dim(diff_coffs_age_period(par))[1], replace=F)
            rnd <-        1:dim(diff_coffs_age_period(par))[1]
            groupColors <- colorRampPalette(c("goldenrod","brown","beige"))(dim(diff_coffs_age_period(par))[1])[rnd]
            #groupColors <- viridis(dim(diff_coffs_age_period(par))[1], option="D")
            
            # Transpose m?
            p <- chorddiag(m, groupColors=groupColors, groupnamePadding=40, ...)
            p
          })


setMethod("chordMovePlot", signature(obj="MFCLTag"), 
          function(obj, frq=NULL, program=NULL, ...){
            
            tag <- obj
            if(is.null(frq))
              stop("A 'frq' file must also be provided")
            
            fsh.reg.map <- data.frame(recap.fishery=1:n_fisheries(frq), recap.region=c(aperm(region_fish(frq), c(3,1,2,4,5,6))))
            
            data <- merge(recaptures(tag), fsh.reg.map)
            
            if(!is.null(program)) 
              data <- merge(recaptures(tag)[recaptures(tag)$program==program,], fsh.reg.map)
            
            # Which way round is this?
            
            m <- tapply(data$recap.number, list(data$region, data$recap.region), sum)
            m[is.na(m)] <- 0
            
            regions <- paste("Region", 1:dim(m)[1])
            
            # This is correct
            dimnames(m) <- list(from=regions, to=regions)
            #groupColors <- c("#000000", "#FFDD89", "#957244", "#F26223","#000000", "#FFDD89", "#957244", "#F26223")
            #rnd <- sample(1:dim(diff_coffs_age_period(par))[1], dim(diff_coffs_age_period(par))[1], replace=F)
            groupColors <- colorRampPalette(c("goldenrod","brown","beige"))(dim(m)[1])
            #groupColors <- viridis(dim(diff_coffs_age_period(par))[1], option="D")
            
            p <- chorddiag(m, groupColors=groupColors, groupnamePadding=40, ...)
            p
          })



#frq  <- read.MFCLFrq("/media/penguin/skj/2019/assessment/Diagnostic/skj.frq")
#tag  <- read.MFCLTag("/media/penguin/skj/2019/assessment/Diagnostic/skj.tag")
#par <- read.MFCLRegion("/media/penguin/skj/2019/assessment/Diagnostic/07.par")

#------------------------------------------------------------------------------------


#' sankeyMovePlot
#'
#' Produces a Sankey plot of movement 
#'
#' @param obj:    An object either of class MFCLRegion or of class MFCLTag
#' @param frq:    An object of MFCLFrq. Defaults to NULL but required if obj=MFCLTag
#'
#'
#' @return A plot of movement between assessment regions as determined either by the tag data or the estimated movement from the assessment.
#' 
#' 
#' @export
#' @docType methods
#' @rdname par-methods
#'

# Sankey plots

setGeneric('sankeyMovePlot', function(obj, ...) standardGeneric('sankeyMovePlot')) 


setMethod("sankeyMovePlot", signature(obj="MFCLRegion"), 
          function(obj, ...){
            
            par <- obj
            
            args <- list(...)
            
            data_long <- data.frame(source=as.numeric(dimnames(diff_coffs_age_period(par))$from),
                                    target=rep(as.numeric(dimnames(diff_coffs_age_period(par))$to), 
                                               each=length(dimnames(diff_coffs_age_period(par))$from)),
                                    value =c(diff_coffs_age_period(par)[,,15,1]))
            
            data_long$target <- paste(data_long$target, " ", sep="")
            
            nodes <- data.frame(name=c(as.character(data_long$source), as.character(data_long$target)) %>% unique())
            
            data_long$IDsource=match(data_long$source, nodes$name)-1 
            data_long$IDtarget=match(data_long$target, nodes$name)-1
            
            # prepare colour scale
            cols <- paste(colorRampPalette(c("darkslategrey","lightblue"))(dim(diff_coffs_age_period(par))[1]), collapse=",")
            ColourScal <- paste("d3.scaleOrdinal() .range(['", paste(unlist(strsplit(cols, split=",")), collapse="','"), "'])", sep="")
            
            # Make the Network
            sankeyNetwork(Links = data_long, Nodes = nodes,
                          Source = "IDsource", Target = "IDtarget",
                          Value = "value", NodeID = "name", 
                          sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20)
            
            
            
          })


setMethod("sankeyMovePlot", signature(obj="MFCLTag"), 
          function(obj, frq=NULL, program=NULL, ...){
            
            tag <- obj
            if(is.null(frq))
              stop("A 'frq' file must also be provided")
            
            fsh.reg.map <- data.frame(recap.fishery=1:n_fisheries(frq), recap.region=c(aperm(region_fish(frq), c(3,1,2,4,5,6))))
            
            data <- merge(recaptures(tag), fsh.reg.map)
            
            if(!is.null(program)) 
              data <- merge(recaptures(tag)[recaptures(tag)$program==program,], fsh.reg.map)
            
            
            data <- tapply(data$recap.number, list(data$region, data$recap.region), sum)
            data[is.na(data)] <- 0
            
            data_long <- data.frame(source=rownames(data), target=rep(colnames(data), each=length(rownames(data))), value=c(data))
            
            
            data_long$target <- paste(data_long$target, " ", sep="")
            
            nodes <- data.frame(name=c(as.character(data_long$source), as.character(data_long$target)) %>% unique())
            
            data_long$IDsource=match(data_long$source, nodes$name)-1 
            data_long$IDtarget=match(data_long$target, nodes$name)-1
            
            # prepare colour scale
            cols <- paste(colorRampPalette(c("darkslategrey","lightblue"))(n_regions(frq)), collapse=",")
            ColourScal <- paste("d3.scaleOrdinal() .range(['", paste(unlist(strsplit(cols, split=",")), collapse="','"), "'])", sep="")
            
            # Make the Network
            sankeyNetwork(Links = data_long, Nodes = nodes,
                          Source = "IDsource", Target = "IDtarget",
                          Value = "value", NodeID = "name", 
                          sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20)
            
          })


