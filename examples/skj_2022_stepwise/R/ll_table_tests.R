# Likelihood tables
library(FLR4MFCL)
library(data.table)
library(ggplot2)

source("fishery_map.R")

basedir <- "C:/Work/MFCL/ofp-sam-skj22/stepwise2022/"
model <- "M9X"

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

  
ll_summary <- summary(ll)  
lldf <- matrix(ll_summary$likelihood, nrow=1, dimnames=list(NULL,ll_summary$component))
lldf <- as.data.frame(lldf)
# Add max gradient
lldf$max_grad <-  max_grad(par)
# Number of parameters
lldf$npars <-  n_pars(par)

# Or build data.table with correct names here
lldf <- data.table(
  "Model" = model,
  "BH Steepness" = subset(ll_summary, component=="bhsteep")$likelihood,
  "Effort devs" = subset(ll_summary, component=="effort_dev")$likelihood,
  "Catchability devs" = subset(ll_summary, component=="catchability_dev")$likelihood,
  "Length comp." = subset(ll_summary, component=="length_comp")$likelihood,
  "Weight comp." = subset(ll_summary, component=="weight_comp")$likelihood,
  "Tag data" = subset(ll_summary, component=="tag_data")$likelihood,
  "Total" = subset(ll_summary, component=="total")$likelihood,
  "Max. gradient" = max_grad(par),
  "No. parameters" = n_pars(par)
)
  
  
  
  
  # Rename and tidy up
  #colnames(lldf)[colnames(lldf)=="bhsteep"] <- "BH Steepness"
  #colnames(lldf)[colnames(lldf)=="effort_dev"] <- "Effort devs"
  #colnames(lldf)[colnames(lldf)=="catchability_dev"] <- "Catchability devs"
  #colnames(lldf)[colnames(lldf)=="length_comp"] <- "Length composition"
  #colnames(lldf)[colnames(lldf)=="weight_comp"] <- "Weight composition"
  #colnames(lldf)[colnames(lldf)=="tag_data"] <- "Tag data"
  #colnames(lldf)[colnames(lldf)=="total"] <- "Total"
  #lldf <- lldf[,c("Model", "BH Steepness", "Effort devs", "Catchability devs", "Length composition", "Weight composition", "Tag data", "Total")]
  
  # Add in the grad column if available
  if(!missing(par.list)){
    max_grads <- unlist(lapply(par.list, max_grad))
    if(npars)
    {
      npars <- unlist(lapply(par.list, n_pars))
      # Cannot assume order of the par list is the same as the likelihood list so safer to merge
      max_grad_df <- data.frame(Model=names(max_grads), maxgrad = max_grads, npar = npars)
      colnames(max_grad_df)[colnames(max_grad_df)=="maxgrad"] <- "Max Gradient"
      colnames(max_grad_df)[colnames(max_grad_df)=="npar"] <- "Parameters"
    } else {
      # Cannot assume order of the par list is the same as the likelihood list so safer to merge
      max_grad_df <- data.frame(Model=names(max_grads), maxgrad = max_grads)
      colnames(max_grad_df)[colnames(max_grad_df)=="maxgrad"] <- "Max Gradient"
    }
    
    lldf <- merge(lldf, max_grad_df)
  }
  return(lldf)






ll_list <- c(ll_list, list(ll))


output$table_llhoods_DT <- renderDT({
  lltab <- likelihood.table(ll_list, par_list)
  colnames(lltab)[colnames(lltab)=="Length composition"] <- "Length comp."
  colnames(lltab)[colnames(lltab)=="Weight composition"] <- "Weight comp."
  # Format MaX Gradient into scientific notation
  lltab[,"Max Gradient"] <- sprintf("%7.2e", lltab[,"Max Gradient"]) 
  # Add more options
  lltab[,"BH Steepness"] <- sprintf("%3.2f", lltab[,"BH Steepness"]) 
  lltab[,"Effort devs"] <- sprintf("%3.2f", lltab[,"Effort devs"]) 
  lltab[,"Catchability devs"] <- sprintf("%3.2f", lltab[,"Catchability devs"]) 
  lltab[,"Length comp."] <- sprintf("%3.2f", lltab[,"Length comp."]) 
  lltab[,"Tag data"] <- sprintf("%3.2f", lltab[,"Tag data"]) 
  lltab[,"Total"] <- sprintf("%3.2f", lltab[,"Total"]) 
  # dom option drops the search and other stuff
  lltab <- datatable(lltab, options=list(pageLength=length(model_names), dom='t'), rownames=FALSE)
  lltab <- lltab %>% formatStyle('Model', target = 'row', backgroundColor = styleEqual(input$model_select,'yellow')) 
  return(lltab)
})

#' Table of likelihood components
#' 
#' Returns a data.frame of the likelihood components. The likelihoods are taken from the 'test_plot_output' file which is output as part of the model fitting process.
#' The output can be processed with the \code{xtable} package to make a table for RMarkdown or Latex reports.
#' @param likelihood.list A named list of MFCLLikelihood objects (from reading in the 'test_plot_output' files), one for each model.
#' @param par.list A named list of MFCLPar objects, the same length and names as the likelihoods argument. If this argument is not supplied, the 'grad' column is not included in the returned data.frame.
#' @param npars Boolean, if TRUE also returns the number of parameters. Default is FALSE
#' @export
#' @import FLR4MFCL
likelihood.table <- function(likelihood.list, par.list,npars=FALSE){
  # Need to add some safety checks here for the object types
  
  # Scrape the likelihoods out of the list of likelihoods
  dfs <- lapply(likelihood.list, function(x){
    lls <- summary(x)
    out <- matrix(lls$likelihood, nrow=1, dimnames=list(NULL,lls$component))
    out <- as.data.frame(out)
    return(out)
  })
  lldf <- do.call("rbind", dfs)
  lldf$Model <- names(likelihood.list)
  
  # Rename and tidy up
  colnames(lldf)[colnames(lldf)=="bhsteep"] <- "BH Steepness"
  colnames(lldf)[colnames(lldf)=="effort_dev"] <- "Effort devs"
  colnames(lldf)[colnames(lldf)=="catchability_dev"] <- "Catchability devs"
  colnames(lldf)[colnames(lldf)=="length_comp"] <- "Length composition"
  colnames(lldf)[colnames(lldf)=="weight_comp"] <- "Weight composition"
  colnames(lldf)[colnames(lldf)=="tag_data"] <- "Tag data"
  colnames(lldf)[colnames(lldf)=="total"] <- "Total"
  lldf <- lldf[,c("Model", "BH Steepness", "Effort devs", "Catchability devs", "Length composition", "Weight composition", "Tag data", "Total")]
  
  # Add in the grad column if available
  if(!missing(par.list)){
    max_grads <- unlist(lapply(par.list, max_grad))
    if(npars)
    {
      npars <- unlist(lapply(par.list, n_pars))
      # Cannot assume order of the par list is the same as the likelihood list so safer to merge
      max_grad_df <- data.frame(Model=names(max_grads), maxgrad = max_grads, npar = npars)
      colnames(max_grad_df)[colnames(max_grad_df)=="maxgrad"] <- "Max Gradient"
      colnames(max_grad_df)[colnames(max_grad_df)=="npar"] <- "Parameters"
    } else {
      # Cannot assume order of the par list is the same as the likelihood list so safer to merge
      max_grad_df <- data.frame(Model=names(max_grads), maxgrad = max_grads)
      colnames(max_grad_df)[colnames(max_grad_df)=="maxgrad"] <- "Max Gradient"
    }
    
    lldf <- merge(lldf, max_grad_df)
  }
  return(lldf)
}

#' Table of stock status metrics
#' 
#' Returns a data.frame of terminal year stock status metrics for each model in the \code{rep.list}.
#' Metrics include SBSBF0latest, MSY, BSMY and FMSY.
#' The output can be processed with the \code{xtable} package to make a table for RMarkdown or Latex reports.
#' @param rep.list A list of MFCLRep objects or a single MFCLRep object. The reference model should be listed first.
#' @param rep.names A vector of character strings naming the models for plotting purposes. If not supplied, model names will be taken from the names in the rep.list (if available) or generated automatically.
#' @export
#' @import FLR4MFCL
status.table <- function(rep.list,rep.names=NULL){
  # Check and sanitise input MFCLRep arguments and names
  rep.list <- check.rep.args(rep=rep.list, rep.names=rep.names)
  rep.names <- names(rep.list)
  # Pull out the bits of interest
  df <- lapply(rep.list, function(x){
    sbsbf0 <- SBSBF0latest(x)
    # Pull out final year
    final_sbsbf0 <- c(sbsbf0[,dim(sbsbf0)[2]])
    out <- data.frame(SBSBF0 = final_sbsbf0, MSY = MSY(x), BMSY=BMSY(x), FMSY=FMSY(x))
    return(out)
  })
  df <- do.call("rbind", df)
  df <- cbind(data.frame(Model=rep.names), df)
  return(df)
}
