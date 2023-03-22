# Get data for the predicted - observed catch plot
# Adapted from code originally in the plotting function plot.pred.obs.catch.R in diags4MFCL

# Requires data.table

get_pred_obs_catch <- function(rep_list, fishery_map, scale_diff=TRUE){
  
  # Pull out catch_obs and catch_pred from rep file
  catch_obs_list <- lapply(rep_list, function(x) as.data.frame(catch_obs(x)))
  catch_obs <- data.table::rbindlist(catch_obs_list, idcol="model")
  data.table::setnames(catch_obs, "data", "catch_obs")
  catch_pred_list <- lapply(rep_list, function(x) as.data.frame(catch_pred(x)))
  catch_pred <- data.table::rbindlist(catch_pred_list, idcol="model")
  data.table::setnames(catch_pred, "data", "catch_pred")
  pdat <- merge(catch_obs, catch_pred)
  pdat[,season := as.numeric(as.character(season))]
  pdat[,ts := .(year + (season-1)/4 + 1/8)] # Hard wired hack
  pdat[,diff := .(catch_obs - catch_pred)]
  
  # Scale by total catch by fishery and Model
  if(scale_diff == TRUE){
    pdat <- pdat[,.(ts=ts, diff = diff / mean(catch_obs, na.rm=TRUE)), by=.(model, unit)]
  }
  
  # Add in fishery names
  data.table::setnames(pdat, "unit", "fishery")
  pdat[,fishery:= as.numeric(as.character(fishery))]
  pdat <- merge(pdat, fishery_map)
  
  # Want pdat to have Model names in the original order - important for plotting order
  pdat[,model:=factor(model, levels=names(rep_list))]
  
  return(pdat)
}
