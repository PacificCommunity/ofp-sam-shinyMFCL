## Copy sensitivity runs from many places within the BET 2020 assessment tree
## into one place: z:/bet/2023/model_runs/sensitivities/2020_sensitivities

## Total size ~10 GB, after removing big Hessian files manually
stop("this script copies historical runs, not necessary to run again")

library(FLR4MFCL)  # finalPar
library(TAF)       # cp

# Find dirs
proj.dir = "//penguin/assessments/bet/2020/Model_Runs/"
one.off.stem = paste0(proj.dir,"998_999_2020_one_offs_doitall/")
grid.stem = paste0(proj.dir,"998_AAA_2020_grid.from_doitall/")
dirs = c(paste0(proj.dir,"2020_stepwise/23_fixSelBump/10N/"),
         paste0(one.off.stem,c("Comp_Data/len_LL_reg_456/","Comp_Data/USAU_wtcomp_idx25/")),
         paste0(proj.dir,"999_2020_one_offs_doitall/Growth/est_richards_oto_only/"),
         paste0(one.off.stem,"Growth/est_richards_tag_int/"),
         paste0(grid.stem,"16_OtoOnly_Diagnostic_20_0.80/"),
         paste0(one.off.stem,c("M/hi_0.146/","M/low_0.109/","M/mid_0.127/")),
         paste0(grid.stem,"23_TagInt_Diagnostic_200_0.80/"),
         paste0(one.off.stem,c("Robust_Normal/down_500/")),
         paste0(grid.stem,"19_TagInt_Diagnostic_60_0.80/"),
         paste0(one.off.stem,c("Robust_Normal/up_10/","Selectivity/ALT/","Selectivity/freeQ2sel/","Start_year/CPUE_1962/","Start_year/Model_1962/")),
         paste0(grid.stem,c("3_TagInt_Diagnostic_20_0.65/","27_TagInt_Diagnostic_20_0.95/")),
         paste0(one.off.stem,c("Tags/mix1/","Tags/remove_tags/")),
         paste0(proj.dir,"998_999_SSMULT/"))
model.names =  c("Diagnostic","len456","idxAU-US","EstRichardsO","EstRichardsT","Oto-Only","M-hi","M-low","M-mid","Size200","Size500","Size60","Size10","Alt","FreeSel","CPUE1962","Model1962","h0.65","h0.95","Mix1","TagFree","SSMULT")
basename(sapply(dirs, finalPar, quiet=TRUE))

# Copy dirs
dest <- "//penguin/assessments/bet/2023/model_runs/sensitivities/2020_sensitivities"
cp(dirs[1], dest)  # diagnostic case (2.5 GB), remove big Hessian files manually
sapply(dirs[-1], cp, dest)  # other dirs are lighter

# Rename dirs to model.names
from <- file.path(dest, basename(dirs))
to <- file.path(dest, model.names)
data.frame(from=basename(from), to=basename(to))
file.rename(from, to)

# Copy dummy bet.tag from Diagnostic into TagFree, to make Shiny work
cp(file.path(dirs[model.names=="Diagnostic"], "bet.tag"),
   file.path(dirs[model.names=="TagFree"]))
