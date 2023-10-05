#' Read length.fit File
#'
#' Read observed and predicted composite (summed over time) catches at length
#' from the \verb{length.fit} file.
#'
#' @param filename name of \verb{length.fit} file.
#' @param model_name model name (default is same as \code{filename}).
#'
#' @return
#' Data frame containing five columns:
#' \tabular{ll}{
#'   \code{fishery} \tab fishery number\cr
#'   \code{length}  \tab length bin\cr
#'   \code{obs}     \tab observed frequency\cr
#'   \code{pred}    \tab predicted frequency\cr
#'   \code{model}   \tab model name
#' }
#'
#' @author
#' Finlay Scott, based on earlier work by Yukio and Nicholas Ducharme-Barth.
#'
#' @importFrom utils read.table
#'
#' @export

read_length_fit_file <- function(filename, model_name=filename)
{
  # Exit gracefully if file doesn't exist
  if(!file.exists(filename))
  {
    cat("Warning: unable to read ", filename, "\n")
    return(data.frame())
  }

  # Read length.fit
  dat <- readLines(filename)

  # Process metadata: length, number of fisheries
  # Get the version number, as older versions have different spacing
  version <- ifelse(strsplit(dat[1], split=" +")[[1]][1]=='#',
                    strsplit(dat[1], split=" +")[[1]][3], 1)
  # Number of fisheries, from file header
  Nfsh <- scan(filename, nlines=1, skip=ifelse(version==1,2,3), quiet=TRUE) - 1
  # Number of lines in the matrix for each fishery, from file header
  Nskips <- scan(filename, nlines=1, skip=ifelse(version==1,4,5), quiet=TRUE)
  # Get parameters no. bins, first bin size, bin width
  size.pars <- scan(filename, nlines=1, skip=ifelse(version==1, 1, 2),
                    quiet=TRUE)
  # Size bins, from file header
  sizebins <- seq(from=size.pars[2], by=size.pars[3], length.out=size.pars[1])
  # Number of species in the length.fit, stop if more than 1
  fishSpPtr <- ifelse(version>=2, scan(filename, nlines=1, skip=7, quiet=TRUE),
                      NA)
  nsp <- ifelse(version>=2, length(unique(fishSpPtr)), 1)

  # Only support single species/sex model for now
  if(nsp > 1)
  {
    stop("cannot process more than 1 species/sex in file")
  }

  # Include all fisheries
  VecFsh <- 1:Nfsh

  # Identify lines of observed size frequencies for fisheries
  # These are lines under # fishery totals bit at the end of file
  LineKeep <- (VecFsh - 1) * (Nskips + 6) + 1

  # Process main data
  # All blocks in file with # fishery 1 etc, are observed and predicted
  # numbers at length at age in each time step (see manual)
  # Only want stuff at end of the file which has data summed over time
  # Remove all unwanted data above fishery totals
  dat <- dat[(grep("totals",dat)+4):length(dat)]

  # Get observed data
  # This is the only observed data we want to keep - pulls out vector for the
  # fishery then skips down to the next fishery and grabs vector
  dat.obs <- dat[LineKeep]
  # Get it in the right format and transpose
  # To a matrix length class (rows), fishery (column)
  dat.obs <- t(read.table(text=dat.obs, nrows=length(LineKeep)))
  # Same for predicted
  dat.pred <- dat[LineKeep + 1]
  dat.pred <- t(read.table(text=dat.pred, nrows=length(LineKeep)))

  out <- data.frame(fishery = rep(VecFsh, each=size.pars[1]),
                    length = rep(sizebins, Nfsh),
                    obs = c(dat.obs),
                    pred = c(dat.pred),
                    model = model_name)
  out
}
