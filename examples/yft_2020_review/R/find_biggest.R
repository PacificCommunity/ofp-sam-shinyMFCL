find_biggest_par <- function(folder, pattern="^[0-9][0-9]\\.par$", full=TRUE,
                             quiet=FALSE)
{
  parfile <- dir(folder, pattern=pattern, full=full)
  parfile <- max(parfile)
  if(!quiet)
    cat(basename(parfile), fill=TRUE)
  parfile
}

find_biggest_rep <- function(folder, pattern="^plot-[0-9][0-9]\\.par.rep$",
                             full=TRUE, quiet=FALSE)
{
  parfile <- dir(folder, pattern=pattern, full=full)
  parfile <- max(parfile)
  if(!quiet)
    cat(basename(parfile), fill=TRUE)
  parfile
}
