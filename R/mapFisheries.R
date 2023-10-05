#' Map Fisheries
#'
#' Draft fisheries description using information from a \file{labels.tmp} file.
#'
#' @param file file containing fisheries labels.
#' @param pattern regular expression to select lines from \code{file} to
#'        include.
#' @param \dots further arguments passed to \code{grep}.
#'
#' @details
#' The default \code{pattern} selects all lines from the \code{file} that start
#' with a digit.
#'
#' @return
#' Data frame containing the columns \code{fishery_name}, \code{fishery},
#' \code{region}, and \code{group}.
#'
#' @note
#' This function applies simple regular expressions (see function code) to parse
#' lines of text into a four-column data frame.
#'
#' Depending on the contents of the \file{labels.tmp} file, the output will
#' often not be the exact desired fishery map. Specifically, the \code{group}
#' column will likely need some manual edits to get good plotting labels for the
#' groups.
#'
#' In other words, this function produces a draft data frame that can both speed
#' up the creation of this data frame and also reduce the likelihood of making
#' mistakes.
#'
#' @author Arni Magnusson.
#'
#' @seealso
#' \code{\link{grep}} is the underlying function to select lines to include.
#'
#' \code{\link{shinyMFCL-package}} gives an overview of the package.
#'
#' @examples
#' \dontrun{
#' mapFisheries("labels.tmp")
#' }
#'
#' @export

mapFisheries <- function(file="labels.tmp", pattern="^[0-9]", ...)
{
  x <- readLines(file)
  x <- grep(pattern, x, ..., value=TRUE)

  fishery_name <- gsub("^[0-9\\. \t]*", "", x)
  fishery <- seq_along(x)
  region <- gsub(".* R([0-9]) .*", "\\1", fishery_name)
  region <- gsub(".*([0-9])", "\\1", region)
  region <- as.integer(region)
  group <- gsub(" .*$", "", fishery_name)

  out <- data.frame(fishery_name, fishery, region, group)
  out
}
