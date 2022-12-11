#' Check equality in all elements of two vectors
#'
#' @param x Vector
#' @param y Vector
#'
#' @return Logical value indicating whether all elements in the two vectors are equal or not.
#'
#' @keywords internal
#'
# .elementwise.all.equal <- function(x, y){Vectorize(function(x, y) {isTRUE(all.equal(x, y))})} #Modified for documenting
.elementwise.all.equal <- Vectorize(function(x, y) {
  isTRUE(all.equal(x, y))
}) # Working version



#' Create dates sequence
#'
#' @param st Character string indicating a starting date for the sequencce in the format "yyyy-mm-dd".
#' @param en Character string indicating a ending date for the sequencce in the format "yyyy-mm-dd".
#' @param freq Numeric value indicating the frequency of dates within a year. For instance, if monthly dates are required, `freq` should be 12. However, if annual dates are required, `freq` should be 1.
#' @param frac Numeric value indicating the period supplied to [zoo::as.Date]. Here it is used to specified the first day of the month `frac = 0`, or the last day of the month `frac = 1`.
#'
#' @return Date object (see [base::as.Date])
#'
#' @keywords internal
#'
.DateSeq <- function(st, en, freq, frac = 0) {
  st <- zoo::as.Date(zoo::as.yearmon(st))
  en <- zoo::as.Date(zoo::as.yearmon(en))
  result <- zoo::as.Date(zoo::as.yearmon(seq(st, en, by = paste(as.character(12 / freq), "months"))), frac = frac)
  return(result)
}



#' Copy coordinates from one grid to another
#'
#' @param x A grid object (see [loadeR::loadGridData] function) to be modified.
#' @param y A grid object from which to extract the coordinates info.
#'
#' @return The `x` grid object with the coordinates info from the `y` grid object.
#'
#' @keywords internal
#'
.copyXYCoords <- function(x, y) {
  if (!exists("xyCoords", where = y) && !exists("xyCoords", where = attributes(y))) {
    stop("y object do not have xy coordinates. Please provide a `y` object with spatial info. See loadTrace, loadCMIP5, or loadUerra for more info.")
  }
  if (exists("xyCoords", where = y)) {
    x$xyCoords <- y$xyCoords
  }
  if (exists("xyCoords", where = attributes(y))) {
    x$xyCoords <- attributes(y)$xyCoords
  }
  return(x)
}



#' Recalculate grid resolution
#'
#' @param grid List object with climate4R framework (see [loadeR::loadGridData]).
#'
#' @return A modified version of the `grid` object with new `resX` and `resY` info based on the actual coordinates `xyCoords` that is already in the `grid` object.
#'
#' @details This function is designed to recalculate grid resolution when a grid object has been cropped to geographical subset region.
#'
#' @keywords internal
#'
.recalcGridResolution <- function(grid) {
  attr(grid$xyCoords, "resX") <- (max(grid$xyCoords$x) - min(grid$xyCoords$x)) / (length(grid$xyCoords$x) - 1)
  attr(grid$xyCoords, "resY") <- (max(grid$xyCoords$y) - min(grid$xyCoords$y)) / (length(grid$xyCoords$y) - 1)
  return(grid)
}
