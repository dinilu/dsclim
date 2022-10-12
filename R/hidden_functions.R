

#' Title
#'
#' @param x TBW
#' @param y TBW
#'
#' @return TBW
#'
#' @keywords internal
#'
# .elementwise.all.equal <- function(x, y){Vectorize(function(x, y) {isTRUE(all.equal(x, y))})} #Modified for documenting
.elementwise.all.equal <- Vectorize(function(x, y) {isTRUE(all.equal(x, y))}) #Working version


#' Title
#'
#' @param st TBW
#' @param en TBW
#' @param freq TBW
#' @param frac TBW
#'
#' @return TBW
#'
#' @keywords internal
#'
.DateSeq <- function(st, en, freq, frac = 0) {
  st <- zoo::as.Date(zoo::as.yearmon(st))
  en <- zoo::as.Date(zoo::as.yearmon(en))
  result <- zoo::as.Date(zoo::as.yearmon(seq(st, en, by = paste(as.character(12/freq), "months"))), frac = frac)
  return(result)
}



#' Copy coordinates from one grid to another
#'
#' @param x A grid object (see loadeR package) to be modified.
#' @param y A grid object from which to extract the coordinates info.
#'
#' @return The x grid object with the coordinates info from the y grid object.
#'
#' @keywords internal
#'
.copyXYCoords <- function(x, y){
  if(!exists("xyCoords", where = y) && !exists("xyCoords", where = attributes(y))){
    stop("y object do not have xy coordinates. Please provide a `y` object with spatial info. See loadTrace, loadCMIP5, or loadUerra for more info.")
  }
  if(exists("xyCoords", where = y)){
    x$xyCoords <- y$xyCoords
  }
  if(exists("xyCoords", where = attributes(y))){
    x$xyCoords <- attributes(y)$xyCoords
  }
  return(x)
}


#' Title
#'
#' @param grid TBW
#'
#' @return TBW
#'
#' @keywords internal
#'
.recalcGridResolution <- function(grid){
  attr(grid$xyCoords, "resX") <- (max(grid$xyCoords$x) - min(grid$xyCoords$x)) / (length(grid$xyCoords$x) - 1)
  attr(grid$xyCoords, "resY") <- (max(grid$xyCoords$y) - min(grid$xyCoords$y)) / (length(grid$xyCoords$y) - 1)
  return(grid)
}


