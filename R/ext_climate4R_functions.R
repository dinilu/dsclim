#' Get vertical levels in a grid.
#'
#' @param grid Grid object (see loadeR package) from which to get the levels
#' @param level Level to be found within the levels in the grid object
#'
#' @return List object with two vectors inside:
#' * level: the level searched.
#' * zRange: the position for the level.
#'
#' @import loadeR
#'
#' @export
#'
#' @examples
#' # TBW
getVerticalLevelPars <- function (grid, level) {
  gcs <- grid$getCoordinateSystem()
  if (gcs$hasVerticalAxis()) {
    levels <- loadeR::scanVarDimensions(grid)$level$Values
    if (is.null(level)) {
      if (length(levels) == 1) {
        level <- levels
        if (gcs$getVerticalAxis()$findCoordElement(level) <
            0) {
          levelInd <- gcs$getVerticalAxis()$findCoordElement(0)
        }
      } else {
        stop("Variable with vertical levels: '@level' following the variable name is required\nPossible values: ",
             paste(levels, collapse = ", "))
      }
    } else {
      if (any(.elementwise.all.equal(level, levels))) {
        levelInd <- gcs$getVerticalAxis()$findCoordElement(level)
      } else {
        stop("Vertical level not found\nPossible values: ",
             paste(levels, collapse = ", "), call. = FALSE)
      }
    }
    zRange <- rJava::.jnew("ucar/ma2/Range", levelInd, levelInd)
  } else {
    if (!is.null(level)) {
      level <- level
    }
    zRange <- rJava::.jnull()
  }
  return(list(level = level, zRange = zRange))
}

# overwriteGetVerticalLevelPars <- function(){
#   utils::assignInNamespace("getVerticalLevelPars", getVerticalLevelPars, ns="loadeR")
# }

#' TBW
#'
#' @param dicPath TBW
#' @param var TBW
#' @param time TBW
#'
#' @return TBW
#'
#' @import utils
#'
#' @examples
#' # TBW
dictionaryLookup <- utils::getFromNamespace("dictionaryLookup", "loadeR")



#' Copy coordinates info from one grid to another
#'
#' @param x A grid object (see loadeR package) to be modified.
#' @param y A grid object from which to extract the coordinates info.
#'
#' @return The x grid object with the coordinates info from the y grid object.
#'
#' @export
#'
#' @examples #TBW
copyXYCoords <- function(x, y){
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
#' @param x TBW
#' @param start_date TBW
#' @param end_date TBW
#'
#' @return TBW
#'
#' @export
#'
#' @examples #TBW
modifyDates <- function(x, start_date="1961-01-01", end_date="1990-12-31") {
  x$Dates$start <- paste(.DateSeq(start_date, end_date, 12, 0), "00:00:00 GMT", sep = " ")
  x$Dates$end <- paste(.DateSeq(start_date, end_date, 12, 1), "00:00:00 GMT", sep = " ")
  return(x)
}



#' Title
#'
#' @param grid TBW
#'
#' @return TBW
#'
#' @export
#'
#' @examples #TBW
recalcGridResolution <- function(grid){
  attr(grid$xyCoords, "resX") <- (max(grid$xyCoords$x) - min(grid$xyCoords$x)) / (length(grid$xyCoords$x) - 1)
  attr(grid$xyCoords, "resY") <- (max(grid$xyCoords$y) - min(grid$xyCoords$y)) / (length(grid$xyCoords$y) - 1)
  return(grid)
}


#' Title
#'
#' @param grid TBW
#' @param output.dir TBW
#'
#' @return TBW
#'
#' @export
#'
#' @examples #TBW
nc2sp_df <- function(grid, output.dir){
  sp <- transformeR::grid2sp(grid)
  df <- as.data.frame(sp)
  df <- df[,c(13,14,1:12)]
  colnames(df) <- c("x", "y", 1:12)
  df
}




#' Compute wind speed from their horizontal (u) and vertical (v) components.
#'
#' @param u A grid object (see loadeR package)
#' @param v TBW
#'
#' @return TBW
#'
#' @export
#'
#' @examples #TBW
compute_wind_speed <- function(u, v) {
  message("[", Sys.time(), "] Computing wind speed (wss) from its horizontal (u) and vertical (v) components")
  u <- transformeR::gridArithmetics(u, u, operator="*")
  v <- transformeR::gridArithmetics(v, v, operator="*")

  ws <- transformeR::gridArithmetics(u, v, operator="+")

  ws$Data <- round(sqrt(ws$Data), 1)
  ws$Variable$varName <- "wss"
  ws$Variable$level <- NA
  attr(ws$Variable, "use_dictionary") <- TRUE
  attr(ws$Variable, "description") <- "Wind speed at surface level"
  attr(ws$Variable, "units") <- "m/s"
  attr(ws$Variable, "longname") <- "wind speed"

  return(ws)
}



