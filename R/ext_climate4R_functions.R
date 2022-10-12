#' Definition of vertical dimension slices
#'
#' @inherit loadeR::getVerticalLevelPars title description details return   author
#'
#' @param grid A grid object as returned from loadGridData.
#' @param level Vertical level. Passed by loadGridDataset, obtained via findVerticalLevel
#'
#'
getVerticalLevelPars <- function (grid, level) {
  gcs <- grid$getCoordinateSystem()
  if (gcs$hasVerticalAxis()) {
    levels <- loadeR::scanVarDimensions(grid)$level$Values
    if (is.null(level)) {
      if (length(levels) == 1) {
        level <- levels
        if (gcs$getVerticalAxis()$findCoordElement(level) < 0) {
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

utils::assignInNamespace("getVerticalLevelPars", getVerticalLevelPars, ns="loadeR")



# overwriteGetVerticalLevelPars <- function(){
#   utils::assignInNamespace("getVerticalLevelPars", getVerticalLevelPars, ns="loadeR")
# }



#' Searches variable string in the dictionary
#'
#' @inherit loadeR::dictionaryLookup
#'
dictionaryLookup <- utils::getFromNamespace("dictionaryLookup", "loadeR")




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
#'
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
nc2spatialdf <- function(grid){
  sp <- transformeR::grid2sp(grid)
  df <- as.data.frame(sp)
  df <- df[,c(13,14,1:12)]
  colnames(df) <- c("x", "y", 1:12)
  return(df)
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
#'
computeWindSpeed <- function(u, v) {
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



