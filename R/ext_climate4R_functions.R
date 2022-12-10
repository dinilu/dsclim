#' Definition of vertical dimension slices
#'
#' @inherit loadeR::getVerticalLevelPars title description details return author
#'
#' @param grid A grid object as returned from loadGridData.
#' @param level Vertical level. Passed by loadGridDataset, obtained via findVerticalLevel
#'
#'
getVerticalLevelPars <- function(grid, level) {
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
        stop(
          "Variable with vertical levels: '@level' following the variable name is required\nPossible values: ",
          paste(levels, collapse = ", ")
        )
      }
    } else {
      if (any(.elementwise.all.equal(level, levels))) {
        levelInd <- gcs$getVerticalAxis()$findCoordElement(level)
      } else {
        stop("Vertical level not found\nPossible values: ",
          paste(levels, collapse = ", "),
          call. = FALSE
        )
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

utils::assignInNamespace("getVerticalLevelPars", getVerticalLevelPars, ns = "loadeR")



#' Searches variable string in the dictionary
#'
#' @inherit loadeR::dictionaryLookup
#'
#' @keywords internal
#'
dictionaryLookup <- utils::getFromNamespace("dictionaryLookup", "loadeR")



#' Modify dates in a climate4R object
#'
#' @param x A climate4R object as loaded by [loadUerra()], [loadCMIP5()], [loadTrace()], or [loadeR::loadGridData()] functions.
#' @param start_date A character string with the new starting date in the format "yyyy-mm-dd".
#' @param end_date A character string with the new ending date in the format "yyyy-mm-dd".
#'
#' @return The function returns the same climate4R object with modified dates according to the new start and end dates.
#'
#' @details Because the dsclim package is designed to work and downscale monthly meteorological data, the function automatically create a vector of monthly dates from the new start to the new end dates. This new vector of dates should have the same length as the dates in the x object. Otherwise, it will through an error.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' uerra <- dsclim::loadUerra(
#'   "Data/UERRA/UERRA-HARMONIE/2m_temperature/latlon/1961-90_2m_temperature.nc",
#'   "tas"
#' )
#'
#' modifyDates(uerra, "1901-01-01", "1930-12-31")
#' }

#'
modifyDates <- function(x, start_date = "1961-01-01", end_date = "1990-12-31") {
  x$Dates$start <- paste(.DateSeq(start_date, end_date, 12, 0), "00:00:00 GMT", sep = " ")
  x$Dates$end <- paste(.DateSeq(start_date, end_date, 12, 1), "00:00:00 GMT", sep = " ")
  return(x)
}



#' Convert climate4R grid object as spatial data frame.
#'
#' @param grid A climate4R grid object (see [loadeR::loadGridData()], [loadUerra()], [loadTrace()], or [loadCMIP5()])
#'
#' @return A dataframe with the format of a spatial data.frame: two columns with X and Y coordinates, and additional twelve columns for the value of the variable in each month of the year.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' uerra <- dsclim::loadUerra(
#'   "Data/UERRA/UERRA-HARMONIE/2m_temperature/latlon/1961-90_2m_temperature.nc",
#'   "tas"
#' )
#'
#' grid2spatialdf(uerra)
#' }
grid2spatialdf <- function(grid) {
  sp <- transformeR::grid2sp(grid)
  df <- as.data.frame(sp)
  df <- df[, c(13, 14, 1:12)]
  colnames(df) <- c("x", "y", 1:12)
  return(df)
}




#' Compute wind speed from their horizontal (u) and vertical (v) components.
#'
#' @param u A climate4R grid object with east-west wind component (see [loadeR::loadGridData]).
#' @param v A climate4R grid object with north-south wind component (see [loadeR::loadGridData]).
#'
#' @return A climate4R grid object with the total wind speed.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' trace.u <- dsclim::loadHistoricalTraceGrid(
#'   "Data/TraCE21ka/U/trace.36.400BP-1990CE.cam2.h0.U.2160101-2204012.nc",
#'   var = "u@992.5561"
#' )
#' trace.v <- dsclim::loadHistoricalTraceGrid(
#'   "Data/TraCE21ka/V/trace.36.400BP-1990CE.cam2.h0.V.2160101-2204012.nc",
#'   var = "v@992.5561"
#' )
#'
#' trace.wss <- dsclim::compute_wind_speed(trace.u, trace.v)
#' }
#'
computeWindSpeed <- function(u, v) {
  message("[", Sys.time(), "] Computing wind speed (wss) from its horizontal (u) and vertical (v) components")
  u <- transformeR::gridArithmetics(u, u, operator = "*")
  v <- transformeR::gridArithmetics(v, v, operator = "*")

  ws <- transformeR::gridArithmetics(u, v, operator = "+")

  ws$Data <- round(sqrt(ws$Data), 1)
  ws$Variable$varName <- "wss"
  ws$Variable$level <- NA
  attr(ws$Variable, "use_dictionary") <- TRUE
  attr(ws$Variable, "description") <- "Wind speed at surface level"
  attr(ws$Variable, "units") <- "m/s"
  attr(ws$Variable, "longname") <- "wind speed"

  return(ws)
}
