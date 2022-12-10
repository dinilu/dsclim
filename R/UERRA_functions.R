
#' Load UERRA data into climate4R grid format
#'
#' @param file A string with the file name with the UERRA data to be loaded.
#' @param var A string with the variable name from the file to be loaded.
#' @param lon_lim An optional vector with two values specifying the longitudinal limits (min and max) to be loaded.
#' @param lat_lim An optional vector with two values specifying the latitudinal limits (min and max) to be loaded.
#' @param dictionary An optional string with the filename with the dictionary for the UERRA data. By default it loads an internal dictionary from the package.
#' @param aggr_times An optional numeric specifying the number of cells to be aggregated.
#' @param aggr_fun An optional function specifying the function to be used in aggregation if aggr.times was specified.
#'
#' @return A climate4R grid data (see [loadeR::loadGridData()] )
#'
#' @import loadeR loadeR.java
#'
#' @export
#'
#' @examples
#' \dontrun{
#' file <- "Data/UERRA/UERRA-HARMONIE/2m_temperature/latlon/1961-90_2m_temperature.nc"
#' var <- "tas"
#' uerra <- loadUerra(uerra.file, local.var)
#' }
#'
loadUerra <- function(file, var, lon_lim = c(-11, 12), lat_lim = c(28, 44), dictionary = system.file("extdata", "UERRA_dictionary.csv", package = "dsclim"), aggr_times = NULL, aggr_fun = mean) {
  data <- loadeR::loadGridData(file, var = var, lonLim = lon_lim, latLim = lat_lim, dictionary = dictionary)

  if (!is.null(aggr_times)) {
    data <- transformeR::upscaleGrid(data, times = aggr_times, aggr.fun = list(FUN = aggr_fun))
  }

  data <- modifyDates(data)

  return(data)
}
