
#' Load UERRA data into climate4R grid format
#'
#' @param file A string with the file name with the UERRA data to be loaded. 
#' @param var A string with the variable name from the file to be loaded.
#' @param lonLim An optional vector with two values specifying the longitudinal limits (min and max) to be loaded.
#' @param latLim An optional vector with two values specifying the latitudinal limits (min and max) to be loaded. 
#' @param dictionary An optional string with the filename with the dictionary for the UERRA data. By default it loads an internal dictionary from the package. 
#' @param aggr.times An optional numeric specifying the number of cells to be aggregated.
#' @param aggr.fun An optional function specifying the function to be used in aggregation if aggr.times was specified.
#'
#' @return TBW
#' @export
#'
#' @examples
#' # uerra.file <- "../Data/UERRA/UERRA-HARMONIE/2m_temperature/latlon/1961-90_2m_temperature.nc"
#' # local.var <- "tas"
#' # uerra = loadUerra(uerra.file, local.var)
loadUerra <- function(file, var, lonLim = NULL, latLim = NULL, dictionary = system.file("extdata", "UERRA_dictionary.csv", package = "TraCE21kaDSR"), aggr.times = NULL, aggr.fun = mean){
  
  data <- loadeR::loadGridData(file, var = var, lonLim = lonLim, latLim = latLim, dictionary = dictionary)

  if(!is.null(aggr.times)){
    data <- transformeR::upscaleGrid(data, times = aggr.times, aggr.fun=list(FUN=aggr.fun))
  }

  data <- modifyDates(data)

  return(data)
}
