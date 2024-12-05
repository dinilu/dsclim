
#' Load CMIP5 data into climate4R grid format
#'
#' @param indir A character string with the directory path for CMIP5 data. This directory should have a folder with the name of the RCP that is going to be loaded. The RCP folder should have in turn a folder with the name of the General Circulation Model that is goint to be loaded.
#' @param rcp A character vector with the name of the RCP that is going to be loaded. Possible values are c("historical", "rcp2.6", "rcp4.5", "rcp6.0", "rcp8.5")
#' @param mod A character vector with the name of the GCM that is going to be loaded. Possible values are c("CESM1-CAM5", "CSIRO-Mk3-6-0", "IPSL-CM5A-MR").
#' @param vars A character vector with names of the CMIP5 variables in their standardized format (as they should match those from other datasets, e.g. TraCE21ka and UERRA datasets).
#' @param cmip5_vars A character vector with names of the CMIP5 variables in their original format (as coded in original CMIP5 datasets).
#' @param lon_lim A numeric vector (length = 2) with the longitudinal extent that is going to be loaded.
#' @param lat_lim A numeric vector (length = 2) with the latitudinal extent that is going to be loaded.
#' @param years A numeric vector with the sequence of years that are going to be loaded.
#' @param dictionary A data dictionary (as in \code{\link[loadeR]{loadGridData}}) that is going to be used to load the data. By default it uses an internal dictionary that standardize CMIP5, TraCE21ka, and UERRA data to the same format.
#'
#' @return A list object with the structure suitable for the climate4R suite. More specifically the data are as returned from the \code{\link[loadeR]{loadGridData}} function.
#'
#' @import loadeR
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dsclim::loadCMIP5("../../Data/CMIP5/",
#'   "rcp2.6",
#'   "CESM1-CAM5",
#'   lon_lim = c(-11, 12),
#'   lat_lim = c(28, 44),
#'   years = 2006:2100
#' )
#' }
loadCMIP5 <- function(indir, rcp, mod, vars = NULL, cmip5_vars = NULL, lon_lim = c(-11, 12), lat_lim = c(28, 44), years = 1991:2100, dictionary = system.file("extdata", "CMIP5_dictionary.csv", package = "dsclim")) {
  if (is.null(cmip5_vars)) {
    cmip5_vars <- cmip5.vars
  }

  if (is.null(vars)) {
    vars <- cmip5.new.vars
  }

  if (rcp != "historical") {
    rcp_folder <- paste0("RCP_", substr(rcp, 4, 4), "_", substr(rcp, 6, 6))
    rcp_file <- paste0("rcp", substr(rcp, 4, 4), substr(rcp, 6, 6))
    dates <- "200601-210012"
  } else {
    rcp_folder <- rcp
    rcp_file <- rcp
    dates <- "185001-200512"
  }

  file_list <- paste0(indir, rcp, "/", mod, "_", rcp_folder, "/", cmip5_vars, "_Amon_", mod, "_", rcp_file, "_r1i1p1_", dates, ".nc")

  data <- mapply(loadeR::loadGridData, dataset = file_list, var = vars, MoreArgs = list(lonLim = lon_lim, latLim = lat_lim, dictionary = dictionary), SIMPLIFY = FALSE)

  start_date <- paste0(years[[1]], "-01-01")
  end_date <- paste0(years[[length(years)]], "-12-31")

  data <- lapply(data, FUN = modifyDates, start_date, end_date)

  names(data) <- vars

  data <- transformeR::makeMultiGrid(data)

  data <- transformeR::subsetGrid(data, years = years)

  return(data)
}



#' Downscale CMIP5 grid data
#'
#' @param rcp A character string indicating the Historical or the Representative Concentration Pathway (RCP) that is going to be downscaled. Possible values are c("historical", "rcp2.6", "rcp4.5", "rcp6.0", "rcp8.5").
#' @param mod A character string indicating the General Circulation Model that is going to be downscaled. Possible values are c("CESM1-CAM5", "CSIRO-Mk3-6-0", "IPSL-CM5A-MR").
#' @param indir A character string indicating the directory path to look for the CMIP5 data. This directory should have a folder with the name of the RCP that is going to be loaded. The RCP folder should have in turn a folder with the name of the General Circulation Model that is goint to be loaded.
#' @param uerra A climate4R grid object to be used as predictand of the dowscaling process, usually the historical high resolution dataset. This object is created with the \code{\link[dsclim]{loadUerra}}) or the \code{\link[loadeR]{loadGridData}}) functions.
#' @param outdir A character string indicating the directory path to save the downscaled datasets.
#' @param vars A character vector indicating the CMIP5 variables to be used as predictors. If NULL (default value) the function use all  available variables: c("tas, "tasmax", "tasmin", "hurs", "ps", "pr", "clt", "sfcWind"), which represent mean surface temperature (tas), maximum surface temperature (tasmax), minimum surface temperature (tasmin), relative humidity (hurs), surface pressure (ps), precipitation (pr), total cloudiness (clt), and surface wind speed (sfcWind). These are the names of the CMIP5 variables in their standardized format (as they should match those from other datasets, e.g. TraCE21ka and UERRA datasets).
#' @param cmip5_vars A character vector indicating the variables to be downloaded. If NULL (default value) the function downscaled all the available variables c("tas, "tasmax", "tasmin", "hurs", "ps", "pr", "cld", "wss"), which represent mean surface temperature (tas), maximum surface temperature (tasmax), minimum surface temperature (tasmin), relative humidity (hurs), surface pressure (ps), precipitation (pr), total cloudiness (cld), and surface wind speed (wss). These are the names of the CMIP5 variables in their original format (as coded in original CMIP5 datasets).
#' @param lon_lim A numeric vector (length = 2) with the longitudinal extent that is going to be dowscaled
#' @param lat_lim A numeric vector (length = 2) with the latitudinal extent that is going to be dowscaled.
#' @param years A numeric vector with the sequence of years that are going to be downscaled. By default, it downscaled the whole future period in CMIP5 dataset (1991-2100).
#' @param dictionary A data dictionary (as in \code{\link[loadeR]{loadGridData}}) that is going to be used to load the data CMIP5 data. By default it uses an internal dictionary that standardize CMIP5, TraCE21ka, and UERRA data to the same format.
#' @param method A character string indicating the downscaling method to be used. More details can be read at \code{\link[downscaleR]{downscaleTrain}}
#' @param family_link A character string indicating the family link to be used if `method = "GLM"`.
#'
#' @inheritParams downscaleR::prepareData
#' @inheritParams loadeR.2nc::grid2nc
#'
#' @return The function return the string "Done" when completed successfully. However, the real output of the function is the output directory with the downscaled data in the same folder structure as the original dataset (Output/CMIP5/RCP/GCM/var). By default, the data are saved yearly (one file for each year) in two different formats: netCDF (nc) and raw text-tab-separated format (columns: x, y, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12; numbered columns represent the 10 months of the year).
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' uerra <- dsclim::loadUerra(
#'   "Data/UERRA/UERRA-HARMONIE/2m_temperature/latlon/1961-90_2m_temperature.nc",
#'   "tas"
#' )
#'
#' dsclim::downscaleCMIP5("rcp6.0",
#'   "CESM1-CAM5",
#'   "Data/CMIP5/",
#'   uerra,
#'   "Output/CMIP5/",
#'   lon_lim = c(-11, 12),
#'   lat_lim = c(28, 44),
#'   years = 1991:2100,
#'   method = "GLM",
#'   family_link = "gaussian"
#' )
#' }
#'
downscaleCMIP5 <- function(rcp, mod, indir, uerra, outdir, vars = NULL, cmip5_vars = NULL, lon_lim = c(-11, 12), lat_lim = c(28, 44), years = 1991:2100, dictionary = system.file("extdata", "CMIP5_dictionary.csv", package = "dsclim"), method = "GLM", family_link = stats::gaussian(link = "identity"), local.predictors = NULL, global.vars = NULL, spatial.predictors = NULL, extended.predictors = NULL, combined.only = TRUE, globalAttributes = NULL) {
  y.var <- uerra$Variable$varName

  if (!dir.exists(paste0(outdir, rcp, "/", mod, "/", y.var, "/dat"))) {
    dir.create(paste0(outdir, rcp, "/", mod, "/", y.var, "/dat"), recursive = TRUE)
  }

  if (is.null(cmip5_vars)) {
    cmip5_vars <- cmip5.vars
  }
  if (is.null(vars)) {
    vars <- cmip5.new.vars
  }

  uerra_years <- sort(unique(lubridate::year(uerra$Dates$start)))

  message("LOADING HISTORICAL CMIP5 DATA...")
  hist_cmip5 <- loadCMIP5(indir = indir, rcp = "historical", mod = mod, vars = vars, cmip5_vars = cmip5_vars, lon_lim = lon_lim, lat_lim = lat_lim, years = uerra_years, dictionary = dictionary)


  message("LOADING FUTURE CMIP5 DATA...")
  if (years[1] <= 2005 && years[length(years)] > 2005) {
    rcp_cmip5_1 <- loadCMIP5(indir = indir, rcp = "historical", mod = mod, vars = vars, cmip5_vars = cmip5_vars, lon_lim = lon_lim, lat_lim = lat_lim, years = years[1]:2005, dictionary = dictionary)
    rcp_cmip5_2 <- loadCMIP5(indir = indir, rcp = rcp, mod = mod, vars = vars, cmip5_vars = cmip5_vars, lon_lim = lon_lim, lat_lim = lat_lim, years = 2006:years[length(years)], dictionary = dictionary)

    rcp_cmip5 <- transformeR::bindGrid(rcp_cmip5_1, rcp_cmip5_2, dimension = "time")
  }
  if (years[1] <= 2005 && years[length(years)] <= 2005) {
    rcp_cmip5 <- loadCMIP5(indir = indir, rcp = "historical", mod = mod, vars = vars, cmip5_vars = cmip5_vars, lon_lim = lon_lim, lat_lim = lat_lim, years = years, dictionary = dictionary)
  }
  if (years[1] > 2005 && years[length(years)] > 2005) {
    rcp_cmip5 <- loadCMIP5(indir = indir, rcp = rcp, mod = mod, vars = vars, cmip5_vars = cmip5_vars, lon_lim = lon_lim, lat_lim = lat_lim, years = years, dictionary = dictionary)
  }

  if (y.var == "pr") {
    uerra_bin <- transformeR::binaryGrid(uerra, condition = "GE", threshold = 1)

    data_bin <- downscaleR::prepareData(hist_cmip5, uerra_bin, spatial.predictors = spatial.predictors, local.predictors = local.predictors, global.vars = global.vars, extended.predictors = extended.predictors, combined.only = combined.only)
    data <- downscaleR::prepareData(hist_cmip5, uerra, spatial.predictors = spatial.predictors, local.predictors = local.predictors, global.vars = global.vars, extended.predictors = extended.predictors, combined.only = combined.only)

    model_bin <- downscaleR::downscaleTrain(data_bin, method = method, family = stats::binomial(link = "logit"), predict = TRUE)
    model <- downscaleR::downscaleTrain(data, method = method, family = family_link, predict = TRUE, condition = "GE", threshold = 1)

    new_data <- downscaleR::prepareNewData(rcp_cmip5, data)

    message("DOWNSCALING CMIP5 DATA...")
    pred_bin <- downscaleR::downscalePredict(new_data, model_bin)
    pred_cont <- downscaleR::downscalePredict(new_data, model)
    pred <- transformeR::gridArithmetics(pred_bin, pred_cont, operator = "*")
  } else {
    data <- downscaleR::prepareData(hist_cmip5, uerra, spatial.predictors = spatial.predictors, local.predictors = local.predictors, global.vars = global.vars, extended.predictors = extended.predictors, combined.only = combined.only)
    model <- downscaleR::downscaleTrain(data, method = method, family = family_link, predict = TRUE)

    new_data <- downscaleR::prepareNewData(rcp_cmip5, data)

    message("DOWNSCALING CMIP5 DATA...")
    pred <- downscaleR::downscalePredict(new_data, model)
  }

  pred$Data <- round(pred$Data, 2)

  # loadeR.2nc::grid2nc(pred, NetCDFOutFile = paste0(outdir, rcp, "/", mod, "/", y.var, "/", y.var, "1991-2100.nc"), missval = -9999, globalAttributes = globalAttributes)

  for (i in 1:length(years)) {
    message("SAVING YEAR: ", years[i])
    y <- years[i]
    yBP <- (years - 1950)[i]
    pred_i <- transformeR::subsetGrid(pred, years = y)

    loadeR.2nc::grid2nc(pred_i, NetCDFOutFile = paste0(outdir, rcp, "/", mod, "/", y.var, "/", y.var, yBP, ".nc"), missval = -9999, globalAttributes = globalAttributes)

    # pred_df <- grid2spatialdf(pred_i)

    # file_name <- paste0(outdir, rcp, "/", mod, "/", y.var, "/dat/", y.var, yBP, ".dat")
    # utils::write.table(pred_df, file = file_name, sep = "\t")
  }
  return("Done")
}
