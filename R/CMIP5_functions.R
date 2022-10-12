
#' Load CMIP5 data into climate4R grid format
#'
#' @param indir A character string with the directory path for CMIP5 data. This directory should have a folder with the name of the RCP that is going to be loaded. The RCP folder should have in turn a folder with the name of the General Circulation Model that is goint to be loaded.
#' @param rcp A character vector with the name of the RCP that is going to be loaded.
#' @param mod A character vector with the name of the GCM that is going to be loaded.
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
#' dsclim::loadCMIP5("tas",
#'                  "tas",
#'                  "data/",
#'                  "rcp2.6",
#'                  "CESM1-CAM5",
#'                  c(-11.25, 12.50),
#'                  c(27, 45),
#'                  2006:2100)
#' }
loadCMIP5 <- function(indir, rcp, mod, vars = NULL, cmip5_vars = NULL, lon_lim = NULL, lat_lim = NULL, years = 1961:1990, dictionary = system.file("extdata", "CMIP5_dictionary.csv", package = "dsclim")){

  if(is.null(cmip5_vars)){cmip5_vars <- cmip5.vars}

  if(is.null(vars)){vars <- cmip5.new.vars}

  if(rcp != "historical"){
    rcp_folder <- paste0("RCP_", substr(rcp, 4, 4), "_", substr(rcp, 6, 6))
    rcp_file <- paste0("rcp", substr(rcp, 4, 4), substr(rcp, 6, 6))
    dates <- "200601-210012"
  } else {
    rcp_folder <- rcp
    rcp_file <- rcp
    dates <- "185001-200512"
  }

  file_list <- paste0(indir, rcp, "/", mod, "_", rcp_folder, "/", cmip5_vars, "_Amon_", mod, "_", rcp_file, "_r1i1p1_", dates, ".nc")

  data <- mapply(loadeR::loadGridData, dataset = file_list, var = vars, MoreArgs=list(lonLim = lon_lim, latLim = lat_lim, dictionary = dictionary), SIMPLIFY = FALSE)

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
#' @param uerra TBW
#' @param rcp TBW
#' @param mod TBW
#' @param indir TBW
#' @param outdir TBW
#' @param vars TBW
#' @param cmip5_vars TBW
#' @param lon_lim TBW
#' @param lat_lim TBW
#' @param years TBW
#' @param dictionary TBW
#' @param method TBW
#' @param family_link TBW
#' @param local_pars TBW
#' @param global_pars TBW
#' @param spatial_pars TBW
#' @param global_nc_attributes TBW
#'
#' @return TBW
#'
#' @export
#'
#' @examples #TBW
downscaleCMIP5 <- function(uerra, rcp, mod, indir, outdir, vars = NULL, cmip5_vars = NULL, lon_lim = c(-11.25, 12.50), lat_lim = c(27, 45), years = 1991:2100, dictionary = system.file("extdata", "CMIP5_dictionary.csv", package = "dsclim"), method = "GLM", family_link = stats::gaussian(link = "identity"), local_pars = NULL, global_pars = NULL, spatial_pars = NULL, global_nc_attributes = NULL){

  # uerra <- uerra
  # rcp <- "rcp6.0"
  # mod <- cmip5.mods[[3]]
  # indir <- "Data/CMIP5/"
  # outdir <- "Output/CMIP5/"
  # lon_lim <- cmip5.lon
  # lat_lim <- cmip5.lat
  # years <- 1996:2100
  # dictionary <- system.file("extdata", "CMIP5_dictionary.csv", package = "dsclim")
  # family_link <- "gaussian"
  # method <- "GLM"
  # local_pars = NULL
  # global_pars = NULL
  # spatial_pars = cmip5.spatial.pars
  # global_nc_attributes <- global.nc.attributes

  y.var <- uerra$y$Variable$varName

  if(!dir.exists(paste0(outdir, rcp, "/", mod, "/", y.var, "/dat"))){
    dir.create(paste0(outdir, rcp, "/", mod, "/", y.var, "/dat"), recursive = TRUE)
  }

  if(is.null(cmip5_vars)){cmip5_vars <- cmip5.vars}
  if(is.null(vars)){vars <- cmip5.new.vars}

  uerra_years <- sort(unique(lubridate::year(uerra$Dates$start)))

  hist_cmip5 <- loadCMIP5(indir = indir, rcp = "historical", mod = mod, vars = vars, cmip5_vars = cmip5_vars, lon_lim = lon_lim, lat_lim = lat_lim, years=uerra_years, dictionary = dictionary)

  if(y.var == "pr"){
    uerra_bin <- transformeR::binaryGrid(uerra, condition = "GE", threshold = 1)

    data_bin <- downscaleR::prepareData(hist_cmip5, uerra_bin, spatial.predictors = spatial_pars, local.predictors = local_pars, global.predictors = global_pars)
    data <- downscaleR::prepareData(hist_cmip5, uerra, spatial.predictors = spatial_pars, local.predictors = local_pars, global.predictors = global_pars)

    model_bin <- downscaleR::downscaleTrain(data_bin, method = method, family = stats::binomial(link="logit"), predict = TRUE)
    model <- downscaleR::downscaleTrain(data, method = method, family = family_link, predict = TRUE, condition = "GE", threshold = 1)
  } else {
    data <- downscaleR::prepareData(hist_cmip5, uerra, spatial.predictors = spatial_pars, local.predictors = local_pars, global.predictors = global_pars)
    model <- downscaleR::downscaleTrain(data, method = method, family = family_link, predict = TRUE)
  }

  if( years[1] <= 2005 && years[length(years)] > 2005) {
    rcp_cmip5_1 <- loadCMIP5(indir = indir, rcp = "historical", mod = mod, vars = vars, cmip5_vars = cmip5_vars, lon_lim = lon_lim, lat_lim = lat_lim, years = years[1]:2005, dictionary = dictionary)
    rcp_cmip5_2 <- loadCMIP5(indir = indir, rcp = rcp, mod = mod, vars = vars, cmip5_vars = cmip5_vars, lon_lim = lon_lim, lat_lim = lat_lim, years = 2006:years[ length(years) ], dictionary = dictionary)

    rcp_cmip5 <- transformeR::bindGrid(rcp_cmip5_1, rcp_cmip5_2, dimension = "time")
  } else {
    rcp_cmip5 <- loadCMIP5(indir = indir, rcp = rcp, mod = mod, vars = vars, cmip5_vars = cmip5_vars, lon_lim = lon_lim, lat_lim = lat_lim, years = years, dictionary = dictionary)
  }

  new_data <- downscaleR::prepareNewData(rcp_cmip5, data)

  if( y.var == "pr"){
    pred_bin <- downscaleR::downscalePredict(new_data, model_bin)
    pred_cont <- downscaleR::downscalePredict(new_data, model)
    pred <- transformeR::gridArithmetics(pred_bin, pred_cont, operator = "*")
  } else {
    pred <- downscaleR::downscalePredict(new_data, model)
  }

  pred$Data <- round(pred$Data, 2)

  loadeR.2nc::grid2nc(pred, NetCDFOutFile = paste0(outdir, rcp, "/", mod, "/", y.var, "/", y.var, "1991-2100.nc"), missval = -9999, globalAttributes = global_nc_attributes)

  for(i in 1:110){
    y <- c(1991:2100)[i]
    yBP <- c(41:151)[i]
    pred_i <- transformeR::subsetGrid(pred, years=y)

    pred_df <- nc2spatialdf(pred_i)

    file_name <- paste0(outdir, rcp, "/", mod, "/", y.var, "/dat/", y.var, yBP, ".dat")
    utils::write.table(pred_df, file = file_name, sep = "\t")
  }
  return("Done")
}
