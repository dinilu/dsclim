
#' Load CMIP5 data into climate4R grid format
#'
#' @param var_list TBW
#' @param var_new_list TBW
#' @param indir TBW
#' @param rcp TBW
#' @param mod TBW
#' @param lonLim TBW
#' @param latLim TBW
#' @param years TBW
#' @param dictionary TBW
#'
#' @return TBW
#' @export
#'
#' @examples #TBW
loadCMIP <- function(var_list, var_new_list, indir = "../../Data/CMIP5/", rcp, mod, lonLim = NULL, latLim = NULL, years = NULL, dictionary = system.file("extdata", "CMIP5_dictionary.csv", package = "TraCE21kaDSR")){
  # var_list <- cmip5.vars
  # var_new_list <- cmip5.new.vars
  # indir <- "../../Data/CMIP5/"
  # rcp <- "rcp2.6"
  # mod <- cmip5.mods[[1]]
  # lonLim <- cmip5.lon
  # latLim <- cmip5.lat
  # years <- 2006:2100
  # dictionary <- system.file("extdata", "CMIP5_dictionary.csv", package = "TraCE21kaDSR")
  
  if(rcp != "historical"){
    rcp_folder <- paste0("RCP_", substr(rcp, 4, 4), "_", substr(rcp, 6, 6))
    rcp_file <- paste0("rcp", substr(rcp, 4, 4), substr(rcp, 6, 6))
    dates <- "200601-210012"
  } else {
    rcp_folder <- rcp
    rcp_file <- rcp
    dates <- "185001-200512"
  } 
  
  file_list <- paste0(indir, rcp, "/", mod, "_", rcp_folder, "/", var_list, "_Amon_", mod, "_", rcp_file, "_r1i1p1_", dates, ".nc")
  
  data <- mapply(loadeR::loadGridData, dataset = file_list, var = var_new_list, MoreArgs=list(lonLim = lonLim, latLim = latLim, dictionary = dictionary), SIMPLIFY = FALSE)
  
  start_date <- paste0(years[[1]], "-01-01")
  end_date <- paste0(years[[length(years)]], "-12-31")
  
  data <- lapply(data, FUN = modifyDates, start_date, end_date)
  
  names(data) <- var_new_list
  
  data <- transformeR::makeMultiGrid(data)
  
  if(!is.null(years)){
    data <- transformeR::subsetGrid(data, years = years)
  } 
  
  return(data)
} 



#' Downscale CMIP5 grid data
#'
#' @param uerra TBW
#' @param var_list TBW
#' @param var_new_list TBW
#' @param rcp TBW
#' @param mod TBW
#' @param lonLim TBW
#' @param latLim TBW
#' @param dictionary TBW
#' @param indir TBW
#' @param outdir TBW
#' @param local.var TBW
#' @param spatial.pars TBW
#' @param method TBW
#' @param family.link TBW
#' @param global.nc.attributes TBW
#'
#' @return TBW
#' @export
#'
#' @examples #TBW
downscaleCMIP5 <- function(uerra, var_list, var_new_list, rcp, mod, lonLim = cmip5.lon, latLim = cmip5.lat, dictionary = system.file("extdata", "CMIP5_dictionary.csv", package = "TraCE21kaDSR"), indir = "../../Data/CMIP5/", outdir = "../../Output/CMIP5/", local.var, spatial.pars = cmip5.spatial.pars, method = "GLM", family.link = family.link, global.nc.attributes = global.nc.attributes){
  
  # uerra <- uerra
  # var_list <- cmip5.vars
  # var_new_list <- cmip5.new.vars
  # rcp <- "rcp6.0"
  # mod <- cmip5.mods[[3]]
  # lonLim <- cmip5.lon
  # latLim <- cmip5.lat
  # dictionary <- system.file("extdata", "CMIP5_dictionary.csv", package = "TraCE21kaDSR")
  # indir <- "../../Data/CMIP5/"
  # outdir <- "../../Output/CMIP5/"
  # local.var <- "cld"
  # spatial.pars = cmip5.spatial.pars
  # family.link <- "gaussian"
  # global.nc.attributes <- global.nc.attributes
  
  if(!dir.exists(paste0(outdir, rcp, "/", mod, "/", local.var, "/dat"))){
    dir.create(paste0(outdir, rcp, "/", mod, "/", local.var, "/dat"), recursive = TRUE)
  } 
  
  hist.cmip5 <- loadCMIP(var_list = var_list, var_new_list = var_new_list, indir = indir, rcp = "historical", mod = mod, years=1961:1990)
  
  if(local.var == "pr"){
    uerra.bin <- transformeR::binaryGrid(uerra, condition = "GE", threshold = 1)
    
    data.bin <- downscaleR::prepareData(hist.cmip5, uerra.bin, spatial.predictors = cmip5.spatial.pars)
    data <- downscaleR::prepareData(hist.cmip5, uerra, spatial.predictors = cmip5.spatial.pars)
    
    model.bin <- downscaleR::downscaleTrain(data.bin, method = "GLM", family = stats::binomial(link="logit"), predict = TRUE)
    model <- downscaleR::downscaleTrain(data, method = "GLM", family = family.link, predict = TRUE, condition = "GE", threshold = 1)
  } else {
    data <- downscaleR::prepareData(hist.cmip5, uerra, spatial.predictors = cmip5.spatial.pars)
    model <- downscaleR::downscaleTrain(data, method = method, family = family.link, predict = TRUE)
  } 
  
  rcp.cmip5.1 <- loadCMIP(var_list = var_list, var_new_list = var_new_list, indir = indir, rcp = "historical", mod = mod, years=1991:2005)
  rcp.cmip5.2 <- loadCMIP(var_list = var_list, var_new_list = var_new_list, indir = indir, rcp = rcp, mod = mod, years = 2006:2100)
  
  rcp.cmip5 <- transformeR::bindGrid(rcp.cmip5.1, rcp.cmip5.2, dimension = "time")
  
  new.data <- downscaleR::prepareNewData(rcp.cmip5, data)
  
  if( local.var == "pr"){
    pred.bin <- downscaleR::downscalePredict(new.data, model.bin)
    pred.cont <- downscaleR::downscalePredict(new.data, model)
    pred <- transformeR::gridArithmetics(pred.bin, pred.cont, operator = "*")
  } else {
    pred <- downscaleR::downscalePredict(new.data, model)
  } 
  
  pred$Data <- round(pred$Data, 2)
  
  loadeR.2nc::grid2nc(pred, NetCDFOutFile = paste0(outdir, rcp, "/", mod, "/", local.var, "/", local.var, "1991-2100.nc"), missval = -9999, globalAttributes = global.nc.attributes)
  
  for(i in 1:110){
    y <- c(1991:2100)[i] 
    yBP <- c(41:151)[i] 
    pred.i <- transformeR::subsetGrid(pred, years=y)
    
    pred.df <- nc2sp_df(pred.i)
    
    file.name <- paste0(outdir, rcp, "/", mod, "/", local.var, "/dat/", local.var, yBP, ".dat")
    utils::write.table(pred.df, file = file.name, sep = "\t")
  } 
  return("Done")
} 
