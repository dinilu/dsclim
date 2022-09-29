## ---- include = FALSE---------------------------------------------------------
options(java.parameters = "-Xmx16g")
knitr::opts_chunk$set(
  eval = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ----set_trace_limits---------------------------------------------------------
#  trace.lon <- c(-25, 25)
#  trace.lat <- c(25, 50)

## ----load_tsmx_trace----------------------------------------------------------
#  trace.tsmx <- dsclim::loadHistoricalTraceGrid("Data/TraCE21ka/TSMX/trace.36.400BP-1990CE.cam2.h0.TSMX.2160101-2204012.nc", var="tasmax", lonLim=trace.lon, latLim = trace.lat)
#  
#  visualizeR::spatialPlot(transformeR::climatology(trace.tsmx), backdrop.theme = "countries", rev.colors=TRUE)

## ----load_tsmn_trace----------------------------------------------------------
#  trace.tsmn <- dsclim::loadHistoricalTraceGrid("Data/TraCE21ka/TSMN/trace.36.400BP-1990CE.cam2.h0.TSMN.2160101-2204012.nc", var="tasmin", lonLim=trace.lon, latLim = trace.lat)
#  
#  visualizeR::spatialPlot(transformeR::climatology(trace.tsmn), backdrop.theme = "countries", rev.colors=TRUE)

## ----load_ts_trace------------------------------------------------------------
#  trace.ts <- dsclim::loadHistoricalTraceGrid("Data/TraCE21ka/TS/trace.36.400BP-1990CE.cam2.h0.TS.2160101-2204012.nc", var="tas", lonLim=trace.lon, latLim = trace.lat)
#  
#  visualizeR::spatialPlot(transformeR::climatology(trace.ts), backdrop.theme = "countries", rev.colors=TRUE)

## ----load_precc_trace---------------------------------------------------------
#  trace.precc <- dsclim::loadHistoricalTraceGrid("Data/TraCE21ka/PRECC/trace.36.400BP-1990CE.cam2.h0.PRECC.2160101-2204012.nc", var="pr", lonLim=trace.lon, latLim = trace.lat)
#  
#  visualizeR::spatialPlot(transformeR::climatology(trace.precc), backdrop.theme = "countries")

## ----load_relhum_trace--------------------------------------------------------
#  trace.relhum <- dsclim::loadHistoricalTraceGrid("Data/TraCE21ka/RELHUM/trace.36.400BP-1990CE.cam2.h0.RELHUM.2160101-2204012.nc", var="hurs@992.5561", lonLim=trace.lon, latLim = trace.lat)
#  
#  visualizeR::spatialPlot(transformeR::climatology(trace.relhum), backdrop.theme = "countries")

## ----load_cldtot_trace--------------------------------------------------------
#  trace.cldtot <- dsclim::loadHistoricalTraceGrid("Data/TraCE21ka/CLDTOT/trace.36.400BP-1990CE.cam2.h0.CLDTOT.2160101-2204012.nc", var="cld", lonLim=trace.lon, latLim = trace.lat)
#  
#  visualizeR::spatialPlot(transformeR::climatology(trace.cldtot), backdrop.theme = "countries")

## ----load_ps_trace------------------------------------------------------------
#  trace.ps <- dsclim::loadHistoricalTraceGrid("Data/TraCE21ka/PS/trace.36.400BP-1990CE.cam2.h0.PS.2160101-2204012.nc", var="ps", lonLim=trace.lon, latLim = trace.lat)
#  
#  visualizeR::spatialPlot(transformeR::climatology(trace.ps), backdrop.theme = "countries")

## ----load_u_trace-------------------------------------------------------------
#  trace.u <- dsclim::loadHistoricalTraceGrid("Data/TraCE21ka/U/trace.36.400BP-1990CE.cam2.h0.U.2160101-2204012.nc", var="u@992.5561", lonLim=trace.lon, latLim = trace.lat)
#  
#  visualizeR::spatialPlot(transformeR::climatology(trace.u), backdrop.theme = "countries")

## ----load_v_trace-------------------------------------------------------------
#  trace.v <- dsclim::loadHistoricalTraceGrid("Data/TraCE21ka/V/trace.36.400BP-1990CE.cam2.h0.V.2160101-2204012.nc", var="v@992.5561", lonLim=trace.lon, latLim = trace.lat)
#  
#  visualizeR::spatialPlot(transformeR::climatology(trace.v), backdrop.theme = "countries")

## ----compute_wss_trace--------------------------------------------------------
#  trace.wss <- dsclim::compute_wind_speed(trace.u, trace.v)
#  
#  visualizeR::spatialPlot(transformeR::climatology(trace.wss), backdrop.theme = "countries")

## ----combine_trace_data-------------------------------------------------------
#  hist.trace <- transformeR::makeMultiGrid(trace.tsmx, trace.tsmn, trace.ts, trace.precc, trace.relhum, trace.cldtot, trace.ps, trace.wss)
#  
#  rm(trace.tsmx, trace.tsmn, trace.ts, trace.precc, trace.relhum, trace.cldtot, trace.ps, trace.wss, trace.u, trace.v)
#  
#  visualizeR::spatialPlot(transformeR::climatology(hist.trace), backdrop.theme = "countries")
#  

## ----set_uerra_limits---------------------------------------------------------
#  uerra.lon <- c(-11, 12)
#  uerra.lat <- c(28, 44)
#  # uerra.lon <- c(-11, 4)
#  # uerra.lat <- c(36, 44)

## ----load_tasmin_uerra--------------------------------------------------------
#  uerra.tasmin <- loadeR::loadGridData("Data/UERRA/UERRA-HARMONIE/2m_temperature/latlon/1961-90_2m_tmin.nc", var = "tasmin", lonLim = uerra.lon, latLim = uerra.lat, dictionary = "Data/UERRA/UERRA_dictionary.dic")
#  
#  visualizeR::spatialPlot(transformeR::climatology(uerra.tasmin), main="tasmin", backdrop.theme="countries", rev.colors=TRUE, at=seq(-15,40,1))

## ----load_tas_uerra-----------------------------------------------------------
#  uerra.tas <- loadeR::loadGridData("Data/UERRA/UERRA-HARMONIE/2m_temperature/latlon/1961-90_2m_temperature.nc", var = "tas", lonLim = uerra.lon, latLim = uerra.lat, dictionary="Data/UERRA/UERRA_dictionary.dic")
#  
#  visualizeR::spatialPlot(transformeR::climatology(uerra.tas), main="tas", backdrop.theme="countries", rev.colors=TRUE, at=seq(-15,40,1))

## ----load_tasmax_uerra--------------------------------------------------------
#  uerra.tasmax <- loadeR::loadGridData("Data/UERRA/UERRA-HARMONIE/2m_temperature/latlon/1961-90_2m_tmax.nc", var = "tasmax", lonLim = uerra.lon, latLim = uerra.lat, dictionary="Data/UERRA/UERRA_dictionary.dic")
#  
#  visualizeR::spatialPlot(transformeR::climatology(uerra.tasmax), main="tasmax", backdrop.theme="countries", rev.colors=TRUE, at=seq(-15,40,1))

## ----load_pr_uerra------------------------------------------------------------
#  uerra.pr <- loadeR::loadGridData("Data/UERRA/MESCAN-SURFEX/total_precipitation/latlon/1961-90_total_precipitation.nc", var = "pr", lonLim = uerra.lon, latLim = uerra.lat, dictionary = "Data/UERRA/UERRA_dictionary.dic")
#  
#  uerra.pr <- transformeR::upscaleGrid(uerra.pr, times = 2, aggr.fun = list(FUN = mean))
#  
#  uerra.pr <- transformeR::interpGrid(uerra.pr, new.coordinates = transformeR::getGrid(uerra.tas), method="bilinear")
#  
#  visualizeR::spatialPlot(transformeR::climatology(uerra.pr), main = "pr", backdrop.theme = "countries", at = seq(0,400,25))

## ----load_hurs_uerra----------------------------------------------------------
#  uerra.hurs <- loadeR::loadGridData("Data/UERRA/UERRA-HARMONIE/2m_relative_humidity/latlon/1961-90_2m_relative_humidity.nc", var = "hurs", lonLim = uerra.lon, latLim = uerra.lat, dictionary = "Data/UERRA/UERRA_dictionary.dic")
#  
#  visualizeR::spatialPlot(transformeR::climatology(uerra.hurs), main="hurs", backdrop.theme="countries", rev.colors=TRUE)

## ----load_ps_uerra------------------------------------------------------------
#  uerra.ps <- loadeR::loadGridData("Data/UERRA/UERRA-HARMONIE/surface_pressure/latlon/1961-90_surface_pressure.nc", var = "ps", lonLim = uerra.lon, latLim = uerra.lat, dictionary = "Data/UERRA/UERRA_dictionary.dic")
#  
#  visualizeR::spatialPlot(transformeR::climatology(uerra.ps), main="ps", backdrop.theme="countries", rev.colors=TRUE)

## ----load_cld_uerra-----------------------------------------------------------
#  uerra.cld <- loadeR::loadGridData("Data/UERRA/UERRA-HARMONIE/total_cloud_cover/latlon/1961-90_total_cloud_cover.nc", var = "cld", lonLim = uerra.lon, latLim = uerra.lat, dictionary = "Data/UERRA/UERRA_dictionary.dic")
#  
#  visualizeR::spatialPlot(transformeR::climatology(uerra.cld), main="cld", backdrop.theme="countries", rev.colors=TRUE)

## ----load_wss_uerra-----------------------------------------------------------
#  uerra.wss <- loadeR::loadGridData("Data/UERRA/UERRA-HARMONIE/10m_wind_speed/latlon/1961-90_10m_wind_speed.nc", var = "wss", lonLim = uerra.lon, latLim = uerra.lat, dictionary = "Data/UERRA/UERRA_dictionary.dic")
#  
#  visualizeR::spatialPlot(transformeR::climatology(uerra.wss), main="wss", backdrop.theme="countries", rev.colors=TRUE)

## ----correct_uerra_dates------------------------------------------------------
#  uerra.tasmax <- modifyDates(uerra.tasmax)
#  uerra.tasmin <- modifyDates(uerra.tasmin)
#  uerra.tas <- modifyDates(uerra.tas)
#  uerra.pr <- modifyDates(uerra.pr)
#  uerra.hurs <- modifyDates(uerra.hurs)
#  uerra.cld <- modifyDates(uerra.cld)
#  uerra.wss <- modifyDates(uerra.wss)

## ----set_cmip5_limits---------------------------------------------------------
#  cmip5.lon <- c(-11.25, 12.50)
#  cmip5.lat <- c(27, 45)

## ----load_tasmin_cmip---------------------------------------------------------
#  cmip.tasmin <- loadeR::loadGridData("Data/CMIP5/rcp2.6/CESM1-CAM5_RCP_2_6/tasmin_Amon_CESM1-CAM5_rcp26_r1i1p1_200601-210012.nc", var = "tasmin", lonLim = cmip5.lon, latLim = cmip5.lat, dictionary = "Data/CMIP5/CMIP5_dictionary.dic")
#  
#  visualizeR::spatialPlot(transformeR::climatology(cmip.tasmin), main="tasmin", backdrop.theme="countries", rev.colors=TRUE, at=seq(-15,40,1))

## ----load_tas_cmip------------------------------------------------------------
#  cmip.tas <- loadeR::loadGridData("Data/CMIP5/rcp2.6/CESM1-CAM5_RCP_2_6/tasmax_Amon_CESM1-CAM5_rcp26_r1i1p1_200601-210012.nc", var = "tas", lonLim = cmip5.lon, latLim = cmip5.lat, dictionary="Data/CMIP5/CMIP5_dictionary.dic")
#  
#  visualizeR::spatialPlot(transformeR::climatology(cmip.tas), main="tas", backdrop.theme="countries", rev.colors=TRUE, at=seq(-15,40,1))

## ----load_tasmax_cmip---------------------------------------------------------
#  cmip.tasmax <- loadeR::loadGridData("Data/CMIP5/rcp2.6/CESM1-CAM5_RCP_2_6/tasmax_Amon_CESM1-CAM5_rcp26_r1i1p1_200601-210012.nc", var = "tasmax", lonLim = cmip5.lon, latLim = cmip5.lat, dictionary="Data/CMIP5/CMIP5_dictionary.dic")
#  
#  visualizeR::spatialPlot(transformeR::climatology(cmip.tasmax), main="tasmax", backdrop.theme="countries", rev.colors=TRUE, at=seq(-15,40,1))

## ----load_pr_cmip-------------------------------------------------------------
#  cmip.pr <- loadeR::loadGridData("Data/CMIP5/rcp2.6/CESM1-CAM5_RCP_2_6/pr_Amon_CESM1-CAM5_rcp26_r1i1p1_200601-210012.nc", var = "pr", lonLim = cmip5.lon, latLim = cmip5.lat, dictionary = "Data/CMIP5/CMIP5_dictionary.dic")
#  
#  cmip.pr <- transformeR::upscaleGrid(cmip.pr, times = 2, aggr.fun = list(FUN = mean))
#  
#  cmip.pr <- transformeR::interpGrid(cmip.pr, new.coordinates = transformeR::getGrid(cmip.tas), method="bilinear")
#  
#  visualizeR::spatialPlot(transformeR::climatology(cmip.pr), main = "pr", backdrop.theme = "countries", at = seq(0,400,25))

## ----load_hurs_cmip-----------------------------------------------------------
#  cmip.hurs <- loadeR::loadGridData("Data/CMIP5/rcp2.6/CESM1-CAM5_RCP_2_6/hurs_Amon_CESM1-CAM5_rcp26_r1i1p1_200601-210012.nc", var = "hurs", lonLim = cmip5.lon, latLim = cmip5.lat, dictionary = "Data/CMIP5/CMIP5_dictionary.dic")
#  
#  visualizeR::spatialPlot(transformeR::climatology(cmip.hurs), main="hurs", backdrop.theme="countries", rev.colors=TRUE)

## ----load_ps_cmip-------------------------------------------------------------
#  cmip.ps <- loadeR::loadGridData("Data/CMIP5/rcp2.6/CESM1-CAM5_RCP_2_6/ps_Amon_CESM1-CAM5_rcp26_r1i1p1_200601-210012.nc", var = "ps", lonLim = cmip5.lon, latLim = cmip5.lat, dictionary = "Data/CMIP5/CMIP5_dictionary.dic")
#  
#  visualizeR::spatialPlot(transformeR::climatology(cmip.ps), main="ps", backdrop.theme="countries", rev.colors=TRUE)

## ----load_cld_cmip------------------------------------------------------------
#  cmip.cld <- loadeR::loadGridData("Data/CMIP5/rcp2.6/CESM1-CAM5_RCP_2_6/clt_Amon_CESM1-CAM5_rcp26_r1i1p1_200601-210012.nc", var = "cld", lonLim = cmip5.lon, latLim = cmip5.lat, dictionary = "Data/CMIP5/CMIP5_dictionary.dic")
#  
#  visualizeR::spatialPlot(transformeR::climatology(cmip.cld), main="cld", backdrop.theme="countries", rev.colors=TRUE)

## ----load_wss_cmip------------------------------------------------------------
#  cmip.wss <- loadeR::loadGridData("Data/CMIP5/rcp2.6/CESM1-CAM5_RCP_2_6/sfcWind_Amon_CESM1-CAM5_rcp26_r1i1p1_200601-210012.nc", var = "wss", lonLim = cmip5.lon, latLim = cmip5.lat, dictionary = "Data/CMIP5/CMIP5_dictionary.dic")
#  
#  visualizeR::spatialPlot(transformeR::climatology(cmip.wss), main="wss", backdrop.theme="countries", rev.colors=TRUE)

## ----correct_cmip_dates-------------------------------------------------------
#  cmip.tasmax <- modifyDates(cmip.tasmax)
#  cmip.tasmin <- modifyDates(cmip.tasmin)
#  cmip.tas <- modifyDates(cmip.tas)
#  cmip.pr <- modifyDates(cmip.pr)
#  cmip.hurs <- modifyDates(cmip.hurs)
#  cmip.cld <- modifyDates(cmip.cld)
#  cmip.wss <- modifyDates(cmip.wss)

