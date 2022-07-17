# Define parameters and argument objects
# cmip5.lon <- c(-11.25, 12.50)
# cmip5.lat <- c(27, 45)
# 
# cmip5.spatial.pars <- list(which.combine = 'cmip5.new.vars',
#                            v.exp = .7,
#                            rot = FALSE)
# 
# local.pars.M21 <- list(n = 1, vars = local.var)
# 
# local.pars.M24 <- list(n = 4, vars = local.var)
# 
# local.pars.M31 <- list(n = 1, vars = 'trace.model.var.names')
# 
# local.pars.M34 <- list(n = 4, vars = 'trace.model.var.names')
# 
# global.nc.attributes <- list("author" = "Diego Nieto Lugilde & Daniel Romera Romera", "institution" = "Universidad de Cordoba", "email" = "bv2nilud@uco.es")
# 
# 
# 
# 
# family.link <- "gaussian"
# 
# spatial.pars <- list(which.combine = c("tas", "tasmax", "tasmin", "hurs@992.5561", "ps", "pr", "cld", "wss"),
                     # v.exp = .7,
                     # rot = FALSE)
# hist.trace = loadHistoricalTraceGrids(traceFileNames("../Data/TraCE21ka/"))

# uerra = loadUerra("../Data/UERRA/UERRA-HARMONIE/2m_temperature/latlon/1961-90_2m_temperature.nc", "tas")

# require(downscaleR)
# data = prepareData(hist.trace, uerra, spatial.predictors = spatial.pars)
# 
# model = downscaleTrain(data, method = "GLM", family = family.link, predict = TRUE)
# 
# global.nc.attributes = list("author" = "Diego Nieto Lugilde & Daniel Romera Romera", "institution" = "Universidad de Cordoba", "email" = "bv2nilud@uco.es")
# 
# downscaleTrace(1, "../Output/Trace21ka/", "../Data/TraCE21ka/", hist.trace = hist.trace, mod_data = data, model = model, global.nc.attributes = global.nc.attributes)
# 
# lapply(1:36, FUN=downscaleTrace, outdir="../Output/TraCE21ka/", trace_dir="../Data/TraCE21ka/", hist_trace=hist.trace, mod_data=data, model=model, global_nc_attributes=global.nc.attributes)
