# 
# Define parameters and argument objects
# 
# trace.lon = c(-25, 25)
# trace.lat = c(25, 50)
# 
# tmp.lon <- c(-11.125, 12.125)
# tmp.lat <- c(27.875, 44.125)
# 
# uerra.lon <- c(-11, 12)
# uerra.lat <- c(28, 44)
# 
# cmip5.lon <- c(-11.25, 12.50)
# cmip5.lat <- c(27, 45)
# 
# trace.file.names <- paste0("../../Data/Trace21ka/", trace.vars, "/trace.36.400BP-1990CE.cam2.h0.", trace.vars, ".2160101-2204012.nc")
# 
# new.trace.file.names <- mapply(FUN=function(x, y, z, w){paste0("../../Data/Trace21ka/", w, "/trace.", x, ".", y, ".cam2.h0.", w, ".", z, ".nc")}, 'trace.years.n', 'trace.years.bp', 'trace.years.nums', MoreArgs = list(w='trace.vars'), SIMPLIFY = FALSE)
# 
# spatial.pars <- list(which.combine = TraCE21kaDSR:::trace.final.var.names,
#                      v.exp = .7,
#                      rot = FALSE)
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
