## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  eval = FALSE,
  collapse = TRUE,
  comment = "#>"
)

## ----explore_tsmxTraCE, eval = FALSE------------------------------------------
#  di.trace <- loadeR::dataInventory("Data/TraCE21ka/TSMX/trace.36.400BP-1990CE.cam2.h0.TSMX.2160101-2204012.nc")
#  str(di.trace)

## ----explore_tsmnTraCE, eval = FALSE------------------------------------------
#  di.trace <- loadeR::dataInventory("Data/TraCE21ka/TSMN/trace.36.400BP-1990CE.cam2.h0.TSMN.2160101-2204012.nc")
#  str(di.trace)

## ----explore_tsTraCE, eval = FALSE--------------------------------------------
#  di.trace <- loadeR::dataInventory("Data/TraCE21ka/TS/trace.36.400BP-1990CE.cam2.h0.TS.2160101-2204012.nc")
#  str(di.trace)

## ----explore_preccTraCE, eval = FALSE-----------------------------------------
#  di.trace <- loadeR::dataInventory("Data/TraCE21ka/PRECC/trace.36.400BP-1990CE.cam2.h0.PRECC.2160101-2204012.nc")
#  str(di.trace)

## ----explore_relhumTraCE, eval = FALSE----------------------------------------
#  di.trace <- loadeR::dataInventory("Data/TraCE21ka/RELHUM/surface_level/trace.36.400BP-1990CE.cam2.h0.RELHUM.2160101-2204012.nc")
#  str(di.trace)

## ----explore_cldtotTraCE, eval = FALSE----------------------------------------
#  di.trace <- loadeR::dataInventory("Data/TraCE21ka/CLDTOT/trace.36.400BP-1990CE.cam2.h0.CLDTOT.2160101-2204012.nc")
#  str(di.trace)

## ----explore_psTraCE, eval = FALSE--------------------------------------------
#  di.trace <- loadeR::dataInventory("Data/TraCE21ka/PS/trace.36.400BP-1990CE.cam2.h0.PS.2160101-2204012.nc")
#  str(di.trace)

## ----explore_uTraCE, eval = FALSE---------------------------------------------
#  di.trace <- loadeR::dataInventory("Data/TraCE21ka/U/trace.36.400BP-1990CE.cam2.h0.U.2160101-2204012.nc")
#  str(di.trace)

## ----explore_vTraCE, eval = FALSE---------------------------------------------
#  di.trace <- loadeR::dataInventory("Data/TraCE21ka/V/trace.36.400BP-1990CE.cam2.h0.V.2160101-2204012.nc")
#  str(di.trace)

## ----create_trace_dictionary, eval = FALSE------------------------------------
#  file.create("Data/TraCE21ka/TraCE21ka_dictionary.dic")
#  writeLines(c("identifier,short_name,time_step,lower_time_bound,upper_time_bound,cell_method,offset,scale,deaccum,derived,interface",
#               "tasmax,TSMX,1mo,0,12,max,-273.15,1,0,0,",
#               "tasmin,TSMN,1mo,0,12,min,-273.15,1,0,0,",
#               "tas,TS,1mo,0,12,mean,-273.15,1,0,0,",
#               "pr,PRECC,1mo,0,12,mean,0,2592000000,0,0,",
#               "hurs,RELHUM,1mo,0,12,mean,0,1,0,0,",
#               "cld,CLDTOT,1mo,0,12,mean,0,1,0,0,",
#               "ps,PS,1mo,0,12,mean,0,1,0,0,",
#               "u,U,1mo,0,12,mean,0,1,0,0,",
#               "v,V,1mo,0,12,mean,0,1,0,0,"),
#             "Data/TraCE21ka/TraCE21ka_dictionary.dic")

## ----create_uerra_dictionary, eval = FALSE------------------------------------
#  file.create("Data/UERRA/UERRA_dictionary.dic")
#  
#  writeLines(c("identifier,short_name,time_step,lower_time_bound,upper_time_bound,cell_method,offset,scale,deaccum,derived,interface",
#               "tasmin,2t,1mo,1,12,min,-273.15,1,0,0,",
#               "tasmax,2t,1mo,1,12,max,-273.15,1,0,0,",
#               "tas,2t,1mo,1,12,mean,-273.15,1,0,0,",
#               "pr,tp,1mo,1,12,sum,0,1,0,0,",
#               "hurs,2r,1mo,1,12,mean,0,1,0,0,",
#               "cld,tcc,1mo,1,12,mean,0,1,0,0,",
#               "wss,10si,1mo,1,12,mean,0,1,0,0,",
#               "ps,sp,1mo,1,12,mean,0,1,0,0,"),
#             "Data/UERRA/UERRA_dictionary.dic")

## ----create_cmip5_dictionary, eval = FALSE------------------------------------
#  
#  file.create("Data/CMIP5/CMIP5_dictionary.dic")
#  
#  writeLines(c("identifier,short_name,time_step,lower_time_bound,upper_time_bound,cell_method,offset,scale,deaccum,derived,interface",
#  "tasmax,tasmax,1mo,0,12,max,-273.15,1,0,0,",
#  "tasmin,tasmin,1mo,0,12,min,-273.15,1,0,0,",
#  "tas,tas,1mo,0,12,mean,-273.15,1,0,0,",
#  "pr,pr,1mo,0,12,mean,0,2592000,0,0,",
#  "hurs,hurs,1mo,0,12,mean,0,1,0,0,",
#  "cld,clt,1mo,0,12,mean,0,1,0,0,",
#  "ps,ps,1mo,0,12,mean,0,1,0,0,",
#  "wss,sfcWind,1mo,0,12,mean,0,1,0,0,"
#  ), "Data/CMIP5/CMIP5_dictionary.dic")

