## code to prepare `DATASET` dataset goes here

calBP.years <- c(-22000:-1, 1:150)

calendar.years <- c(-20050:-1, 1:2100)

# C4R.vocabulary.update("cld", "Vertically-integrated total cloud cover", "%")

cmip5.rcps <- c("rcp2.6", "rcp4.5", "rcp6.0", "rcp8.5")
cmip5.vars <- c("tas", "tasmax", "tasmin", "hurs", "ps", "pr", "clt", "sfcWind")
cmip5.new.vars <- c("tas", "tasmax", "tasmin", "hurs", "ps", "pr", "cld", "wss")
cmip5.mods <- c("CESM1-CAM5", "CSIRO-Mk3-6-0", "IPSL-CM5A-MR")


# trace.vars = c("TS", "TSMX", "TSMN", "RELHUM", "PS", "PRECC", "CLDTOT", "U", "V")
trace.source.var.names <- c("TS", "TSMX", "TSMN", "RELHUM", "PS", "PRECC", "CLDTOT", "U", "V")

# trace.var.names = c("tas", "tasmax", "tasmin", "hurs@992.5561", "ps", "pr", "cld", "u@992.5561", "v@992.5561")
trace.standard.var.names <- c(
  "tas", "tasmax", "tasmin", "hurs@992.5561", "ps",
  "pr", "cld", "u@992.5561", "v@992.5561"
)

# trace.model.var.names = c("tas", "tasmax", "tasmin", "hurs@992.5561", "ps", "pr", "cld", "wss")
trace.final.var.names <- c("tas", "tasmax", "tasmin", "hurs@992.5561", "ps", "pr", "cld", "wss")

trace.years.n <- sprintf("%02d", 1:36)

trace.years.bp <- c("22000-20001BP", "20000-19001BP", "19000-18501BP", "18500-18401BP", "18400-17501BP", "17500-17001BP", "17000-16001BP", "16000-15001BP", "15000-14901BP", "14900-14351BP", "14350-13871BP", "13870-13101BP", "13100-12901BP", "12900-12501BP", "12500-12001BP", "12000-11701BP", "11700-11301BP", "11300-10801BP", "10800-10201BP", "10200-09701BP", "09700-09201BP", "09200-08701BP", "08700-08501BP", "08500-08001BP", "08000-07601BP", "07600-07201BP", "07200-06701BP", "06700-06201BP", "06200-05701BP", "05700-05001BP", "05000-04001BP", "04000-03201BP", "03200-02401BP", "02400-01401BP", "01400-00401BP", "400BP-1990CE")

trace.years.nums <- c("0000101-0200012", "0200101-0300012", "0300101-0350012", "0350101-0360012", "0360101-0450012", "0450101-0500012", "0500101-0600012", "0600101-0700012", "0700101-0710012", "0710101-0765012", "0765101-0813012", "0813101-0890012", "0890101-0910012", "0910101-0950012", "0950101-1000012", "1000101-1030012", "1030101-1070012", "1070101-1120012", "1120101-1180012", "1180101-1230012", "1230101-1280012", "1280101-1330012", "1330101-1350012", "1350101-1400012", "1400101-1440012", "1440101-1480012", "1480101-1530012", "1530101-1580012", "1580101-1630012", "1630101-1700012", "1700101-1800012", "1800101-1880012", "1880101-1960012", "1960101-2060012", "2060101-2160012", "2160101-2204012")

trace.years.y1 <- c(22000, 20000, 19000, 18500, 18400, 17500, 17000, 16000, 15000, 14900, 14350, 13870, 13100, 12900, 12500, 12000, 11700, 11300, 10800, 10200, 09700, 09200, 08700, 08500, 08000, 07600, 07200, 06700, 06200, 05700, 05000, 04000, 03200, 02400, 01400, 400)

trace.years.y2 <- c(20001, 19001, 18501, 18401, 17501, 17001, 16001, 15001, 14901, 14351, 13871, 13101, 12901, 12501, 12001, 11701, 11301, 10801, 10201, 09701, 09201, 08701, 08501, 08001, 07601, 07201, 06701, 06201, 05701, 05001, 04001, 03201, 02401, 01401, 00401, -00040)

usethis::use_data(calBP.years, calendar.years, cmip5.rcps, cmip5.vars, cmip5.new.vars, cmip5.mods, trace.source.var.names, trace.standard.var.names, trace.final.var.names, trace.years.n, trace.years.bp, trace.years.nums, trace.years.y1, trace.years.y2, internal = TRUE, overwrite = TRUE)
