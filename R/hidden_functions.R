

#' Title
#'
#' @param x TBW
#' @param y TBW
#'
#' @return TBW
# .elementwise.all.equal <- function(x, y){Vectorize(function(x, y) {isTRUE(all.equal(x, y))})} #Modified for documenting
.elementwise.all.equal <- Vectorize(function(x, y) {isTRUE(all.equal(x, y))}) #Working version


#' Title
#'
#' @param st TBW
#' @param en TBW
#' @param freq TBW
#' @param frac TBW
#'
#' @import zoo
#' @return TBW
#'
#' @examples # TBW
.DateSeq <- function(st, en, freq, frac=0) {
  st <- zoo::as.Date(zoo::as.yearmon(st))
  en <- zoo::as.Date(zoo::as.yearmon(en))
  result <- zoo::as.Date(zoo::as.yearmon(seq(st, en, by = paste(as.character(12/freq), "months"))), frac = frac)
  return(result)
}

# loadeR.java::javaString2rChar(Sys.Date())
