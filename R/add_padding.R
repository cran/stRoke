#' MOVED Add padding to string
#'
#' @param d vector of strings or numbers
#' @param length final string length
#' @param after if padding should be added after as opposed to default before
#' @param pad padding string of length 1
#' @param lead leading string for all. Number or character vector. Cycled.
#' @param tail tailing string for all. Number or character vector. Cycled.
#'
#' @return vector or character strings of same length.
#' @export
#'
#' @examples
#' add_padding(sample(1:200,5),tail="AA",lead=c(2,3,"e"))
add_padding <- function(d,length=NULL,after=FALSE,pad="0",lead=NULL,tail=NULL){
  if (!is.vector(d)) {
    stop("Please supply vector")
  }
  
  if (nchar(pad)!=1) {
    stop("Padding value should be just a single character or digit")
  }
  
  ns <- nchar(d)
  
  if (is.null(length)){
    l <- max(ns)
  } else {
    l <- length
  }
  
  ps <- unlist(lapply(l-ns,function(i){
    paste(rep(pad,i),collapse="")}))
  
  if (after) {
    out <- paste0(d,ps)
  } else {
    out <- paste0(ps,d)
  }
  
  paste0(lead,out,tail)

}
