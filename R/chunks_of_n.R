#' MOVED Split to chunks of size n
#'
#' @param d data. Can be vector or data frame.
#' @param n number of chunks
#' @param label naming prefix for chunk names
#' @param even boolean to set if size of chunks should be evenly distributed.
#' @param pattern regex pattern to extract names from provided vector. If data
#' frame, will assume first column is name.
#'
#' @return List of length n
#' @export
#'
#' @examples
#' tail(chunks_of_n(seq_len(100),7),3)
#' tail(chunks_of_n(seq_len(100),7,even=TRUE),3)
#' ds <- data.frame(nm=paste0("Sub",
#' add_padding(rownames(stRoke::talos))),stRoke::talos)
#' head(chunks_of_n(ds,7,pattern="Sub[0-9]{3}",label="grp"),2)
#' ## Please notice that no sorting is performed. This is on purpose to preserve
#' ## original sorting. If sorting is intended, try something like this:
#' ds[order(ds$nm),] |> chunks_of_n(7,pattern="Sub[0-9]{3}",label="grp") |> 
#' head(2)

chunks_of_n <- function(d,n,label=NULL, even=FALSE, pattern=NULL){
  
  if (!(is.vector(d) |
        is.data.frame(d)) |
      inherits(d,"list")) {
    stop("Provided data is not vector or data.frame.")
  }
  
  if (is.data.frame(d)) ns <- nrow(d) else ns <- length(d) 
  
  if (even) {
    g <- sort(rep_len(seq_len(ceiling(ns / n)), ns))
  } else {
    g <- ceiling(seq_len(ns) / n)
  }
  
  ls <- split(d, g)
  
  if (!is.null(pattern)) {
    if(is.data.frame(d)) {
      ns <- str_extract(d=d[[1]],pattern=pattern)
      } else ns <- str_extract(d=d,pattern=pattern)
    
    
    suffix <- do.call(c, lapply(split(ns, g), function(i) {
      paste0(i[[1]], "-", i[[length(i)]])
    }))
  } else suffix <- names(ls)
  
  if (is.character(label)){
    names(ls) <- paste0(label,"-",suffix)
    } else names(ls) <- suffix
  
  ls
}

#' Splits in n chunks
#'
#' @param d data
#' @param n number of chunks
#' @param ... arguments passed to internal `chunks_of_n()`
#'
#' @return List of chunks
#' @export
#'
#' @examples
#' lengths(n_chunks(d=seq_len(100),n=7,even=TRUE))
#' lengths(n_chunks(d=seq_len(100),n=7,even=FALSE))
n_chunks <- function(d,n,...){
  if (!(is.vector(d) |
        is.data.frame(d)) |
      inherits(d,"list")) {
    stop("Provided data is not vector or data.frame.")
  }
  
  if (is.data.frame(d)) ns <- nrow(d) else ns <- length(d) 
  
  nn <- ceiling(ns/n)
  
  chunks_of_n(d=d,n=nn,...)
}
