#' Extract string based on regex pattern
#' 
#' DEPRECATION: moved to `agdamsbo/project.aid`
#' 
#' Use base::strsplit to 
#' @param d vector of character strings
#' @param pattern regex pattern to match
#'
#' @return vector of character strings
#' @export
#'
#' @examples
#' ls <- do.call(c,lapply(sample(4:8,20,TRUE),function(i){
#' paste(sample(letters,i,TRUE),collapse = "")}))
#' ds <- do.call(c,lapply(1:20,function(i){
#' paste(sample(ls,1),i,sample(ls,1),"23",sep = "_")}))
#' str_extract(ds,"([0-9]+)")
str_extract <- function(d,pattern){
  if (!is.vector(d)) stop("Please provide a vector")
  
  ## Drawing on the solution in REDCapCAST::strsplitx to split around pattern
  nl <- strsplit(gsub("~~", "~", # Removes double ~
                      gsub("^~", "", # Removes leading ~
                           gsub(
                             # Splits and inserts ~ at all delimiters
                             paste0("(", pattern, ")"), "~\\1~", d
                           ))), "~")
  
  ## Reusing the pattern, to sub with "" and match on length 0 to index the
  ## element containing the pattern. Only first occurance included.
  indx <- lapply(nl,function(i){
    match(0,nchar(sub(pattern,"",i)))
    })
  
  ## Using lapply to subsset the given index for each element in list
  do.call(c,lapply(seq_along(nl), function(i){
    nl[[i]][indx[[i]]]
  } ))
}
