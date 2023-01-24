

#' @title Filter files in a folder
#' @description This function filters files in a folder based on the 
#' provided filter.
#' @param folder.path character. Path of the folder to be filtered
#' @param filter.by character. Filter to be applied on the files
#' @param full.names logical. Whether to return full file names or not
#' @return character vector. Filtered files
#' @export
#'
#' @examples
#' # Gives path to files/folders with "tests" in the name in the 
#' # working directory
#' files_filter(getwd(),"tests")
#' @import utils
files_filter <- function(folder.path,filter.by,full.names=TRUE){
  
  #list all files in the folder
  files <- list.files(path=folder.path, full.names=full.names) 
  
  #filter files
  files[grepl(filter.by,files)] 

}



