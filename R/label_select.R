#' Helper function for labels in gtsummary
#' 
#' Function to select labels from list of label pairs (format: `age~"Age"`). 
#' Alternative is to use attributes, eg from `library(Hmisc)`.
#'
#' @param lst List of variables and labels (format: `age~"Age"`)
#' @param vec Vector of variables to be subset from the list
#'
#' @return List of labels ordered like vec, formatted like lst
#' @export
#' 
#' @importFrom gtsummary tbl_summary
#'
#' @examples
#' vars<-c("hypertension", "diabetes", "mrs_1")
#' labels_all<-list(rtreat~"Trial treatment",
#' civil~"Cohabitation",
#' diabetes~"Known diabetes", 
#' hypertension~"Known hypertension", 
#' mrs_1~"One month mRS", 
#' mrs_6~"Six months mRS", 
#' '[Intercept]'~"Intercept")
#' label_select(labels_all,vars)
#' 
#' ## With gtsummary::tbl_summary()
#' #stRoke::talos[vars] |> 
#' #gtsummary::tbl_summary(label = label_select(labels_all,vars))
label_select<-function(lst,vec){
  lst[match(vec,unlist(lapply(lst,function(i){i[[2]]})))]
}




