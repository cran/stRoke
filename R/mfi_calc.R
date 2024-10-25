utils::globalVariables(c("ndx"))
#' Reverses relevant MFI subscores
#'
#' @param d data frame or tibble
#' @param var numeric vector of indices of columns to reverse
#'
#' @return data.frame or tibble depending on input
#'
#' @examples
#' # rep_len(sample(1:5),length.out = 100) |> matrix(ncol=10) |> multi_rev(2:4)
multi_rev <- function(d, var){
  # Forcing and coercing to numeric
  dm <- d |> as.matrix() |> 
    as.numeric()|> 
    matrix(ncol=ncol(d)) |> 
    data.frame()
  
  # Reversing everything (fast enough not to subset)
  dr <- range(dm,na.rm=TRUE) |> sum()-dm
  
  # Inserting reversed scores in correct places
  for (i in var){
    dm[i] <- dr[i]
  }
  
  if (tibble::is_tibble(d)){
    tibble::tibble(dm)
  } else {
      dm
    }

}

#' MFI domain score calculator
#'
#' @param ds data set of MFI scores, 20 columns
#' @param reverse.vars variables/columns to reverse
#' @param reverse reverse scoring
#'
#' @return tibble of domain scores
#' @export
#'
#' @examples
#' mfi_mess <- data.frame(matrix(
#' sample(c(" 1. ", "2. -A", "3.", " 4  ", "5.", NA),200,replace=TRUE),ncol=20))
#' mfi_mess |> mfi_domains()
mfi_domains <-
  function(ds,
           reverse = TRUE,
           reverse.vars = c(2, 5, 9, 10, 13, 14, 16, 17, 18, 19)) {
    
  if(ncol(ds)!=20){
    stop("The supplied dataset should only contain the 20 MFI subscores")}
  
  # Subscore indexes
  indexes <- list(
    data.frame(grp="gen", ndx=c(1, 5, 12, 16)),
    data.frame(grp="phy", ndx=c(2, 8, 14, 20)),
    data.frame(grp="act", ndx=c(3, 6, 10, 17)),
    data.frame(grp="mot", ndx=c(4, 9, 15, 18)),
    data.frame(grp="men", ndx=c(7, 11, 13, 19))
  ) |> dplyr::bind_rows() |> dplyr::arrange(ndx)
  
  # Removes padding and converts to numeric
  ds_n <- ds |> dplyr::mutate_if(is.factor, as.character) |> 
    dplyr::mutate(dplyr::across(tidyselect::everything(),
                  # Removes everything but the leading alphanumeric character
                  # Data should be cleaned accordingly
                  ~str_extract(d=.,pattern="[[:alnum:]]")))
  
  # Assumes reverse scores are not correctly reversed
  if (reverse){ds_n <- ds_n |> multi_rev(var=reverse.vars)}
  
  # Domain wise summations
  split.default(ds_n, factor(indexes$grp)) |> 
    lapply(function(x){
      apply(x, MARGIN = 1, sum, na.ignore=FALSE)
    }) |> dplyr::bind_cols()
  
  }
