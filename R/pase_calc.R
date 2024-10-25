#' PASE score calculator
#' 
#' Calculates PASE score from raw questionnaire data.
#' @param ds data set
#' @param adjust_work flag to set whether to include 10b type 1. 
#' @param consider.missing character vector of values considered missing.
#' Default is TRUE.
#'
#' @return data.frame
#' @export
#' @details
#' Labelling should be as defined by the questionnaire.
#' 02-06 should start with 0:3, 02a-06b should start with 1:4.
#' 
#' ## Regarding work scoring
#' The score calculation manual available for the PASE questionnaire, all types
#' of work should be included. According to the article by 
#' Washburn RA. et al (1999) sitting work is not included in the item 10 score.
#' This differentiation is added with the option to set `adjust_work` to
#' exclude item 10b category 1 work (set `TRUE`).
#' 
#' ## Regarding output
#' Output includes sub scores as well as sums, but also to columns assessing data 
#' quality and completeness. If any field has not been filled, `score_incompletes`
#' will return `TRUE`. If all measures are missing `score_missings` is `TRUE`.
#' If `adjust_work==TRUE`, 10b has to be filled, or `score_incompletes` will be
#' set `TRUE`.
#' 
#' @examples
#' summary(pase_calc(stRoke::pase)[,13])
#' str(pase_calc(stRoke::pase))
#' 
pase_calc <- function(ds, 
                      adjust_work = FALSE, 
                      consider.missing = c("Not available")) {
  
  if (ncol(ds) != 21) {
    stop("supplied data set has to contain exactly 21 columns.
         Formatting should follow the stRoke::pase data set.")
    }
  
  pase <- ds
  
  ## Classify all as characters
  ## Labelling should be as defined by the questionnaire.
  ## 02-06 should start with 0:3, 02a-06b should start with 1:4.
  
  pase <- do.call(data.frame, lapply(pase, as.character))
  
  ## Missings and incompletes
  # Cosidered missing if all data is missing
  missings <- apply(apply(ds, 2, is.na), 1, all)
  
  # Considered incomplete if any entry in main answers is missing
  mains <- grep("([0-9]{2}|(09[a-d]))$",colnames(pase))
  
  if (length(mains)!=13){
    stop("The supplied dataset does not contain expected variable names.
         Please run str(stRoke::pase) and format your data accordingly.")
    }
  
  incompletes <-
    apply(sapply(ds[, mains], function(x) {
      x %in% consider.missing | is.na(x)
    }), 1, any)
  
  names(pase) <- c(
    "pase01",
    "pase01b",
    "pase02",
    "pase02a",
    "pase03",
    "pase03b",
    "pase04",
    "pase04b",
    "pase05",
    "pase05b",
    "pase06",
    "pase06b",
    "pase07",
    "pase08",
    "pase09a",
    "pase09b",
    "pase09c",
    "pase09d",
    "pase10",
    "pase10a",
    "pase10b"
  )
  
  pase_list <- lapply(unique(substr(names(pase), 5, 6)), function(x) {
    pase[grepl(x, substr(names(pase), 5, 6))]
  })
  names(pase_list) <- unique(substr(names(pase), 5, 6))
  
  ## PASE 2-6
  
  pase_weights <- list(
    "1" = c(
      "1" = 0.11,
      "2" = 0.32,
      "3" = 0.64,
      "4" = 1.07
    ),
    "2" = c(
      "1" = 0.25,
      "2" = 0.75,
      "3" = 1.5,
      "4" = 2.5
    ),
    "3" = c(
      "1" = 0.43,
      "2" = 1.29,
      "3" = 2.57,
      "4" = 4.29
    )
  )
  
  ## Multiplication factors
  pase_multip_26 <- c(20, 21, 23, 23, 30)
  
  pase_score_26 <- lapply(seq_along(pase_list[2:6]), function(x) {
    df <- pase_list[2:6][[x]]
    # score <- c()
    
    ## =====================
    ## Checking labelling
    if (!all(stRoke::str_extract(df[, 1], "^[0-3]") |> 
             as.numeric() |> 
             range(na.rm = TRUE) == c(0, 3))) {
      stop("Labelling of 02-06 should start with a number ranging 0-3")
    }
    
    if (!all(stRoke::str_extract(df[, 2], "^[1-4]") |> 
             as.numeric() |> 
             range(na.rm = TRUE) == c(1, 4))) {
      stop("Labelling of 02-06 subscores should start with a number ranging 1-4")
    }
    
    ## =====================
    
    ## Extracting the first string element in main entry
    n1 <- stRoke::str_extract(df[, 1],"^[0-3]") |> as.numeric()
    ## Extracting the first string element in subentry
    n2 <- stRoke::str_extract(df[, 2],"^[1-4]") |> as.numeric()
    
    score <- c()
    for (i in seq_along(n1)) {
      
      ind1 <- match(n1[i],seq_along(pase_weights))
      
      if (is.na(ind1)){
        score[i] <- n1[i]
      } else {
        score[i] <- pase_weights[[ind1]][n2[i]] * pase_multip_26[x] 
      }
      
    }
    score
  })
  
  names(pase_score_26) <- paste0("pase_score_", names(pase_list[2:6]))
  
  ## PASE 7-9d
  pase_multip_79 <- c(25, 25, 30, 36, 20, 35)
  
  pase_score_79 <-
    data.frame(t(t(
      sapply(Reduce(cbind,pase_list[7:9]),function(j){
        grepl("[Jj]a",j)
      }) + 0 # short hand logic to numeric
    ) * pase_multip_79))
  
  names(pase_score_79) <-
    paste0("pase_score_", sub("pase","",names(pase_score_79)))
  
  ## PASE 10
  ## Completely ignores if 10b is not completed
  pase_score_10 <- 21 * suppressWarnings(as.numeric(pase_list[[10]][[2]])) / 7
  
  if (adjust_work){
    # Only includes work time if 10b is != 1
    pase_score_10[substr(pase_list[[10]][[3]],1,1) == "1"] <- 0
    # Consequently consider "Not available" in 10b as incomplete
    incompletes[ds[,21] %in% consider.missing & !incompletes & !is.na(incompletes)] <- TRUE
  }
  
  pase_score <- cbind(pase_score_26, pase_score_79, pase_score_10)
  
  data.frame(
    pase_score,
    pase_score_sum = rowSums(pase_score, na.rm = TRUE),
    pase_score_missings = missings,
    pase_score_incompletes = incompletes
  )
}
