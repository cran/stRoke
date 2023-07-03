#' PASE score calculator
#' 
#' Calculates PASE score from raw questionnaire data.
#' @param ds data set
#' @param adjust_work flag to set whether to include 10b type 1. 
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
#' 
pase_calc <- function(ds, adjust_work = FALSE) {
  
  if (ncol(ds) != 21) stop("supplied data set has to contain exactly 21 columns")
  
  pase <- ds
  
  ## Classify all as characters
  ## Labelling should be as defined by the questionnaire.
  ## 02-06 should start with 0:3, 02a-06b should start with 1:4.
  
  pase <- do.call(data.frame, lapply(pase, as.character))
  
  ## Missings and incompletes
  missings <- apply(apply(ds, 2, is.na), 1, all)
  incompletes <-
    apply(sapply(ds[, c(1, 3, 5, 7, 9, 11, 13:20)], function(x) {
      x == "Not available" | is.na(x)
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
  
  pase_weigths <- list(
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
    score <- c()
    
    ## =====================
    ## Checking labelling
    if (!all(range(suppressWarnings(as.numeric(substr(
      df[, 1], 1, 1
    ))), na.rm = TRUE) == c(0, 3))) {
      stop("Labelling of 02-06 should start with a number ranging 1-4")
    }
    ## =====================
    
    for (i in seq_len(nrow(df))) {
      # Setting categories from numbers
      n1 <- suppressWarnings(as.numeric(substr(df[, 1][i], 1, 1)))
      
      # Using if statement to calculate row wise
      if (n1 %in% c(1:3)) {
        # Second category
        n2 <- suppressWarnings(as.numeric(substr(df[, 2][i], 1, 1)))
        score[i] <- pase_weigths[[n1]][n2] * pase_multip_26[x] 
        
      } else if (n1 %in% 0) {
        score[i] <- 0
      } else {
        score[i] <- NA
      }
    }
    score
  })
  
  names(pase_score_26) <- paste0("score_", names(pase_list[2:6]))
  
  ## PASE 7-9d
  pase_multip_79 <- c(25, 25, 30, 36, 20, 35)
  
  pase_score_79 <-
    data.frame(t(t(
      sapply(Reduce(cbind,pase_list[7:9]),function(j){
        grepl("[Jj]a",j)
      }) + 0 # short hand logic to numeric
    ) * pase_multip_79))
  
  names(pase_score_79) <-
    paste0("score_", sub("pase", "", names(pase_score_79)))
  
  ## PASE 10
  ## Completely ignores if 10b is not completed
  pase_score_10 <- 21 * suppressWarnings(as.numeric(pase_list[[10]][[2]])) / 7
  
  if (adjust_work){
    # Only includes work time if 10b is != 1
    pase_score_10[substr(pase_list[[10]][[3]],1,1) == "1"] <- 0
    # Consequently consider "Not available" in 10b as incomplete
    incompletes[ds[,21] == "Not available" & !incompletes & !is.na(incompletes)] <- TRUE
  }
  
  pase_score <- cbind(pase_score_26, pase_score_79, pase_score_10)
  
  data.frame(
    pase_score,
    score_sum = rowSums(pase_score, na.rm = TRUE),
    score_missings = missings,
    score_incompletes = incompletes
  )
}
