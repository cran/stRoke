

#' @title Calculates the probability of winning
#' @description Calculates the probability of winning (winP). In the referenced 
#' article Zou et al (2022) proposes a method for calculating probability of
#' winning with a confidence interval an p-value testing.
#' @param data A data frame containing the response and group variable.
#' @param response The name of the response variable.
#' Takes first column if empty.
#' @param group The name of the group variable.
#' Takes second column if empty.
#' @param alpha The alpha level for the hypothesis test. Default is 0.05.
#' @param beta The beta level for the sample size calculation. Default is 0.2.
#' @param group.ratio The ratio of group sizes. Default is 1.
#' @param sample.size Flag to include sample size calculation. Default is FALSE.
#' @param print.tables Flag to print cumulative tables. Default is FALSE.
#' @param dec Numeric for decimals to print. Default is 3.
#' @return A list containing the win_prob statistics. 
#' @export
#' @importFrom stats pnorm qnorm xtabs
#' @source \doi{10.1161/STROKEAHA.121.037744}
#' @examples
#' win_prob(data=stRoke::talos,response="mrs_6",group="rtreat")
win_prob <-
  function(data,
           response = NULL,
           group = NULL,
           alpha = 0.05,
           beta = 0.2,
           group.ratio = 1,
           sample.size = FALSE,
           print.tables = FALSE,
           dec = 3) {
    if (is.null(response)) {
      response <- names(data)[1]
    }
    
    if (is.null(group)) {
      group <- names(data)[2]
    }
    
    group_levels <- levels(factor(data[, group]))
    
    if (length(group_levels) != 2) {
      stop("The group has to contain 2, and only 2 levels.")
    }
    
    if (!is.numeric(group.ratio)) {
      stop("Group ratio must be a numeric")
    }
    
    if (!is.logical(sample.size)) {
      stop("Sample size must be a logical")
    }
    
    # Internal helper function for calculating ranks
    freq_rank <- function(data, x = "Freq") {
      lapply(data, function(i) {
        rank <- c()
        n_i <- nrow(i)
        for (j in seq_len(n_i)) {
          if (j < n_i) {
            rank[j] <-
              ((i[j, x] + 1) / 2 + (sum(i[seq_len(n_i)[(j + 1):n_i], x])))
          }
          if (j == n_i) {
            rank[j] <- (i[j, x] + 1) / 2
          }
        }
        cbind(i, rank)
      })
    }
    
    overall <-
      freq_rank(list(data.frame(xtabs(data = data[c(response)]))))[[1]]
    
    tbl <- xtabs(data = data[c(response, group)])
    
    tbl_df <- data.frame(tbl)
    
    prop_df <- data.frame(proportions(tbl, group))
    
    df <- cbind(tbl_df, prop = prop_df[, "Freq"])
    
    list_cum <- split(df, df[, group])
    
    list_cum <- lapply(list_cum, function(i) {
      data.frame(i, overall_rank = overall$rank)
    })
    
    list_cum <- freq_rank(list_cum)
    
    sum_a <- sum(df$Freq[df$rtreat == group_levels[1]])
    sum_b <- sum(df$Freq[df$rtreat == group_levels[2]])
    
    list_cum[[1]]$win_frac <-
      with(list_cum[[1]], overall_rank - rank) / sum_b
    list_cum[[2]]$win_frac <-
      with(list_cum[[2]], overall_rank - rank) / sum_a
    
    winP_a <- sum(with(list_cum[[1]], prop * win_frac))
    winP_b <- sum(with(list_cum[[2]], prop * win_frac))
    
    var_win_frac_a <-
      sum(with(list_cum[[1]], prop * win_frac ^ 2)) - winP_a ^ 2
    var_win_frac_b <-
      sum(with(list_cum[[2]], prop * win_frac ^ 2)) - winP_b ^ 2
    
    var_win_prob <- var_win_frac_a / sum_a + var_win_frac_b / sum_b
    
    se_win_prob <- sqrt(var_win_prob)
    
    ci_up <-
      exp(log(winP_a / (1 - winP_a)) - qnorm(1 - alpha / 2) * se_win_prob /
            (winP_a / (1 - winP_a))) /
      (1 + exp(log(winP_a / (1 - winP_a)) - qnorm(1 - alpha / 2) *
                 se_win_prob / (winP_a / (1 - winP_a))))
    
    ci_lo <-
      exp(log(winP_b / (1 - winP_b)) + qnorm(1 - alpha / 2) * se_win_prob /
            (winP_b / (1 - winP_b))) /
      (1 + exp(log(winP_b / (1 - winP_b)) + qnorm(1 - alpha / 2) *
                 se_win_prob / (winP_b / (1 - winP_b))))
    
    test_stat <-
      abs(log(winP_a / (1 - winP_a))) / (se_win_prob / (winP_a * (1 - winP_a)))
    p_val <- 2 * (1 - pnorm(test_stat))
    
    nnt <- 1 / (winP_a - 0.5)
    
    ss_n <- NA
    
    if (sample.size) {
      ss_n <-
        ceiling((group.ratio + 1) / group.ratio *
                  (qnorm(1 - alpha / 2) + qnorm(1 - beta)) ^ 2 *
                  (var_win_frac_a + group.ratio * var_win_frac_b) /
                  (winP_a * (1 - winP_a) * log(winP_a / (1 - winP_a))) ^ 2
        )
    }
    
    out <- list(
      list_cum = list_cum,
      group_levels = group_levels,
      sum_a = sum_a,
      sum_b = sum_b,
      winP_a = winP_a,
      winP_b = winP_b,
      var_win_frac_a = var_win_frac_a,
      var_win_frac_b = var_win_frac_b,
      var_win_prob = var_win_prob,
      se_win_prob = se_win_prob,
      conf.int = c(ci_lo, ci_up),
      test_stat = test_stat,
      p_val = p_val,
      nnt = nnt,
      ss_n = ss_n,
      param.record = list(
        data = data,
        response = response,
        group = group,
        alpha = alpha,
        beta = beta,
        group.ratio = group.ratio,
        sample.size = sample.size,
        print.tables = print.tables,
        dec = dec
      )
    )
    class(out) <- c("win_Prop", class(out))
    return(out)
  }


print.win_Prop <- function (x, ...) {
  args <- list(...)
  
  cat("\t Zou et al's winP (doi: 10.1161/STROKEAHA.121.037744) \n\n")
  cat(
    sprintf(
      "Probability of a random observation in %s group 
      will have a higher response score than a random
      observation in %s group:\n\n",
      x$group_levels[2],
      x$group_levels[1]
    )
  )
  
  cat("  ")
  
  cat(
    sprintf(
      "\t   winP: %2.3f (%2.3f, %2.3f)      p=%1.4f",
      x$winP_a,
      x$conf.int[1],
      x$conf.int[2],
      x$p_val
    )
  )
  
  cat("\n")
  
  cat("--------------------------------------------\n\n")
  
  cat(sprintf("The numbers needed to treat (NNT) are: %s\n\n",
              ceiling(x$nnt)))
  
  cat("\n")
  
  if (x$param.record$sample.size) {
    cat("--------------------------------------------\n\n")
    
    cat(
      sprintf(
        "\tWith %s/%s ratio = %s and beta = %s
              the sample size needed is: %s\n\n",
        x$group_levels[1],
        x$group_levels[2],
        x$param.record$group.ratio,
        x$param.record$beta,
        x$ss_n
      )
    )
  }
  
  cat("\n")
  
  if (x$param.record$print.tables) {
    cat("--------------------------------------------\n\n")
    
    lc <- lapply(x$list_cum, function(i, t = c("prop", "win_frac")) {
      i[t] <- round(i[t], x$param.record$dec)
      i[, !names(i) == x$param.record$group]
    })
    
    for (i in x$group_levels) {
      tab <- knitr::kable(lc[[i]], row.names = FALSE)
      cat(sprintf("Results for the %s group:\n", i))
      cat(sprintf("\t%s\n",
                  tab))
      cat("\n")
    }
  }
  return(invisible(x))
}
