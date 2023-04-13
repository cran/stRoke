utils::globalVariables(c("df","group","score","strata"))

#' Generic stroke study outcome
#' 
#' Includes table 1, grotta bars and ordinal logistic regression plot. 
#' Please just use this function for illustration purposes.
#' To dos: modify grottaBar and include as own function.
#'
#' @param df Data set as data frame
#' @param group Variable to group by
#' @param score Outcome measure variable
#' @param strata Optional variable to stratify by
#' @param variables String of variable names to include in adjusted OLR-analysis
#'
#' @return Returns list with three elements
#' @export
#'
#' @import ggplot2
#' @importFrom gtsummary tbl_summary
#' @importFrom gtsummary add_overall
#' @importFrom MASS polr
#' @importFrom rankinPlot grottaBar
#' @importFrom stats as.formula
#'
#' @examples
#' # generic_stroke(df = stRoke::talos, group = "rtreat", score = "mrs_6", 
#' # variables = c("hypertension","diabetes","civil"))
generic_stroke <-
  function(df,
           group,
           score,
           strata = NULL,
           variables = NULL){
    t1 <- gtsummary::tbl_summary(data = df[, c(group, variables)],
                                 by = group) |>
      gtsummary::add_overall()
    
    x <- table(df[, c(group, score, strata)])
    f1 <- rankinPlot::grottaBar(
      x = x,
      groupName = group,
      scoreName = score,
      strataName = strata,
      colourScheme = "custom"
    )
    
    df[, score] <- factor(df[, score], ordered = TRUE)
    
    f2 <- ci_plot(MASS::polr(
      as.formula(paste0(score, "~.")),
      data = df[, c(group, score, variables)],
      Hess = TRUE,
      method = "logistic"
    ),
    method = "model")
    
    list("Table 1" = t1,
         "Figure 1" = f1,
         "Figure 2" = f2)
  }

