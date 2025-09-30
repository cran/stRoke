#' Data frame with synthetic data generated from the TALOS trial cohort
#'
#' Contains data synthesized based on data in the TALOS trial cohort.
#' This is intended for educational use alone.
#'
#' @format A data frame with 642 rows and 16 variables:
#' \describe{
#'   \item{id}{ID}
#'   \item{active}{Active treatment}
#'   \item{age}{Age}
#'   \item{male}{Male sex}
#'   \item{nihss}{NIHSS score}
#'   \item{diabetes}{Known diabetes}
#'   \item{hypertension}{Known hypertension}
#'   \item{cohabitation}{Living alone}
#'   \item{pase_pre}{Pre-stroke PASE score}
#'   \item{pase_6}{6 months PASE score}
#'   \item{mfi_6}{6 months general fatigue score (MFI domain)}
#'   \item{mdi_6}{6 months MDI score}
#'   \item{who_6}{6 months WHO-5 score}
#'   \item{mrs_pre}{Pre-stroke mRS}
#'   \item{mrs_1}{1 month mRS}
#'   \item{mrs_6}{6 months mRS}
#' }
#' @source \doi{10.1161/STROKEAHA.117.020067}
#' @usage data(trial)
"trial"

