% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/label_select.R
\name{label_select}
\alias{label_select}
\title{Helper function for labels in gtsummary}
\usage{
label_select(lst, vec)
}
\arguments{
\item{lst}{List of variables and labels (format: \code{age~"Age"})}

\item{vec}{Vector of variables to be subset from the list}
}
\value{
List of labels ordered like vec, formatted like lst
}
\description{
Function to select labels from list of label pairs (format: \code{age~"Age"}).
Alternative is to use attributes, eg from \code{library(Hmisc)}.
}
\examples{
vars<-c("hypertension", "diabetes", "mrs_1")
labels_all<-list(rtreat~"Trial treatment",
civil~"Cohabitation",
diabetes~"Known diabetes", 
hypertension~"Known hypertension", 
mrs_1~"One month mRS", 
mrs_6~"Six months mRS", 
'[Intercept]'~"Intercept")
label_select(labels_all,vars)

## With gtsummary::tbl_summary()
#stRoke::talos[vars] |> 
#gtsummary::tbl_summary(label = label_select(labels_all,vars))
}
