#' Calculate age in years, months, or days
#' 
#' @param dob Date of birth
#' @param enddate End date for age calculation (default is Sys.Date())
#' @param units Units for age calculation (default is "years"). 
#'              Can be c("days", "months", "years")
#' @param precise Option to calculate age precisely (default is TRUE)
#' @return numeric vector length 1
#' @export
#' 
#' @examples
#' trunc(age_calc(as.Date("1945-10-23"),as.Date("2018-09-30")))
#' 
#' @references 
#' Becker, J.P. (2020). eeptools: An R Package for Teaching and Learning 
#' Ecology and Evolutionary Biology. Journal of Statistical Software, 
#' 93(2), 1-27.
#' @source \doi{10.18637/jss.v093.i02}
#' 
#' @keywords date time age

age_calc<-function (dob, enddate = Sys.Date(), units = "years", precise = TRUE)
{
  if (!inherits(dob, "Date") | !inherits(enddate, "Date")) {
    stop("Both dob and enddate must be Date class objects")
  }
  
  if (length(dob)==1 && enddate < dob) {
    stop("End date must be a date after date of birth")
  }
  
  if (length(dob)>1 && any(enddate < dob)) {
    stop("End date must be a date after date of birth")
  }
  
  start <- as.POSIXlt(dob)
  end <- as.POSIXlt(enddate)
  
  if (precise) {
    start_is_leap <- ifelse(start$year%%400 == 0, TRUE, 
                            ifelse(start$year%%100 == 0, 
                                   FALSE, 
                                   ifelse(start$year%%4 == 0, TRUE, FALSE)))
    end_is_leap <- ifelse(end$year%%400 == 0, TRUE, 
                          ifelse(end$year%%100 == 0, 
                                 FALSE, 
                                 ifelse(end$year%%4 == 0, TRUE, FALSE)))
  }
  if (units == "days") {
    result <- as.numeric(difftime(end, start, units = "days"))
  }
  else if (units == "months") {
    months <- vapply(
      mapply(
        seq,
        as.POSIXct(start),
        as.POSIXct(end),
        by = "months",
        SIMPLIFY = FALSE
      ),
      length,
      numeric(1)
    ) - 1
    
    
    
    if (precise) {
      month_length_end <- ifelse(end$mon == 1 & end_is_leap,
                                 29, ifelse(end$mon == 1, 28,
                                            ifelse(end$mon %in% c(3, 5, 8, 10),
                                                   30, 31)))
      month_length_prior <- ifelse((end$mon - 1) == 1 &
                                     start_is_leap, 29, 
                                   ifelse((end$mon - 1) == 1, 28, 
                                          ifelse((end$mon - 1) %in% 
                                                   c(3, 5, 8, 10), 30, 31)))
      month_frac <- ifelse(end$mday > start$mday, 
                           (end$mday - start$mday)/month_length_end, 
                           ifelse(end$mday < start$mday, 
                                  (month_length_prior - 
                                     start$mday)/month_length_prior + 
                                    end$mday/month_length_end, 0))
      result <- months + month_frac
    }
    else {
      result <- months
    }
  }
  else if (units == "years") {
    years <- vapply(
      mapply(
        seq,
        as.POSIXct(start),
        as.POSIXct(end),
        by = "years",
        SIMPLIFY = FALSE
      ),
      length,
      numeric(1)
    ) - 1
    if (precise) {
      start_length <- ifelse(start_is_leap, 366, 365)
      end_length <- ifelse(end_is_leap, 366, 365)
      start_day <- ifelse(start_is_leap & start$yday >=
                            60, start$yday - 1, start$yday)
      end_day <- ifelse(end_is_leap & end$yday >= 60, end$yday -
                          1, end$yday)
      year_frac <- ifelse(start_day < 
                            end_day, (end_day - start_day)/end_length, 
                          ifelse(start_day > end_day,
                                 (start_length - start_day)/start_length + 
                                   end_day/end_length, 0))
      result <- years + year_frac
    }
    else {
      result <- years
    }
  }
  
  else {
    stop("Unrecognized units. Please choose years, months, or days.")
  }
  result
}
