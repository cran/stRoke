

#' MOVED Write ical object
#' 
#' This function creates an ical file based on a data frame with mixed events. 
#' Export as .ics file using `calendar::ic_write()`.
#' 
#' @param df A data frame with the calendar data
#' @param date The name of the event date column in the data frame
#' @param date.end The name of the end date column in the data frame
#' @param title The name of the title column in the data frame
#' @param time.start The name of the start time column in the data frame
#' @param time.end The name of the end time column in the data frame
#' @param place The name of the place column in the data frame
#' @param place.def Default location to use when place is NA
#' @param time.def Default start time to use when time.start is NA
#' @param time.dur Default duration of the event in minutes, if time.end is NA
#' @param descr Name of description/notes column if any.
#' @param link Name of link column, if any.
#' @param t.zone A character string of time zone for events. The string must be 
#' a time zone that is recognized by the user's OS.
#' 
#' @return ical object
#' 
#' @examples
#' df <- data.frame(
#'   date = c("2020-02-10", "2020-02-11"),
#'   date.end = c("2020-02-13",NA),
#'   title = c("Conference", "Lunch"),
#'   start = c("12:00:00", NA),
#'   time.end = c("13:00:00", NA),
#'   note = c("Hi there","Remember to come"),
#'   link = c("https://icalendar.org","https://agdamsbo.github.io/stRoke/")
#' )
#' 
#' write_ical(
#'   df,
#'   date = "date",
#'   date.end = "date.end",
#'   title = "title",
#'   time.start = "start",
#'   time.end = "time.end",
#'   place.def = "Conference Room",
#'   descr = "note",
#'   link = "link"
#' )
#' 
#' @export
#' 
#' @importFrom lubridate ymd hms dminutes
#' @importFrom dplyr if_else
#' @importFrom calendar ic_guid ic_write
#'
#' @seealso
#' [calendar package](https://github.com/ATFutures/calendar/)
#' [icalendar standard webpage](https://icalendar.org)
#'
#'
write_ical <-
  function(df,
           date = "date",
           date.end = NA,
           title = "title",
           time.start = "start",
           time.end = "end",
           place = NA,
           place.def = NA,
           time.def = "10:00:00",
           time.dur = 60,
           descr = NA,
           link = NA,
           t.zone = "CET") {
    if (!date %in% colnames(df)) {
      stop("Supplied date is not a valid column name")
    }
    
    if (!title %in% colnames(df)) {
      stop("Supplied title is not a valid column name")
    }
    
    if (any(is.na(df[,title]))) {
      stop("Missing title values are not allowed")
    }
    
    if (is.character(place) & !place %in% colnames(df)) {
      stop("Supplied place is not a valid column name")
    }
    
    if (is.character(time.start) & !time.start %in% colnames(df)) {
      stop("Supplied time.start is not a valid column name")
    }
    
    if (is.character(time.end) & !time.end %in% colnames(df)) {
      stop("Supplied time.end is not a valid column name")
    }
    
    # Both ifelse() and dplyr::if_else() has problems and gives errors 
    # handling NA's, as everything is evaluated.
    # This is my take on a approach by row.
    df <- do.call(rbind, 
                  lapply(
                    split(df,
                          seq_len(nrow(df))),
                    function(i) {
                      if (is.na(i[time.start])) {
                        i$start_time <-
                          lubridate::ymd(i[, date], tz = t.zone) +
                          lubridate::hms(time.def)
                      }
                      else if (!is.na(i[, time.start])) {
                        i$start_time <-
                          lubridate::ymd(i[, date], tz = t.zone) +
                          lubridate::hms(i[, time.start])
                      }
                      
                      if (is.character(date.end) &
                          is.na(i[, time.end]) &
                          !is.na(i[, date.end])) {
                        stop("time.end is missing for some date.end")
                      }
                      else if (is.character(date.end) &
                          !is.na(i[, time.end]) &
                          !is.na(i[, date.end])) {
                        i$end_time <-
                          lubridate::ymd(i[, date.end], tz = t.zone) +
                          lubridate::hms(i[, time.end])
                      }
                      else if (!is.na(i[, time.end])) {
                        i$end_time <-
                          lubridate::ymd(i[, date], tz = t.zone) +
                          lubridate::hms(i[, time.end])
                      } else {
                        i$end_time <- 
                          i$start_time + lubridate::dminutes(time.dur)
                      }
                      
                      i
                      
                    }))
    
    place_meet <- rep(NA, nrow(df))
    
    if (!is.na(place)) {
      place_meet <- df[, place]
    }
    
    place_meet[is.na(place_meet)] <- place.def
    
    df_mod <-  data.frame(
      SUMMARY = df[, title],
      DTSTART = df[, "start_time"],
      DTEND = df[, "end_time"],
      UID = replicate(nrow(df), calendar::ic_guid()),
      stringsAsFactors = FALSE
    )
    
    if (!all(is.na(place_meet))) {
      df_mod <-  data.frame(df_mod,
                            LOCATION = place_meet)
    }
    
    if (!is.na(link)) {
      df_mod <-  data.frame(df_mod,
                            URL = df[, link])
    }
    
    if (!is.na(descr)) {
      df_mod <-  data.frame(df_mod,
                            DESCRIPTION = df[, descr])
    }
    
    calendar::ical(df_mod)
  }
