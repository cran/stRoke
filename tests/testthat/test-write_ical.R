test_that("write_ical() returns a ical object", {
 df <- data.frame(
   date = c("2020-02-10", "2020-02-11", "2020-02-11"),
   date.end = c("2020-02-13",NA,NA),
   title = c("Conference", "Lunch", "Walk"),
   start = c("12:00:00", NA, "08:00:00"),
   time.end = c("13:00:00", NA, "17:30:00"),
   note = c("Hi there","Remember to come", ""),
   link = c("https://icalendar.org","https://agdamsbo.github.io/stRoke/", "")
 )
  
 expect_s3_class(
   write_ical(
     df,
     date.end = "date.end",
     time.end = "time.end",
     place.def = "Home",
     descr = "note",
     link = "link"
   ),
   "ical"
 )
})

test_that("write_ical() returns a ical object", {
  df <- data.frame(
    date = c("2020-02-10", "2020-02-11", "2020-02-11"),
    date.end = c("2020-02-13",NA,NA),
    title = c("Conference", "Lunch", "Walk"),
    start = c("12:00:00", NA, "08:00:00"),
    time.end = c("13:00:00", NA, "17:30:00"),
    place = c("Home", "Work", NA),
    note = c("Hi there","Remember to come", ""),
    link = c("https://icalendar.org","https://agdamsbo.github.io/stRoke/", "")
  )
  
  expect_s3_class(
    write_ical(
      df,
      date.end = "date.end",
      time.end = "time.end",
      place = "place",
      descr = "note",
      link = "link"
    ),
    "ical"
  )
})

test_that("write_ical() returns error", {
  df <- data.frame(
    date = c("2020-02-10", "2020-02-11"),
    title = c("Conference", "Lunch"),
    start = c("12:00:00", NA),
    end = c("13:00:00", NA),
    note = c("Hi there","Remember to come"),
    link = c("https://icalendar.org","https://agdamsbo.github.io/stRoke/")
  )
  expect_error(write_ical(df, date = "wrong"))
  expect_error(write_ical(df, place = "wrong"))
  expect_error(write_ical(df, title = "wrong"))
  expect_error(write_ical(df, time.start = "wrong"))
  expect_error(write_ical(df, time.end = "wrong"))
})

test_that("write_ical() returns error", {
  df <- data.frame(
    date = c("2020-02-10", "2020-02-11"),
    date.end = c(NA,"2020-02-13"),
    title = c("Conference", "Lunch"),
    start = c("12:00:00", NA),
    end = c("13:00:00", NA),
    note = c("Hi there","Remember to come"),
    link = c("https://icalendar.org","https://agdamsbo.github.io/stRoke/")
  )
  expect_error(write_ical(df,
                          date.end = "date.end"))
})

test_that("write_ical() returns error", {
  df <- data.frame(
    date = c("2020-02-10", "2020-02-11"),
    date.end = c("2020-02-13",NA),
    title = c(NA, "Lunch"),
    start = c("12:00:00", NA),
    end = c("13:00:00", NA),
    note = c("Hi there","Remember to come"),
    link = c("https://icalendar.org","https://agdamsbo.github.io/stRoke/")
  )
  expect_error(write_ical(df,
                          date.end = "date.end"))
})