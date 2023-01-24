#' CPR check
#'
#' Checking validity of cpr number. Vectorised.
#' @param cpr cpr-numbers as ddmmyy"-."xxxx or ddmmyyxxxx. 
#' Also mixed formatting. Vector or data frame column.
#' @keywords cpr
#'
#' @return Logical vector of cpr validity
#' @export
#'
#' @examples 
#' fsd<-c("2310450637", "010190-2000", "010115-4000",
#' "300450-1030","010150-4021")
#' cpr_check("2310450637")
#' cpr_check(fsd)
#' all(cpr_check(fsd))
cpr_check <- function(cpr) {
  # Check validity of CPR number, format ddmmyy-xxxx
  # Build upon data from this document:
  # https://cpr.dk/media/12066/personnummeret-i-cpr.pdf
  ## OBS according to new description, not all valid CPR numbers
  ## apply to this modulus 11 rule.
  message(
    "OBS: as per 2007 not all valid CPR numbers apply to modulus 11 rule.
    \nSee the vignette 'Toolbox'"
  )
  
  str_length <- nchar(cpr)
  # Calculating length of each element in vector
  
  cpr_short <-
    paste0(substr(cpr, 1, 6), substr(cpr, str_length - 3, str_length))
  # Subsetting strings to first 6 and last 4 characters to short format cpr.
  
  cpr_matrix <-
    matrix(as.numeric(unlist(strsplit(cpr_short, ""))), nrow = 10)
  # Splitting all strings by each character to list,
  # unlisting and creating matrix. Default is by column.
  
  test_vector <- c(4, 3, 2, 7, 6, 5, 4, 3, 2, 1)
  # Multiplication vector from
  # https://cpr.dk/media/12066/personnummeret-i-cpr.pdf
  
  colSums(cpr_matrix * test_vector) %% 11 == 0
  # Testing if modulus 11 == 0 of sums of matrix * multiplication vector.
}

#' Extracting date of birth from CPR
#'
#' For easy calculation. Does not handle cprs with letters (interim cpr)
#' @param cpr cpr-numbers as ddmmyy"-."xxxx or ddmmyyxxxx. 
#' Also mixed formatting. Vector or data frame column.
#' @param format character string of dob date format. Default is "%d-%m-%Y".
#' @keywords cpr
#' 
#' @return character vector
#' @export
#'
#' @examples
#' cpr_dob("231045-0637")
#' fsd<-c("2310450637", "010190-2000", "010115-4000",
#' "300450-1030","010150-4021")
#' cpr_dob(fsd)
cpr_dob <- function(cpr, format = "%d-%m-%Y") {
  ## Input as cpr-numbers in format ddmmyy-xxxx
  ## Build upon data from this document:
  ## https://cpr.dk/media/12066/personnummeret-i-cpr.pdf
  
  # Checks format and length
  check_form <- (nchar(cpr) == 10 & grepl("-", cpr)) |
    (nchar(cpr) == 11 & !grepl("[^A-Za-z0-9]", substr(cpr, 7, 7))) |
    !nchar(cpr) %in% 10:11
  
  if (any(check_form)) {
    warning("CPR length should be in format ddmmyy-xxxx or ddmmyyxxxx. 
            Output will contain NAs")
  }
  
  str_length <- nchar(cpr)
  # Calculating length of each element in vector
  
  cpr_short_all <-
    paste0(substr(cpr, 1, 6), substr(cpr, str_length - 3, str_length))
  # Subsetting strings to first 6 and last 4 characters to short format cpr.
  
  # Checks if letters in other positions than 1:7 or 10
  check_lets <- grepl("\\D", paste0(substr(cpr_short_all, 1, 7),
                                    substr(cpr_short_all, 10, 10)))
  
  if (any(check_lets)) {
    warning(
      "Does only handle CPRs with letters in position 2 and 3 of 
      the last 4 positions. Output will contain NAs"
    )
  }
  
  checks_any <- check_form | check_lets
  
  non_na <- seq_along(cpr)[!checks_any]
  
  cpr_short <- cpr_short_all[!checks_any]
  
  dobs <- c()
  
  a00 <- c(0:99)
  a36 <- c(0:36)
  a57 <- c(0:57)
  
  b00 <- c(0:3)
  b36 <- c(4, 9)
  b57 <- c(5:8)
  
  year <- as.numeric(substr(cpr_short, 5, 6))
  
  ddmmyy <- as.Date(substr(cpr_short, 1, 6), format = "%d%m%y")
  
  for (i in seq_along(cpr_short)) {
    p56 <- year[i]
    
    p7 <- substr(cpr_short[i], 7, 7)
    
    birth <- ddmmyy[i]
    
    if (((p56 %in% a00) && (p7 %in% b00)))
    {
      dob <- as.Date(format(birth, format = "19%y%m%d"), format = "%Y%m%d")
    }
    else if (((p56 %in% a36) && (p7 %in% b36)))
    {
      dob <- as.Date(format(birth, format = "20%y%m%d"), format = "%Y%m%d")
    }
    else if ((!(p56 %in% a36) && (p7 %in% b36)))
    {
      dob <- as.Date(format(birth, format = "19%y%m%d"), format = "%Y%m%d")
    }
    else if (((p56 %in% a57) && (p7 %in% b57)))
    {
      dob <- as.Date(format(birth, format = "20%y%m%d"), format = "%Y%m%d")
    }
    else if ((!(p56 %in% a57) && (p7 %in% b57)))
    {
      dob <- as.Date(format(birth, format = "18%y%m%d"), format = "%Y%m%d")
    }
    dobs[i] <- dob
    
  }
  dobs <-
    format(as.Date(dobs, origin = "1970-01-01"), format = format)
  
  merge(
    data.frame(index = seq_along(cpr), dobs = NA),
    data.frame(index = non_na, dobs),
    by = "index",
    all = TRUE
  )[, 3]
}

#' Determine female sex from CPR
#'
#' Just checking if last number of a string is equal or not.
#' @param cpr Vector. cpr-numbers as ddmmyy"-."xxxx or ddmmyyxxxx. 
#' Also mixed formatting. Vector or data frame column.
#' @keywords cpr
#' 
#' @return Logical vector
#' @export
#' @examples
#' cpr_female(stRoke::cprs[,1])
cpr_female <- function(cpr) {
  if (!is.vector(cpr))
    stop("Input has to be vector")
  
  x <-
    nchar(as.character(cpr)) # Formats as character to avoid confusions
  
  as.integer(substr(cpr, start = x, stop = x)) %% 2 == 0
}
