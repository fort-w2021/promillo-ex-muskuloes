#' Tell me how drunk
#'
#' @param age person's in years
#' @param sex person's sex (male or female)
#' @param height person's height in cm
#' @param weight person's weight in Kg
#' @param drinking_time drinking time in hours
#' @param drinks drinks vector e.g., c("schnaps", "wein")
#' @return promille Per mille blood alcohol value
#' @export
#' @import checkmate
tell_me_how_drunk <- function(age, sex = c("male", "female"), height, weight,
                              drinking_time, drinks) {

  # make input homogeneous and check them
  sex <- tolower(sex)
  sex <- match.arg(sex)
  drinks <- unlist(drinks)
  checkmate::assert_choice(sex, c("male", "female"))
  checkmate::assert_number(age, lower = 0, finite = TRUE)
  checkmate::assert_number(height, lower = 0, finite = TRUE)
  checkmate::assert_number(weight, lower = 0, finite = TRUE)
  checkmate::assert_posixct(drinking_time, len = 2, sorted = TRUE)
  checkmate::assert_numeric(drinks, lower = 0, finite = TRUE)
  checkmate::assert_names(names(drinks), subset.of = c(
    "massn", "hoibe",
    "wein", "schnaps"
  ))
  drinks <- tapply(drinks, names(drinks), sum)
  if (age < 16) {
    warning("illegal drinking age")
  }
  if (age < 18 & "schnaps" %in% names(drinks)) {
    warning("illegal: hard liquor ain't allowed below 18 years old")
  }
  alcohol_drunk <- get_alcohol(drinks)
  bodywater <- get_bodywater(sex, age, height, weight)
  get_permille(alcohol_drunk, bodywater, drinking_time)
}

#' show me how drunk, plots per mille alcohol level at 5 mins interval
#'
#' @inheritParams tell_me_how_drunk
#' @export
#' @importFrom checkmate assert_posixct
#' @importFrom ggplot2 qplot
show_me_how_drunk <- function(age, sex, height,
                              weight, drinking_time, drinks) {
  checkmate::assert_posixct(drinking_time, len = 2, sorted = TRUE)
  x <- seq(drinking_time[[1]], drinking_time[[2]], by = "5 mins")
  y <- sapply(x, function(t) {
    tell_me_how_drunk(
      age, sex, height, weight,
      c(drinking_time[[1]], t), drinks
    )
  })
  ggplot2::qplot(x, y, xlab = "time", ylab = "per mille alcohol level")
}

# utilities --------------------------------------------------------------------

#' get alcohol consumption
#'
#' @param drinks drinks vector, e.g., c("schnaps", "wein")
#' @return total alcohol consumed
#' @import checkmate
get_alcohol <- function(drinks) {
  # homogenize inputs:
  drinks <- unlist(drinks)
  checkmate::assert_subset(names(drinks),
    choices = c("massn", "hoibe", "wein", "schnaps"),
    empty.ok = FALSE
  )
  checkmate::assert_numeric(drinks, lower = 0)

  volume <- c(
    "massn"    = 1000,
    "hoibe"   = 500,
    "wein"    = 200,
    "schnaps" = 40
  )
  alcohol_concentration <- c(
    "massn"    = 0.06,
    "hoibe"   = 0.06,
    "wein"    = 0.11,
    "schnaps" = 0.4
  )
  alcohol_density <- 0.8

  sum(drinks * volume[names(drinks)] *
    alcohol_concentration[names(drinks)] * alcohol_density)
}

#' get bodywater of a person
#'
#' @param sex person's sex (male or female)
#' @param age person's age in years
#' @param height person's height in cm
#' @param weight person's weigh in Kg
#' @return bodywater
#' @import checkmate
get_bodywater <- function(sex = c("male", "female"), age, height, weight) {
  sex <- tolower(sex)
  sex <- match.arg(sex)

  checkmate::assert_number(age, lower = 10, upper = 110)
  checkmate::assert_number(height, lower = 100, upper = 230)
  checkmate::assert_number(weight, lower = 40, upper = 300)

  coef <- if (sex == "male") {
    c(2.447, -0.09516, 0.1074, 0.3362)
  } else {
    c(0.203, -0.07, 0.1069, 0.2466)
  }
  t(coef) %*% c(1, age, height, weight)
}

#' get Permille
#'
#' @param alcohol_drunk amount of alcohol consumed
#' @param bodywater person's bodywater
#' @param drinking_time interval of time during which alcohol was consumed
#' @return permille blood alcohol value
#' @import checkmate
get_permille <- function(alcohol_drunk, bodywater, drinking_time) {
  checkmate::assert_posixct(drinking_time,
    any.missing = FALSE, sorted = TRUE, len = 2
  )

  alcohol_density <- 0.8
  blood_density <- 1.055
  permille <- alcohol_density * alcohol_drunk / (blood_density * bodywater)

  partylength <- difftime(drinking_time[2], drinking_time[1], units = "hours")
  sober_per_hour <- 0.15
  # sobering up starts only after one hour & you can't be more sober than 0:
  max(0, permille - (max(0, partylength - 1) * sober_per_hour))
}
