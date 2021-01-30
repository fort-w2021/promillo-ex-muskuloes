test_that("basic implementation is correct", {
  expect_equal(
    tell_me_how_drunk(
      age = 39,
      sex = "male",
      height = 190,
      weight = 87,
      drinking_time = as.POSIXct(c(
        "2016-10-03 17:15:00",
        "2016-10-03 22:55:00"
      )),
      drinks = c("massn" = 3, "schnaps" = 4)
    ),
    2.359,
    tolerance = 0.01,
    ignore_attr = TRUE
  )
  expect_equal(
    tell_me_how_drunk(
      age = 24,
      sex = "female",
      height = 160,
      weight = 54,
      drinking_time = as.POSIXct(c(
        "2016-10-03 14:00:00",
        "2016-10-03 21:00:00"
      )),
      drinks = list("hoibe" = 1, "schnaps" = 2)
    ),
    0.40,
    tolerance = 0.01,
    ignore_attr = TRUE
  )
  expect_equal(
    tell_me_how_drunk(
      age = 68,
      sex = "male",
      height = 169,
      weight = 84,
      drinking_time = as.POSIXct(c(
        "2016-10-03 08:10:00",
        "2016-10-03 08:15:00"
      )),
      drinks = c("schnaps" = 3)
    ),
    0.687,
    tolerance = 0.01,
    ignore_attr = TRUE
  )
  expect_equal(
    tell_me_how_drunk(
      age = 38,
      sex = "male",
      height = 190,
      weight = 134,
      drinking_time = as.POSIXct(c(
        "2016-10-03 18:00:00",
        "2016-10-03 22:00:00"
      )),
      drinks = c("hoibe" = 1)
    ),
    0,
    ignore_attr = TRUE
  )
})

# use input homogenization and match.arg for this:
test_that("interface for sex is implemented flexibly", {
  expect_equal(
    tell_me_how_drunk(
      age = 38,
      sex = "male",
      height = 190,
      weight = 134,
      drinking_time = as.POSIXct(c(
        "2016-10-03 18:00:00",
        "2016-10-03 22:00:00"
      )),
      drinks = c("hoibe" = 5)
    ),
    tell_me_how_drunk(
      age = 38,
      sex = "m",
      height = 190,
      weight = 134,
      drinking_time = as.POSIXct(c(
        "2016-10-03 18:00:00",
        "2016-10-03 22:00:00"
      )),
      drinks = c("hoibe" = 5)
    )
  )
  expect_equal(
    tell_me_how_drunk(
      age = 38,
      sex = "M",
      height = 190,
      weight = 134,
      drinking_time = as.POSIXct(c(
        "2016-10-03 18:00:00",
        "2016-10-03 22:00:00"
      )),
      drinks = c("hoibe" = 5)
    ),
    tell_me_how_drunk(
      age = 38,
      sex = "m",
      height = 190,
      weight = 134,
      drinking_time = as.POSIXct(c(
        "2016-10-03 18:00:00",
        "2016-10-03 22:00:00"
      )),
      drinks = c("hoibe" = 5)
    )
  )
})

test_that("interface for drinks is implemented flexibly", {
  expect_equal(
    tell_me_how_drunk(
      age = 38,
      sex = "M",
      height = 190,
      weight = 134,
      drinking_time = as.POSIXct(c(
        "2016-10-03 18:00:00",
        "2016-10-03 22:00:00"
      )),
      drinks = list(list("hoibe" = 1), "schnaps" = 2)
    ),
    tell_me_how_drunk(
      age = 38,
      sex = "m",
      height = 190,
      weight = 134,
      drinking_time = as.POSIXct(c(
        "2016-10-03 18:00:00",
        "2016-10-03 22:00:00"
      )),
      drinks = c("schnaps" = 2, "hoibe" = 1)
    )
  )
  # think about how to achieve this as simply as possible:
  expect_equal(
    tell_me_how_drunk(
      age = 38,
      sex = "M",
      height = 190,
      weight = 134,
      drinking_time = as.POSIXct(c(
        "2016-10-03 18:00:00",
        "2016-10-03 22:00:00"
      )),
      drinks = list("schnaps" = 1, "hoibe" = 1, "schnaps" = 2)
    ),
    tell_me_how_drunk(
      age = 38,
      sex = "m",
      height = 190,
      weight = 134,
      drinking_time = as.POSIXct(c(
        "2016-10-03 18:00:00",
        "2016-10-03 22:00:00"
      )),
      drinks = c("schnaps" = 3, "hoibe" = 1)
    )
  )
})

# anything under age < 16 not ok, hard liquor under age 18 not ok.
test_that("legal drinking age is checked", {
  expect_warning(
    tell_me_how_drunk(
      age = 14,
      sex = "female",
      height = 160,
      weight = 50,
      drinking_time = as.POSIXct(c(
        "2016-10-03 18:00:00",
        "2016-10-03 22:00:00"
      )),
      drinks = c("hoibe" = 7)
    ),
    regexp = "illegal"
  )
  expect_warning(
    tell_me_how_drunk(
      age = 17,
      sex = "female",
      height = 160,
      weight = 50,
      drinking_time = as.POSIXct(c(
        "2016-10-03 18:00:00",
        "2016-10-03 22:00:00"
      )),
      drinks = c("schnaps" = 7)
    ),
    regexp = "illegal"
  )
  expect_silent(
    tell_me_how_drunk(
      age = 17,
      sex = "female",
      height = 160,
      weight = 50,
      drinking_time = as.POSIXct(c(
        "2016-10-03 18:00:00",
        "2016-10-03 22:00:00"
      )),
      drinks = c("massn" = 2)
    )
  )
})
