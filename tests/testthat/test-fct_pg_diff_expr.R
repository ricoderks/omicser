test_that("check vline", {
  # do I get a list
  expect_type(
    vline(x = -1),
    type = "list"
  )

  # do I get an error when not a number is supplied to x
  expect_error(
    vline(x = "a"),
    regexp = "'x' is not numerical!"
  )

  # do I get an error if a number is supplied to color
  expect_error(
    vline(color = 1),
    regexp = "'color' needs te be a character!"
  )

  # do I get an error if a wrong color name is supplied to color
  expect_error(
    vline(color = "abc"),
    regexp = "The color supplied to 'color' is not recognized!"
  )

  # do I get an error if a wrong color name is supplied to color
  expect_error(
    vline(color = "#0000ZZ"),
    regexp = "HEX color code supplied to 'color' is not correct!"
  )
})

test_that("check hline", {
  # do I get a list
  expect_type(
    hline(y = 1),
    type = "list"
  )

  # do I get an error when not a number is supplied to y
  expect_error(
    hline(y = "a"),
    regeyp = "'y' is not numerical!"
  )

  # do I get an error if a number is supplied to color
  expect_error(
    hline(color = 1),
    regeyp = "'color' needs te be a character!"
  )

  # do I get an error if a wrong color name is supplied to color
  expect_error(
    hline(color = "abc"),
    regeyp = "The color supplied to 'color' is not recognized!"
  )

  # do I get an error if a wrong color name is supplied to color
  expect_error(
    hline(color = "#0000ZZ"),
    regeyp = "HEX color code supplied to 'color' is not correct!"
  )
})
