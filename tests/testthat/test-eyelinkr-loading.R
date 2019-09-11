test_path <- "../test-data/test-file-SR1000.asc"

test_that("loading gaze works", {
  expect_silent(gaze <- read_gaze(test_path))
  expect_equal(278080, nrow(gaze))
  column_classes <- c("integer","numeric","numeric","numeric","character")
  expect_true(all(sapply(gaze, class) == column_classes))
})

test_that("loading fixations works", {
  expect_silent(fix <- read_fixations(test_path))
  expect_equal(797, nrow(fix))
  column_classes <- c("integer", "integer", "integer", "numeric",
                      "numeric", "integer", "integer")
  expect_true(all(sapply(fix, class) == column_classes))
  required_columns <- c("start", "duration", "x", "y")
  expect_true(all(required_columns %in% colnames(fix)))
})

test_that("loading events works", {
  expect_silent(events <- read_events(test_path))
  expect_equal(4990, nrow(events))
  column_classes <- c("integer", "factor", "factor")
  expect_true(all(sapply(events, class) == column_classes))
})

test_that("reading of logging rate works", {
  text <- readLines(test_path, n=250)
  expect_silent(freq <- parse_logging_rate(text))
  expect_true(freq == 1000)
})

test_that("loading completely works", {
  expect_silent(ls <- load_asc_file(test_path))
})

