context('preprocessing')
test_path <- "../test-data/test-file-SR1000.asc"
events <- read_events(test_path)

test_that("event preprocessing", {
  expect_silent(events2 <- remove_event_brackets(events))
  expect_equal(nchar(events2$type[1]), events$type[1] - 2)
  expect_silent(events_only_down <- remove_key_up(events))
  expect_gt(nrow(events), nrow(events_only_down))
  expect_silent(events_no_walking <- remove_walking_keys(events))
  expect_gt(nrow(events), nrow(events_no_walking))
})
