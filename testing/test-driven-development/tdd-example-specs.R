test_that("result for NULL is NULL", {
  expect_equal(monotone_sum(NULL), NULL)
})

test_that("result for empty vector is empty vector", {
  expect_equal(monotone_sum(integer(0)), integer(0))
})

test_that("non-numeric data is an error", {
  expect_error(monotone_sum(c("content")))
})

test_that("result for vector containing only 0 is itself", {
  expect_equal(monotone_sum(c(0)), c(0))
})

test_that("result for unit vector is itself", {
  v <- c(1234)
  expect_equal(monotone_sum(v), v)
})

test_that("result for single strictly-increasing sequence is sum", {
  expect_equal(monotone_sum(c(1, 3, 5)), c(9))
})

test_that("result for two strictly-increasing sequences is double sum", {
  expect_equal(monotone_sum(c(1, 3, 1, 3)), c(4, 4))
})

test_that("sequences end when value doesn't strictly increase", {
  expect_equal(monotone_sum(c(2, 2)), c(2, 2))
})

test_that("long mixed sequence handled correctly", {
  expect_equal(monotone_sum(c(4, 4, 6, 8, 2, 3, 3)), c(4, 18, 5, 3))
})
