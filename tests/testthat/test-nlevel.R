context("test-nlevel.R")

test_that("detects level based on numeric order", {
  f <- factor(c("a", "b", "b", "c"))

  expect_equal(fct_nlevel(f, 1), c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(fct_nlevel(f, -1), c(FALSE, FALSE, FALSE, TRUE))
})

test_that("detects lowest level", {
  f <- factor(c("a", "b", "b", "c"))

  expect_equal(fct_lo(f), c(TRUE, FALSE, FALSE, FALSE))
})

test_that("detects highest level", {
  f <- factor(c("a", "b", "b", "c"))

  expect_equal(fct_hi(f), c(FALSE, FALSE, FALSE, TRUE))
})
