
context('Testing leaf_apply')

test_that("leaf_apply works with zero-length list", {
  # Try a zero-length list
  foo <- leaf_apply(list(), function(x, name){}, class=c("foo"))
  expect_that(foo, is_a("list"))
})