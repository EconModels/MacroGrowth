context('Testing rowsSameOrdered')

test_that("rowsSameOrdered() is correct", {
  
  # This one should work.
  df1 <- data.frame(c(1, 1, 1), c(2, 2, 2), c(3, 4, 5))
  expect_true(rowsSameOrdered(df1))
  
  # Deal with one row (the first) that has no order.
  df2 <- data.frame(c(1, 1, 1), c(1, 2, 2), c(1, 3, 3))
  expect_true(rowsSameOrdered(df2))
  
  # None of the rows has an ordering. So, we can't tell. Expect false.
  df3 <- data.frame(c(1, 1, 1), c(1, 1, 1), c(1, 1, 1))
  expect_false(rowsSameOrdered(df3))

  # This one should fail. Rows are not in the same order.
  df4 <- data.frame(c(10, 1, 2, 3), c(10, 3, 2, 1))
  expect_false(rowsSameOrdered(df4))
})
