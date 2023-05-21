test_that("blocks 1-dimensional objects", {
  test <- c(1,2,3)
  expect_error(netupdate(test), "Input must be a graph object, not a vector")
})

test_that("data types are correct", {
  expect_equal()
})
