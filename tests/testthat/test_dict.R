context("Dict test")

test_that("Numbers as keys", {
  d <- dict()
  expect_equal( d$get(1), as.numeric(NA) )
  d$set(1, 23)
  d$set(-0.5, "test")
  expect_equal( d$get(1), 23 )
  expect_equal( d$get(-0.5), "test" )
})

test_that("Vectors of numbers as keys", {
  d <- dict()
  expect_equal( d$get(c(1,2)), as.numeric(NA) )
  d$set(1, 23)
  d$set(c(-0.5, 0.5), "test")
  expect_equal( d$get(1), 23 )
  expect_equal( d$get(c(-0.5, 0.5)), "test" )
})
