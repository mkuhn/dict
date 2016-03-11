context("Dict test")

test_that("Numbers as keys", {
  d <- dict()
  expect_equal( d$get(1), NA )
  d$set(1, 23)
  d$set(-0.5, "test")
  expect_equal( d$get(1), 23 )
  expect_equal( d$get(-0.5), "test" )
  d$set(1:10, ":-)")
  expect_equal( d$get(1:10), ":-)" )
})

test_that("Vectors of numbers as keys", {
  d <- dict()
  expect_equal( d$get(c(1,2)), NA )
  d$set(1, 23)
  d$set(c(-0.5, 0.5), "test")
  expect_equal( d$get(1), 23 )
  expect_equal( d$get(c(-0.5, 0.5)), "test" )
})

test_that("Strings as keys", {
  d <- dict()
  expect_equal( d$get("1"), NA )
  d$set("1", 23)
  d$set("A", "test")
  expect_equal( d$get("1"), 23 )
  expect_equal( d$get("A"), "test" )
})

test_that("Vectors of strings as keys", {
  d <- dict()
  expect_equal( d$get(c("1","2")), NA )
  d$set("1", 23)
  d$set(c("A", "B"), "test")
  expect_equal( d$get("1"), 23 )
  expect_equal( d$get(c("A", "B")), "test" )
})

test_that("Mixed keys", {
  d <- dict()
  d$set(1, 23)
  d$set("1", 42)
  d$set(c("A", "B"), "test")
  expect_equal( d$get(1), 23 )
  expect_equal( d$get("1"), 42 )
  expect_equal( d$get(c("A", "B")), "test" )
})

test_that("[[ ]] syntax", {
  d <- dict()
  d[[1]] <-  23
  d[["1"]] <- 42
  d[[ c("A", "B") ]] <- "test"
  expect_equal( d[[1]], 23 )
  expect_equal( d[["1"]], 42 )
  expect_equal( d[[c("A", "B")]], "test" )
  expect_equal( d$get("not here"), NA )
  expect_equal( d$get("not here", "default"), "default" )
  expect_error( d[["not here"]] )
})

test_that("Append numbers: numbers as keys", {
  d <- numvecdict()
  expect_equal( d$get(1), as.numeric(NA) )
  d$append_number(1, 2)
  expect_equal( d$get(1), c(2) )
  d$append_number(1, 3)
  expect_equal( d$get(1), c(2,3) )
})


test_that("Append numbers: numeric vectors as keys", {
  d <- numvecdict()
  expect_equal( d$get(c(1,2)), as.numeric(NA) )
  d$append_number(c(1,2), 2)
  expect_equal( d$get(c(1,2)), c(2) )
  d$append_number(c(1,2), 3)
  expect_equal( d$get(c(1,2)), c(2,3) )
  expect_error( d$append_number(c(1,2), "?!?!") )
})


test_that("Append numbers: strings as keys", {
  d <- numvecdict()
  expect_equal( d$get("A"), as.numeric(NA) )
  d$append_number("A", 2)
  expect_equal( d$get("A"), c(2) )
  d$append_number("A", 3)
  expect_equal( d$get("A"), c(2,3) )
})


test_that("Append numbers: string vectors as keys", {
  d <- numvecdict()
  expect_equal( d$get(c("A", "B")), as.numeric(NA) )
  d$append_number(c("A", "B"), 2)
  expect_equal( d$get(c("A", "B")), c(2) )
  d$append_number(c("A", "B"), 3)
  expect_equal( d$get(c("A", "B")), c(2,3) )
})

test_that("Directly set vector", {
  d <- numvecdict()
  d$set(1, 2)
  expect_equal( d$get(1), c(2) )

  d[[ c("a", "b") ]] <- c(1, 2)
  expect_equal( d[[ c("a", "b") ]], c(1, 2) )

  d$set(1, c(2,3))
  expect_equal( d$get(1), c(2,3) )
  d$append_number(1, 4)
  expect_equal( d$get(1), c(2,3, 4) )
})


