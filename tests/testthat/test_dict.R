context("Dict test")

test_that("Numbers as keys", {
  d <- dict()
  expect_true( is.null(d$get(1)) )
  expect_true( is.na(d$get(1, NA)) )
  d$set(1, 23)
  d$set(-0.5, "test")
  expect_equal( d$get(1), 23 )
  expect_equal( d$get(-0.5), "test" )
  d$set(1:10, ":-)")
  expect_equal( d$get(1:10), ":-)" )
})

test_that("Length", {
  d <- dict()
  d[[1]] <- 1
  d[[2]] <- "A"
  d[[c("A", "B")]] <- list()
  expect_equal( d$length(), 3 )

  d <- numvecdict()
  d[[1]] <- 1
  d[[2]] <- 2
  d$append_number(1, 23)
  d$append_number("A", 42)
  expect_equal( d$length(), 3 )

})

test_that("Vectors of numbers as keys", {
  d <- dict()
  expect_true( is.null(d$get(c(1,2))) )
  d$set(1, 23)
  d$set(c(-0.5, 0.5), "test")
  expect_equal( d$get(1), 23 )
  expect_equal( d$get(c(-0.5, 0.5)), "test" )
})

test_that("Strings as keys", {
  d <- dict()
  expect_true( is.null(d$get("1")) )
  d$set("1", 23)
  d$set("A", "test")
  expect_equal( d$get("1"), 23 )
  expect_equal( d$get("A"), "test" )
})

test_that("Vectors of strings as keys", {
  d <- dict()
  expect_true( is.null(d$get(c("1","2"))) )
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
  expect_true( is.null(d$get("not here")) )
  expect_equal( d$get("not here", "default"), "default" )
  expect_error( d[["not here"]] )

  d <- numvecdict()
  expect_true( length(d[[1]]) == 0 )
})

test_that("Append numbers: numbers as keys", {
  d <- numvecdict()
  expect_equal( d$get(1), numeric() )
  d$append_number(1, 2)
  expect_equal( d$get(1), c(2) )
  d$append_number(1, 3)
  expect_equal( d$get(1), c(2,3) )
})


test_that("Append numbers: numeric vectors as keys", {
  d <- numvecdict()
  expect_equal( d$get(c(1,2)), numeric() )
  d$append_number(c(1,2), 2)
  expect_equal( d$get(c(1,2)), c(2) )
  d$append_number(c(1,2), 3)
  expect_equal( d$get(c(1,2)), c(2,3) )
  expect_error( d$append_number(c(1,2), "?!?!") )
})


test_that("Append numbers: strings as keys", {
  d <- numvecdict()
  expect_equal( d$get("A"), numeric() )
  d$append_number("A", 2)
  expect_equal( d$get("A"), c(2) )
  d$append_number("A", 3)
  expect_equal( d$get("A"), c(2,3) )
})


test_that("Append numbers: string vectors as keys", {
  d <- numvecdict()
  expect_equal( d$get(c("A", "B")), numeric() )
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

test_that("Append other numvecdict", {
  a <- numvecdict()
  a[[1]] <- c(1,2)

  b <- numvecdict()
  b[[1]] <- c(3,4)
  b[["A"]] <- c(1,2)
  b[[c("A", "B")]] <- 1
  b[[c(1,2)]] <- 1

  a$append_items(b)

  # can modify b without affecting merged a
  b$append_number(1, 23)
  b$append_number("A", 5)

  expect_equal( a[[1]], c(1,2,3,4) )
  expect_equal( a[["A"]], c(1,2) )
})

test_that("Unsupported keys", {
  d <- numvecdict()
  l <- list(1,2,3)
  expect_error( d[[l]] )
  expect_error( d[[l]] <- 1 )
  expect_error( d$append_number(l, 1) )
})

test_that("Keys/values", {
  d <- dict()
  d$set(1, 1)
  d$set(c(1,2), "2")
  d$set("A", c(3, 0))
  d$set(c("A", "B"), c("4", "X"))
  # order of returned items is arbitrarily specified in C++ code
  expect_equal( d$keys(), list(1, c(1,2), "A", c("A", "B")) )
  expect_equal( d$values(), list(1, "2", c(3, 0), c("4", "X")) )

  expect_equal( d$items(), list(
    list(key = 1, value = 1),
    list(key = c(1,2), value = "2"),
    list(key = "A", value = c(3, 0)),
    list(key = c("A", "B"), value = c("4", "X"))
  ))

  getDict <- function() {
    d <- numvecdict()
    d[[1]] <- 1
    d[[c(1,2)]] <- 2
    d[["A"]] <- c(3, 0)
    d[[ c("A", "B") ]] <- c(4, 5, 9)
    d
  }

  d <- getDict()

  # order of returned items is arbitrarily specified in C++ code
  expect_equal( d$keys(), list(1, c(1,2), "A", c("A", "B")) )
  expect_equal( d$values(), list(1, 2, c(3, 0), c(4, 5, 9)) )

  expect_equal( d$items(), list(
    list(key = 1, value = 1),
    list(key = c(1,2), value = 2),
    list(key = "A", value = c(3, 0)),
    list(key = c("A", "B"), value = c(4, 5, 9))
  ))

  expect_equal( d$each_mean()$items(), list(
    list(key = 1, value = 1),
    list(key = c(1,2), value = 2),
    list(key = "A", value = 1.5),
    list(key = c("A", "B"), value = 6)
  ))

  expect_equal( d$each_median()$items(), list(
    list(key = 1, value = 1),
    list(key = c(1,2), value = 2),
    list(key = "A", value = 1.5),
    list(key = c("A", "B"), value = 5)
  ))

  expect_equal( d$each_max()$items(), list(
    list(key = 1, value = 1),
    list(key = c(1,2), value = 2),
    list(key = "A", value = 3),
    list(key = c("A", "B"), value = 9)
  ))

  expect_equal( d$each_min()$items(), list(
    list(key = 1, value = 1),
    list(key = c(1,2), value = 2),
    list(key = "A", value = 0),
    list(key = c("A", "B"), value = 4)
  ))

  d2 <- getDict()
  d2$inplace_mean()
  expect_equal( d$each_mean()$items(), d2$items() )

  d2 <- getDict()
  d2$inplace_median()
  expect_equal( d$each_median()$items(), d2$items() )

  d2 <- getDict()
  d2$inplace_min()
  expect_equal( d$each_min()$items(), d2$items() )

  d2 <- getDict()
  d2$inplace_max()
  expect_equal( d$each_max()$items(), d2$items() )

})

