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

test_that("Append other numvecdict", {
  a <- numvecdict()
  a[[1]] <- c(1,2)
  b <- numvecdict()
  b[[1]] <- c(3,4)
  b[["A"]] <- c(1,2)
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
  d[[1]] <- 1
  d[[c(1,2)]] <- "2"
  d[["A"]] <- c(3, 0)
  d[[ c("A", "B") ]] <- c("4", "X")
  # order of returned items is arbitrarily specified in C++ code
  expect_equal( d$keys(), list(1, c(1,2), "A", c("A", "B")) )
  expect_equal( d$values(), list(1, "2", c(3, 0), c("4", "X")) )

  expect_equal( d$items(), list(
    list(key = 1, value = 1),
    list(key=c(1,2), value="2"),
    list(key="A", value=c(3, 0)),
    list(key=c("A", "B"), value=c("4", "X"))
  ))

  d <- numvecdict()
  d[[1]] <- 1
  d[[c(1,2)]] <- 2
  d[["A"]] <- c(3, 0)
  d[[ c("A", "B") ]] <- c(4, 5)
  # order of returned items is arbitrarily specified in C++ code
  expect_equal( d$keys(), list(1, c(1,2), "A", c("A", "B")) )
  expect_equal( d$values(), list(1, 2, c(3, 0), c(4, 5)) )

  expect_equal( d$items(), list(
    list(key = 1, value = 1),
    list(key=c(1,2), value=2),
    list(key="A", value=c(3, 0)),
    list(key=c("A", "B"), value=c(4, 5))
  ))

  dm <- d$means()

  expect_equal( dm$items(), list(
    list(key = 1, value = 1),
    list(key=c(1,2), value=2),
    list(key="A", value=c(1.5)),
    list(key=c("A", "B"), value=c(4.5))
  ))
})

