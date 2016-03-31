# dict
[![Build Status](https://travis-ci.org/mkuhn/dict.svg?branch=master)](https://travis-ci.org/mkuhn/dict)
[![codecov.io](https://codecov.io/github/mkuhn/dict/coverage.svg?branch=master)](https://codecov.io/github/mkuhn/dict?branch=master)

`dict` provides dictionaries with arbitrary keys and values for R. Other solutions in R, such as named lists, the [`hash` package](https://cran.r-project.org/web/packages/hash/index.html), or environments only let you use strings and, partially, numbers as keys. However, it is not possible to use vectors like `c(1,2,3)` or `c("A", "B")` as keys. This package provides efficient implementations of standard [Python-style dictionaries](https://docs.python.org/3/library/stdtypes.html#mapping-types-dict) and a [`defaultdict`](https://docs.python.org/3/library/collections.html#defaultdict-objects) for numeric vectors.

Under the hood, it creates a separate C++ `unordered_map` for the following types:

- `numeric` (both single values and vectors)
- `character` (both single values and vectors)

Please refer to the [introduction for an overview of the functions](vignettes/introduction.Rmd), or see below for a short usage example.

### Installation

To install:

```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("mkuhn/dict")
```

This has been tested on Mac OS X (using clang) and Ubuntu (using gcc 4.8). Older versions of gcc might not work, due to incomplete support of C++11. On Windows, you will need the [new gcc 4.93 toolchain](https://github.com/rwinlib/r-base/wiki/Testing-Packages-with-Experimental-R-Devel-Build-for-Windows) or wait until R 3.3.0, to be released mid-April 2016.

### Usage

Usage of `dict`:

```r
library(dict)

d <- dict()

d[[1]] <- 42
d[[c(2, 3)]] <- "Hello!"
d[["foo"]] <- "bar"
d[[1]]
d[[c(2, 3)]]
d$get("not here", "default")

d$keys()
d$values()
d$items()

# [[ ]] gives an error for unknown keys
d[["?"]]

```

Usage of `numvecdict`:

```r
library(dict)

d <- numvecdict()

# initialize with vector
d[[c(2, 3)]] <- c(1,2,3)
d$append_number(c(2, 3), 4)
d[[c(2, 3)]]

# if the key doesn't exist: create new vector
d$append_number(1, 23)
d[[1]]

```




