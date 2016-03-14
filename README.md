# dict
[![Build Status](https://travis-ci.org/mkuhn/dict.svg?branch=master)](https://travis-ci.org/mkuhn/dict)
[![codecov.io](https://codecov.io/github/mkuhn/dict/coverage.svg?branch=master)](https://codecov.io/github/mkuhn/dict?branch=master)

`dict` provides dictionaries with arbitrary keys and values for R. Under the hood, it creates a separate C++ unordered_map for the following type:

- `numeric` (both single values and vectors)
- `character` (both single values and vectors)

### Installation

To install:

```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("mkuhn/dict")
```

This has been tested on Mac OS X (using clang) and Ubuntu (using gcc 4.8). Older versions of gcc might not work, due to incomplete support of C++11.

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




