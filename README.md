# dict
[![Build Status](https://travis-ci.org/mkuhn/dict.svg?branch=master)](https://travis-ci.org/mkuhn/dict)

`dict` provides dictionaries with arbitrary keys and values for R. Under the hood, it creates a separate C++ unordered_map for the following type:

- `numeric` (both single values and vectors)
- `character` (both single values and vectors)

### Installation

To install:

```r
if (!require("devtools")) install.packages("devtools")
devtools::install_github("mkuhn/dict")
```

### Usage

Usage of `dict`:

```r
library(dict)

d <- dict()

d$set(1, 42)
d$set(c(2, 3), "Hello!")
d$set("foo", "bar")
d$get(1)
d$get(c(2, 3))

```


Usage of `numvecdict`:

```r
library(dict)

d <- numvecdict()

# initialize with vector
d$set(c(2, 3), c(1,2,3))
d$append_number( c(2, 3), 4)
d$get(c(2, 3) )

# if the key doesn't exist: create new vector
d$append_number(1, 23)
d$get(1)

```




