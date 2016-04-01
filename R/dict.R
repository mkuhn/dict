
dict <- function(init_keys = NULL, init_values = NULL) {

  if (xor(is.null(init_keys), is.null(init_values))) stop("Need both keys and values!")

  thisEnv <- environment()
  thisEnv$values <- list()
  thisEnv$idx_dict <- new(IdxDict)
  thisEnv$get_or_zero <- thisEnv$idx_dict$get_or_zero
  thisEnv$get_or_set_idx <- thisEnv$idx_dict$get_or_set_idx
  thisEnv$keys <- thisEnv$idx_dict$keys
  thisEnv$items <- thisEnv$idx_dict$items

  if (!is.null(init_keys)) {
    thisEnv$values <- as.list(init_values)
    for (i in seq_along(init_keys)) {
      thisEnv$get_or_set_idx(init_keys[[i]])
    }
  }

  thisEnv$init_keys <- NULL
  thisEnv$init_values <- NULL

  l <- list(

    thisEnv = thisEnv,

    set = function(key, value) {
      if (is.null(value)) stop("Cannot store NULL in dict!")
      idx <- thisEnv$get_or_set_idx(key)
      thisEnv$values[[idx]] <- value
    },

    get = function(key, default_value=NULL) {
      idx <- thisEnv$get_or_zero(key)
      if (idx == 0) return(default_value)
      thisEnv$values[[idx]]
    },

    get_or_stop = function(key) {
      idx <- thisEnv$get_or_zero(key)
      if (idx == 0) stop(
        paste("Key error:", capture.output(print(key)))
      )
      thisEnv$values[[idx]]
    },

    keys = function() {
      thisEnv$keys()
    },

    values = function() {
      thisEnv$values
    },

    items = function() {
      lapply( thisEnv$items(), function(l) list(key=l$key, value = thisEnv$values[[l$value]] ))
    },

    length = function() {
      length(thisEnv$values)
    }
  )

  class(l) <- "dict"

  l
}

`[[.dict` <- function(d, key) d$get_or_stop(key)
`[[<-.dict` <- function(d, key, value) {
  d$set(key, value)
  d
}

numvecdict <- function() {
  d <- new(NumVecDict)
  assign('get',
         function(key, default_value=numeric()) d$get_with_default(key, default_value),
         envir = d
  )
  d

}