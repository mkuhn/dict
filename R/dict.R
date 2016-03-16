
dict <- function() {

  thisEnv <- environment()
  thisEnv$values <- list()
  thisEnv$idx_dict <- new(IdxDict)

  thisEnv$idx_dict

  l <- list(

    thisEnv = thisEnv,

    # .method( "[[", &Dict<int>::get_or_stop )
    # .method( "get_with_default", &Dict<int>::get_with_default )
    # .method( "[[<-", &Dict<int>::set )
    # .method( "set", &Dict<int>::set )
    # .method( "keys", &Dict<int>::keys )
    # .method( "values", &Dict<int>::values )
    # .method( "items", &Dict<int>::items )
    # .method( "length", &Dict<int>::length )

    set = function(key, value) {
      if (is.null(value)) stop("Cannot store NULL in dict!")
      idx <- thisEnv$idx_dict$get_or_set_idx(key)
      thisEnv$values[[idx]] <- value
    },

    get = function(key, default_value=NULL) {
      idx <- thisEnv$idx_dict$get_or_zero(key)
      if (idx == 0) return(default_value)
      thisEnv$values[[idx]]
    },

    get_or_stop = function(key) {
      idx <- thisEnv$idx_dict$get_or_zero(key)
      if (idx == 0) stop(
        paste("Key error:", capture.output(print(key)))
      )
      thisEnv$values[[idx]]
    },

    keys = function() {
      thisEnv$idx_dict$keys()
    },

    values = function() {
      thisEnv$values
    },

    items = function() {
      lapply( thisEnv$idx_dict$items(), function(l) list(key=l$key, value = thisEnv$values[[l$value]] ))
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
         function(key, default_value=NA) d$get_with_default(key, default_value),
         envir = d
  )
  d

}