
dict <- function() {
  d <- new(Dict)
  assign('get',
         function(key, default_value=NA) d$get_with_default(key, default_value),
         envir = d
  )
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