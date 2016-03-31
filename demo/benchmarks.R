# BENCHMARK FILE.
# - Demo file for comparing hash benchmarks.
#

message(
  "\nThe hash-benchmark compares named access and update speed of R's native " ,
  "\nvectors, lists, envrionments and hashes from the hash package . . . \n\n"
)

library(hash)
library(dict)
library(rbenchmark)
library(ggplot2)

# STEP 0. CREATE A SAMPLE SET OF KEYS AND VALUES.
#   size: the sample size
#   keys:   hash's keys
#   values: hash's values

size   <- 1e4   # The size of the refernece objects.
keys   <- as.character( sample(1:size) )  # A vector of
values <- as.character( rnorm( size ) )

  # Which is faster setting by mapply or doing a for-loop
  # Intialize parameters and prepare things.

  # ---------------------------------------------------------------------
# BENCHMARK 1:
#  Speed for assigning values to an environment
#  The following benchmark compares the speeds of setting key,values
#  on an environment by using mapply, a for-loop and lapply.
#
# CONCLUSION:
#   Use for-loop for setting it is at least 5% faster than the other
#   methods.
#
#  R-2.9.2:
#    Using the for-loop is about 15-20% faster than apply and 2-3x faster
#    than mapply
#
#  R-2.11.0: size 5e4
#    results from benchmark()
#      test replications elapsed relative user.self sys.self user.child sys.child
# 2 for_loop            5   7.026 1.000000     7.025        0          0         0
# 3   lapply            5   7.383 1.050811     7.384        0          0         0
# 1   mapply            5   7.750 1.103046     7.753        0          0         0
#
#
# ---------------------------------------------------------------------

message( "BENCHMARK 1:\n Testing the best method to assign many keys to a new environment\n" )

env.mapply <- new.env( hash = T , parent = emptyenv() )
env.lapply <- new.env( hash = T , parent = emptyenv() )
env.for    <- new.env( hash = T , parent = emptyenv() )
h          <- hash()

benchmark(
  for_loop = for( i in 1:length(keys) ) assign( keys[[i]], values[[i]], envir = env.for ) ,
  mapply   = mapply( assign, keys, values, MoreArgs = list( envir = env.mapply ) ) ,
  lapply   = lapply(
    ( 1:length(keys) ) ,
    FUN = function(i) assign( keys[[i]], values[[i]], envir = env.lapply )
  ) ,
  replications = 5 ,
  order = "relative"
)

cat( "\n\n" )


# ---------------------------------------------------------------------
# BENCHMARK 2: ACCESSING SINGLE VALUES
#   Compare times for accessing single elements of a list vs vector vs hash
#
# CONCLUSIONS:
#  - For number of items, looking up in a list is faster than looking
#    up in an environment.
#
# ---------------------------------------------------------------------

# Create a list using mapply, n.b much faster than for-loop
message( "BENCHMARK 2: Accessing a single value in a large hash structure\n" )

number.of.lookups <- 1e4
bm2 <- data.frame()


# LOOP OVER SIX ORDERS OF MAGNITUDES.
for( size in 2^(1:13) ) {

  cat( "\nComparing access time for object of size", size, "\n" )

  # CREATE NAMED-LIST:
  li<-mapply(
    function(k,v) {
      li<-list()
      li[[k]]<-v
      li
    } ,
    keys[1:size] ,
    values[1:size] ,
    USE.NAMES=F
  )


  # CREATE NAMED-HASH:
  ha <- hash( keys[1:size], values[1:size] )

  # CREATE DICT:
  di <- dict()
  for (i in 1:size) {
    di[[ keys[i] ]] <- values[i]
  }

  # CREATE A VECTOR
  ve <-  values[1:size]
  names(ve) <- keys[1:size]

  # CREATE KEYS TO LOOK UP:
  ke <- keys[ round(runif(max=size,min=1,n=number.of.lookups )) ]

  print(
    res <-  benchmark(
      `get/env` = for( k in ke ) get( k, ha@.Data ) ,
      `get/hash`   = for( k in ke ) get(k, ha) ,
      `hash`  = for( k in ke ) ha[[k]] ,
      `dict [[ ]]`  = for( k in ke ) di[[k]] ,
      `dict get_or_stop`  = for( k in ke ) di$get_or_stop(k) ,
      `list`  = for( k in ke ) li[[k]] ,
      `vector`= for( k in ke ) ve[[k]] ,
      replications = 10 ,
      order = "relative"
    )
  )
  res$size <- size
  bm2 <- rbind( bm2, res )

}

p <- ggplot(bm2, aes(size, elapsed, color=test)) + geom_point()
p <- p + ggtitle("Reading from data structures")
p <- p + ylab(paste0("Elapsed Time (per ", number.of.lookups ," reads)"))
p <- p + xlab("Object Size (n elements)")
p

cat("\n\n")


# ---------------------------------------------------------------------
# BENCHMARK 3: Slices [
#   Take slices of an object.  This is equivalent to [[.
#   We compare
#
# Notes:
#  - There is no native slice operation for env
#  -
#
# ---------------------------------------------------------------------

message( "BENCHMARK 3: Slices\n" )

slice.pct  <- 0.01
n.lookups  <- 100
bm3 <- data.frame()

for( size in 2^(0:13) ) {

  slice.size <- floor( size * slice.pct ) + 1
  cat( "\nComparing slice time for object of size", size, "with slice pct", slice.pct, "\n" )

  # CREATE NAMED-LIST:
  li<-mapply(
    function(k,v) {
      li<-list()
      li[[k]]<-v
      li
    } ,
    keys[1:size] ,
    values[1:size] ,
    USE.NAMES=F
  )


  # CREATE NAMED-HASH:
  ha <- hash( keys[1:size], values[1:size] )

  # CREATE A VECTOR
  ve <-  values[1:size]
  names(ve) <- keys[1:size]

  # CREATE KEYS TO LOOK UP:
  kes <- lapply( 1:n.lookups, function(x) keys[ round(runif(max=size,min=1,n=slice.size )) ] )
  # ke <- keys[ round(runif(max=size,min=1,n=slice.size )) ]

  print(
    res <-
      benchmark(
        `hash`   = for( ke in kes ) ha[ ke ] ,
        `list`   = for( ke in kes ) li[ ke ] ,
        `vector` = for( ke in kes ) ve[ ke ] ,
        `mget`   = for( ke in kes ) mget( ke, ha@.Data ) ,
        replications = 5 ,
        order = "relative"
      )
  )

  res$size <- size
  bm3 <- if( nrow(bm3)==0) res else rbind( bm3, res )

}


xyplot(
  elapsed ~ size, groups=test,
  data=bm3,
  type="b", pch=16:20, col=rainbow(5),
  lwd=2, main="Reading from data structures", cex=1.2, cex.title=4,
  auto.key=list(space = "right", points = FALSE, lines = FALSE, lwd=4, cex=1, col=rainbow(5)) ,
  scales=list( cex=2 ),
  ylab = "Elapsed Time ( per 1K Reads)" ,
  xlab = "Object Size ( n elements )"
)



cat( "BENCHMARK 3: [[ Single Element ]] <- Writes \n" )

n.writes  <- 100
bm4 <- data.frame()

for( size in 2^(0:13) ) {

  # CREATE NAMED-LIST:
  li<-mapply(
    function(k,v) {
      li<-list()
      li[[k]]<-v
      li
    } ,
    keys[1:size] ,
    values[1:size] ,
    USE.NAMES=F
  )


  # CREATE NAMED-HASH:
  ha <- hash( keys[1:size], values[1:size] )

  # CREATE ENV
  en <- new.env( hash=TRUE )
  for( i in 1:size ) assign( keys[[i]], values[[i]],  en )

  # CREATE A VECTOR
  ve <-  values[1:size]
  names(ve) <- keys[1:size]

  # CREATE KEYS TO LOOK UP:
  kes <- keys[ round(runif(n=n.writes,min=1,max=length(keys)  )) ]
  # ke <- keys[ round(runif(max=size,min=1,n=slice.size )) ]


  print(
    res <-
      benchmark(
        # `hash`   = for( ke in kes ) ha[[ ke ]] <- "a" ,
        #  `list`   = for( ke in kes ) li[[ ke ]] <- "a" ,
        `vector` = for( ke in kes ) ve[[ ke ]] <- "a" ,
        # `env/assign`   = for( ke in kes ) assign( ke, "a" , en ) ,
        replications = 5 ,
        order = "relative"
      )
  )

  res$size <- size
  bm4 <- if( nrow(bm4)==0) res else rbind( bm4, res )

}


xyplot(
  elapsed ~ size, groups=test,
  data=bm4,
  type="b", pch=16:20, col=rainbow(5),
  lwd=2, main="Writing 100 Values to data structure", cex=1.2, cex.title=4,
  auto.key=list(space = "right", points = FALSE, lines = FALSE, lwd=4, cex=1, col=rainbow(5)) ,
  scales=list( cex=2 ),
  ylab = "Elapsed Time ( per 100  Writes" ,
  xlab = "Object Size ( n elements )"
)

