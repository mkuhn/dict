\name{dict}
\alias{dict}

\title{
Create a dictionary
}
\description{
Returns a dictionary in which numbers and string (including vectors of each) can be used as keys,
and any R object can be used as value.
}
\usage{
dict()
}

\value{
A dictionary object that can be used to assign and extra values using either the \code{[[ ]]} operator
or member functions accessed via \code{$}.
}

\seealso{
\code{\link{numvecdict}}
}
\examples{
# create new dictionary
d <- dict()

# assign an item whose key is c(2:3)
d[[ c(2,3) ]] <- "Test"

# retrieve the element
d[[ c(2,3) ]]

# get will return NULL if the key is not in the dictionary
# (but you can also set the second argument to specify the default value)
d$get("?")
}
