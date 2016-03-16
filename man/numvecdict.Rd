\name{numvecdict}
\alias{numvecdict}

\title{
Create a dictionary of numerical vectors.
}
\description{
Returns a dictionary in which numbers and string (including vectors of each) can be used as keys.
This dictionary can only store vectors of numbers.
}
\usage{
numvecdict()
}

\value{
A dictionary object that can be used to assign and add values using either \code{[[} operator
or member functions accessed via \code{$}.
}

\seealso{
\code{\link{dict}}
}
\examples{
# create dictionary
d <- numvecdict()

# assign new vector
d[[ c(2,3) ]] <- c(1:3)

# and add a number
d$append_number(c(2,3), 4)

# return values
d[[ c(2,3) ]]

# appending to a new key will create a new vector
d$append_number("new!", 42)
}
