\name{buildBinaries}
\alias{buildBinaries}
\title{
Function to convert values to binary form
}
\description{
This function is intended primarily for internal use by the \code{big*} Boolean functions. Its job is to take an input in any base, in almost any class (numeric, character, etc) and generate the binary form of the same value. 
}
\usage{
buildBinaries(x, y = NULL, inBase, inTwosComp = FALSE, binSize = 0)
}
\arguments{
  \item{x,y}{
The values to be converted.  Typically these are the \code{x,y} values provided to one of the Boolean functions in this package. If only \code{x} is input, \code{y} defaults to \code{NULL}
}
  \item{inBase}{
The base (2 thru 36) of the input values.
}
\item{inTwosComp}{
When the input \code{inBase} is 2, this specifies whether the input is positive unless a negative sign is present, or 2's complement format . }
  \item{binSize}{
The minimum number of bits to use for the output binary data. If insufficient for the size of the input(s), this will be increased to the next 4*N size. When there are two inputs, both outputs are set to the same number of bits. ~
}

}

\value{
  \item{xbin }{A vector of numeric ones and zeroes representing the binary form of \code{x}
  }
  \item{ybin }{A vector of numeric ones and zeroes representing the binary form of \code{y}. If the input is NULL,  a single value of \code{0} is returned. }
}

\author{
Author and Maintainer:Carl Witthoft \email{carl@witthoft.com}
}

\examples{

buildBinaries(73,-73,inBase=10)

}

