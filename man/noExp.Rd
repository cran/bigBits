\name{noExp}
\alias{noExp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to convert character-string numbers in exponential notation to "pure" integers. 
}
\description{
This is a helper function for \code{\link[bigBits]{base2base}}. When an input value is a character string with exponential notation, e.g.,  "2.65e4" , this function rebuilds the character string as a pure integer, e.g., "26500" . Decimal portions are removed. 
}
\usage{
noExp(x)
}
\arguments{
  \item{x}{
An input character string, assumed only to contain numerals 0-9, "+,-,e,E" and the decimal separator character defined in the current locale.  
}
}
\details{
Both input and output must be base 10, as exponentiation in other bases is outside the current scope of this package. 
}
\value{
A character string representing the input as an integer written "longhand," i.e. no exponent. This string will contain only numerals and possibly a lead minus sign in the case of negative inputs.  
}

\author{
Author and Maintainer:Carl Witthoft \email{carl@witthoft.com}

}

\examples{

noExp('37e3')
noExp('-2.345e4')
# this returns zero
noExp('234e-5')
}

