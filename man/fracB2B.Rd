\name{fracB2B}
\alias{fracB2B}
\title{
Function which converts fractions (between 1 and 0) from any base to any other base. 
}
\description{
This function accepts inputs in any base from 2 through 36 and generates the fractional portion of the input values in any selected base from 2 through 36. 
}
\usage{
fracB2B( x, inBase = 10, outBase = 16, maxdig = 0)
}
\arguments{
 \item{x}{
A value or vector or list of values which are to be converted. The class can generally be numeric, mpfr,bigz,bigq, or character strings. Any integer part is removed, as only the fractional part is converted with this function.  See Details for more information. 
}
 \item{inBase}{
The base of the input \code{x}, in the range 2 through 36. If the contents of \code{x} are incompatible with the specified base, a warning is issued and that value is skipped (i.e. if \code{x} has multiple values, \code{base2base} will keep running and process the other values). Default is 10.
}
 \item{outBase}{
The desired base of the output, in the range 2 through 36. Default is 16.
}
 \item{maxdig}{
The maximum number of digits to return in each result. This avoids an infinite loop when a given decimal does not terminate in the output base. The default value of \code{0} causes the function to generate a "reasonable" estimate for the number of places needed to maintain precision. 
}
}
\details{
In general, it's safest to provide a character string(s). There is some automagical conversion that will take, e.g., a numeric \code{364} with \code{inBase = 8} and treat as base 8 (thus decimal 244), but this is not guaranteed. Further, keep in mind that numeric values with more than roughly 16 digits will likely run into floating-point precision errors. 
Inputs in any base greater than 10 must be character strings. This is because the command parser converts, e.g., 0x3a, to the decimal value 58 prior to passing the value to the function body. 
}
\value{
A vector containing the converted value(s) as strings. A negative sign is included for negative inputs. 
}
\author{
Author and Maintainer:Carl Witthoft \email{carl@witthoft.com}
}
\seealso{
\code{base2base}

}
\examples{
(fracB2B(12.43e2,10,16)) # no decimal part
(fracB2B(12.43e-2,10,16))
(fracB2B(101.101,2,10)) # magic. it works!!!
fracB2B('.357') # "0.5b64"
fracB2B('.357',maxdig = 10) #"0.5b645a1cac"
fracB2B(".5b64",16,10)
fracB2B(".5b645a1cac",16,10)
}

