\name{bigAnd}
\alias{bigAnd}
\alias{bigOr}
\alias{bigXor}
\alias{bigNot}
\alias{bigShiftL}
\alias{bigShiftR}
\title{
Functions to perform binary operations on integers of arbitrary size, and of arbitrary base (up to 36). 
}
\description{
These functions extend the capabilities of the matching base \code{bitw*} functions (which are limited to 32-bit integers).  Not only can any integer be processed, at least up to the machine limits as determined with the \code{gmp} library, but the inputs and outputs can be in any base. Further, both unsigned (a minus sign indicates negative) and 2's complement base-2 values are allowed. 
}
\usage{
bigOr(x, y, inBase = 10, outBase = 10, inTwosComp = TRUE) 
bigAnd(x, y, inBase = 10, outBase = 10,  inTwosComp = TRUE) 
bigXor(x, y, inBase = 10, outBase = 10, inTwosComp = TRUE) 
bigNot(x, inBase = 10, outBase = 10,  binSize = 0, inTwosComp = TRUE, outTwosComp = TRUE)
bigShiftL(x, shift, inBase = 10, outBase = 10, binSize = 0, inTwosComp = TRUE) 
bigShiftR(x, shift,  inBase = 10, outBase = 10, inTwosComp = TRUE) 
}
\arguments{
  \item{x,y}{ The integers to be processed. These can be numeric, integer, \code{mpfr}, \code{bigz} , or character class. They must be of the same class.
  They can be any base from 2 through 36 as specified by \code{inBase}. 
  If these are character strings, formats such as "-37e+5" or "0x4e" (for hex data) are accepted. 
   See the Details section for the capabilities and limits on "translation" of inputs.  If the lengths of \code{x} and \code{y} differ, the shorter one will be silently recycled.  
}
  \item{inBase}{
Specify the designated base of the input(s) .  Default is 10.
}
  \item{outBase}{
Specify the designated base of the output(s) .  Default is 10.
}
\item{inTwosComp}{
When \code{inBase} is 2,  the input(s) is treated as being in 2's complement format when this is TRUE (the default). Otherwise the input(s) is treated as a  positive base-2 value unless a negative sign is present. .  If \code{inBase} is not equal to 2, this arg is ignored. 
}
\item{binSize}{
Specify the number of binary bits for the output calculation. If this is set to zero (the default), the minimum number is set to 4*N such that the current value of the input and output is containable.  But see the Details section for a discussion of 2s complement behavior. 
}
\item{outTwosComp}{
Whent \code{outBase} is 2, return the 2s complement version of the value(s). Default is TRUE; when False, return a  binary value(s) with a negative sign as necessary. 
}
\item{shift}{
The number of bits to shift the input by. Only positive values are allowed. \code{bigShiftL} and \code{bigShiftR} shift to the "left" and "right" respectively. 
}
}
\details{
The inputs, when not in base 10, are expected to follow the common encoding where the letters "a" through "z" correspond to the decimal values 10 through 35.  Values in bases greater than 10 must be character strings. 
If the input is base 16 ('hex'), the character string can begin with or without '0x'.
Inputs specified as base 2 through 10 can be provided in any of the numeric formats and the functions automagically interpret them correctly. For example, when  \code{x} is numeric  1101 and  \code{inBase} is 2 , the functions will interpret the input as 13 if \code{inTwosComp} is FALSE and as -3 if TRUE. 

Shifting to the right when 2's complement is in use can lead to unexpected results. \code{bigShiftR} assumes 32-bit binary 2scomp for compatibility with \code{\link[base]{bitwShiftR}}. But for an arbitrarily large binary 2's complement input, the output, for a shift of one, will move -1 (11111...) to 2^(N-1) -1 , where N is the number of bits including the sign bit. \code{bigShiftR} defaults to max(32, min_needed_for_magnitude_of_x) bits.
Similarly, \code{bigShiftL} by default provides sufficient bits to handle the shifted value. This is unlike \code{\link[base]{bitwShiftL}} which  returns the value of the 32 LSBs (in 2's complement form) if the shifted value exceeds 2^31-1 .  If \code{binSize} is not zero (the default), \code{bigShiftL} will truncate to the specified bit size (or 32, whichever is greater).

Note that, for compability with the base \code{bitw*} functions, the value is internally extended to (at least) 32 bits prior to bitwise operations. In particular, the value of the NOT function when 2s complement is in use depends on the specified size of the binary data. 
Remember that there will be precision errors if large numerics are entered, possibly leading to roundoff errors.  In general, it's safer to enter values in \code{bigz} format or as character strings. 

}
\value{

A list object with one value per entry, corresponding to the input value(s) of x (or y if y is the longer input). In most cases the entries are character strings.  However, if the input and the output are specified as base 10, then the output is converted to the class of the input.  
}

\author{
Author and Maintainer:Carl Witthoft \email{carl@witthoft.com}

}


\seealso{
\code{\link[base]{bitwAnd}} and other "bitw*" functions
}
