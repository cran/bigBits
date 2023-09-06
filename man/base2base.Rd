\name{base2base}
\alias{base2base}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function which converts arbitrary-size integers from any base to any base. 
}
\description{
This function accepts inputs in  any base from 2 through 36 and produces the same value in any selected base from 2 through 36. This includes options for signed and 2s complement binary data. 
}
\usage{
base2base(x,frombase=10, tobase=2, classOut=c('bigz', 'mpfr',
 'numeric','character') , binSize = 0, inTwosComp = FALSE,
   outTwosComp = FALSE )
}
\arguments{
  \item{x}{
A value or vector or list of values which are to be converted. The class can generally be numeric, mpfr, bigz, or character strings. See Details for more information. 
}
  \item{frombase}{
The base of the input \code{x}. If the contents of \code{x} are incompatible with the specified base, a warning is issued and that value is skipped (i.e. if \code{x} has multiple values, \code{base2base} will keep running and process the other values). Default is 10
}
  \item{tobase}{
The desired base of the output. Default is 2.
}
  \item{classOut}{
Specify the class of the output. This only has meaning when \code{tobase} is 10; all other bases are returned as character strings. Warning: if the input is larger than the max integer size on your system and "numeric" is selected, there will be a roundoff/truncation error.  
}
  \item{binSize}{
This applicable only when the output is in base 2. Specifies how many binary digits are to be generated.  If this value is less than that necessary to contain the output value, the number of bits will be increased to a 4*N value. The default is zero, which allows the function to calculate the minimum 4*N bits required. 
}
  \item{inTwosComp}{
Only checked if \code{frombase} is 2.  If FALSE, (the default), the input is  positive unless a negative sign is present.  If TRUE, the input is handled as a 2s complement binary. 
}
  \item{outTwosComp}{
Only checked if \code{tobase} is 2.  If FALSE, (the default), the output, if negative, includes a "-" sign.  If TRUE,  a 2's complement binary.
}
}
\details{
In general, when submitting an input in other than base 10, it's safest to provide a character string(s). There is some automagical conversion that will take, e.g., a numeric  \code{364} with \code{frombase = 8} and treat as base 8 (thus decimal 244), but this is not guaranteed.  Further, keep in mind that numeric values with more than roughly 16 digits will likely run into floating-point precision errors. For base-10 inputs, use of \code{bigz} form is recommended. 
Inputs in hex format must be character strings.  This is because the command parser converts, e.g.,  0x3a, to the decimal value 58 prior to passing the value to the function body.  Since, as noted above, \code{base2base} will attempt to convert a numeric input into the value in the base specified,  \code{base2base(0x3a, inbase= 16, ...)} will in fact process the input as  58hex, i.e. 88 decimal. 

}
\value{
A list containing the converted value(s). Unless \code{tobase} is 10, each element is a character string.  When \code{tobase} is equal to 10, the output class is specified with the argument \code{classOut} .  
Note: if an input or output is incompatible with the specified input or output base, a dummy value "%no" if character, or "NA" if a number-like class,  is returned along with a warning message describing the error. 
}

\author{
Author and Maintainer:Carl Witthoft \email{carl@witthoft.com}

}

\seealso{
\code{\link[base]{strtoi}}
\code{\link[base]{as.hexmode}}

}
\examples{
(base2base(12.4e1,10,16))
(base2base(12.4e-2,10,16))
(base2base(101101,2,10))  # magic. it works!!!
(base2base('1111',2,2,inTwosComp=TRUE, outTwosComp=TRUE))
(base2base('0111',2,2,inTwosComp=TRUE, outTwosComp=TRUE))
(base2base('1111',2,2,inTwosComp=TRUE, outTwosComp=FALSE))
(base2base('0111',2,2,inTwosComp=TRUE, outTwosComp= FALSE))
(base2base(1e55,10,16)) #loses precision before even starting
(base2base('1e55',10,16)) #works
(base2base('1767707668033969' , 10, 36))
}

