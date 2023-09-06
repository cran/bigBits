# helper func to 'convert' strings with decimals and exponents

# this only gets called -- from base2base() -- if an 'e' was found AND input base was 10 
# input is expected to be a charstring
noExp <- function(x) {
# just in case...
	x <- tolower(x) # incase of "E" for exponent
# remove plus signs, but keep minus signs (base2base should have done this, but in case this func gets used externally)
	x <- gsub('[+]','', x)
	xsplit <- unlist(strsplit(x,''))
	finde <- grep('e',xsplit)
# extra safety check:
	if(!length(finde)) {
		return(x) # if input had decimal part, shame on user
	}	
#  pick the character based on Sys.localeconv()[1]	
	thedec <- Sys.localeconv()[1]		
	finddot <- grep(paste0('[',thedec,']'), xsplit)
	# how many zeros to add on. Adjust later when 'fixing' decimal point	
	xexp <- xsplit[(finde +1 ):length(xsplit)]
	xexpn <- as.numeric(paste0(xexp,collapse = '',sep =''))
#xmant includes decimal part		
	xmant <- xsplit[1:(finde-1)]
# now  remove the decimal cha if it's there
# first, for decimal components
	if(length(finddot)) {
		regit <- regexpr(paste0('[',thedec,']{1}[0-9]{1,}'), x)
# will return -1 if things are bad in certain ways
		if(  xexpn > 0){
			if (length(regit) && regit > 0) {		
			decdigs <- xsplit[(finddot+1):(finddot -1 + attr(regit, 'match.length') )]
#  remove decdigs in excess of 'xexp'  reduce exprep by the number of 
	# digits that are "saved" 
			deldigx <- xexpn - length(decdigs) 	
			if (deldigx < 0){
				#won't add any zeros but delete some digits to get integer
				decdigs <- decdigs[1:xexpn]
				xexpn <- 0
				# }
			}else{
		# deldigx is >= 0 , keep all digits
				xexpn <- deldigx 
			}	
	# now remove the decimal char and add zeros as needed
			xout <- xsplit[1:(finddot-1)]
			xout <- paste0(c(xout,decdigs),sep='',collapse='')
			xout <- paste0(c(xout, rep('0', times= xexpn)), sep='',collapse='')
			} 	
		} else {
	# xexpn is 0 or neg
	# xout starts here as the stuff to left of dot
				xout <- xsplit[1:(finddot-1)]
				newlen <- length(xout) + xexpn
				if (newlen > 0) {
					xout <- paste0(c(xout[1:newlen]),	sep='',collapse='')
				} else {
#	 normally base2base will have caught " x < 1" condition before calling noExp
					xout <- 0 
				}
		} #end of else tied to if xexpn >0 	
	#  else here means no dot	so just replace 'e' with zeros OR truncate x
    } else{	
		if (xexpn >=0){
			xout <- paste0(c(xmant,rep('0',times=xexpn)) , sep='',collapse='')	
		} else {
			negdel <- length(xmant) + xexpn
			if ( negdel > 0 ) {
				xout <- paste0(c(xmant[1:negdel]), sep='',collapse='')
				} else xout <- 0
		}
	}	# end of the no-dot else section			
	return(xout)
}