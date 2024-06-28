# revisions
# Jun 2023, allow bigz and bigq as well 
# Jun 2024 fixed bug that fouled up converting binary decimal parts. 

#TODO
# DUNallow mpfr inputs. use formatDec(x) to convert to char string

# Convert ONLY the fractional part, throwing away any integer part. Live with it. 
 fracB2B <- function( x,  inBase = 10, outBase = 16, maxdig = 0) {
# input validation
# note- formatDec currently fails for super-high precbits values. 
# I could hack it here to check precbits(x) and reduces to 1000 to make it work.
# (check for max(getPrec(x)))
# formatDec sticks in an "Ncharacter" class which confuses stuff later. Kill it. 
#  browser()
# I think this can put a space char that I never want, so ,clean that out too
if(is(x[[1]], 'mpfr') ) {
	x <- as.character(formatDec(x))
	x <- gsub(' ','',x)
}
 
# new
if(is(x[[1]],'bigz')) return(0)  #no fraction
if(is(x[[1]], 'bigq')) {
	x <- as.character(Rmpfr::formatDec(Rmpfr::.bigq2mpfr(x)) )
	x <- gsub(' ','',x)
}
# clean this up
#if (!is(x[[1]], 'character') && !is(x[[1]],'numeric')) stop('Illegal input class. Use character strings, mpfrs, or numerics.')
if (!class(x[[1]]) %in% c('mpfr','numeric','character')) stop(c("Unknown input class" , class(x[[1]]) ) )
if(!(2<=inBase && 36 >= inBase && 2<=outBase && 36>= outBase)){
	stop('Both bases must be in range 2:36')
	}
#casting to char is exact, up to 15 +/-  digits. 
if(is(x[[1]],'numeric')) x <- as.character(x)
# initialize stuff
theans <-list()
outz <- as.bigz(outBase)
#  pick the decimal point character based on Sys.localeconv()[1]	
thedec <- Sys.localeconv()[1]
negval = rep('',times = length(x) )
# check for negs
for (jn in 1:length(x)) {
	gotneg <- grep('^-',x[[jn]])
	if (length(gotneg)) {
		negval[jn] = '-'
		x[[jn]] <- gsub('^-','',x[[jn]])
		}
}
# Note: base2base() call will take care of possible exponents in input char strings
# browser()
for(jx in 1:length(x)) 	{
#check for no fractional part
	if (!length(grep(paste0('[',thedec,']'),x[[jx]]) ) ) {
		theans[[jx]] <- '0'
		next
	}
# remove any integer part 
# browser()
	 x[[jx]] <- gsub(paste0('^.{0,}[',thedec,']'),'',x[[jx]] )
	tmpx <- base2base(x[[jx]], inBase, 10, classOut = 'character')[[1]]
#browser()
#check for the error string
	if(tmpx == '%no') {
		theans[[jx]] <- tmpx
		next
	}
# TODO: see if I want to remove lead zeros from x[[jx]] . probably not
	thepow <- nchar(x[[jx]]) # yes, watch the input size
# now convert  to a bigq
# since integer part was removed, just divide by "shift power" of input
	tmpfrac <- as.bigq( as.bigz(tmpx) , inBase^(as.bigz(thepow)) ) 
# inner loop to calc digits in outBase
#  put a limit on number of digits in case output is a repeating fraction
	if(maxdig == 0) {
		# generate a reasonable stopping length
		maxdig <- max(4,nchar(x[[jx]]) * ceiling(inBase/outBase))
	}
	loopcount = 0
	domore = TRUE
	theans[[jx]] <- as.character(NULL)
# browser()
	while(domore && loopcount < maxdig){		
		tmp <- tmpfrac * outz # the "new" integer part will be appended ..
		tmpint <- as.bigz(tmp)
		theans[[jx]] <- c(theans[[jx]], as.character(tmpint) )
		tmpfrac <- tmp - tmpint 
		if (tmpfrac == 0) domore = FALSE
		loopcount = loopcount + 1
	}
# Convert each term separately to ensure, e.g., 14 base 10 turns into E base 16 
# seems to be a bug here - 0.2 decimal input is giving way too many zeros
# to the output binary representation.  All other bases seem ok,
# and converting FROM base2 seems ok .
# this is because base2base forces base2 out, when character, to 4N slots
# I need to figure out how to keep 'wanted' lead zeros ? 
# Since binary is a special case, and its REAL SIMPLE, just skip the call!  
# new:
	if(outBase == 2) {
		theans[[jx]] <- paste0(theans[[jx]], collapse = '')
	} else {
		termtmp <- rep('0',times=length(theans[[jx]]))	
		for (jz in 1:length(theans[[jx]])){
			termtmp[jz] <- unlist(base2base(theans[[jx]][[jz]], 10, outBase, classOut ='character'))
		}
	theans[[jx]] <- paste0(termtmp,collapse='') 
	}
# collapse that list - and put decimal back in, ISO std
	theans[[jx]] <- paste0(c('0', thedec, unlist(theans[[jx]]), collapse='') )
	theans[[jx]] <- paste0(c(negval[[jx]],theans[[jx]]),collapse='')
} #end of jx loop

return(unlist(theans) ) 
}



