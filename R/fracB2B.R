# Convert ONLY the fractional part, throwing away any integer part. Live with it. 
 fracB2B <- function( x,  inBase = 10, outBase = 16, maxdig = 0) {
# input validation
if (!is(x[[1]], 'character') && !is(x[[1]],'numeric')) stop('Illegal input class. Use character strings or numerics.')
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
	 x[[jx]] <- gsub(paste0('^.{0,}[',thedec,']'),'',x[[jx]] )
	tmpx <- base2base(x[[jx]], inBase, 10, classOut = 'character')[[1]]
#browser()
#check for the error string
	if(tmpx == '%no') {
		theans[[jx]] <- tmpx
		next
	}
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
	while(domore && loopcount < maxdig){		
		tmp <- tmpfrac * outz # the "new" integer part will be appended ..
		tmpint <- as.bigz(tmp)
		theans[[jx]] <- c(theans[[jx]], as.character(tmpint) )
		tmpfrac <- tmp - tmpint 
		if (tmpfrac == 0) domore = FALSE
		loopcount = loopcount + 1
	}
# Convert each term separately to ensure, e.g., 14 base 10 turns into E base 16 
	termtmp <- rep('0',times=length(theans[[jx]]))	
	for (jz in 1:length(theans[[jx]])){
		termtmp[jz] <- unlist(base2base(theans[[jx]][[jz]], 10, outBase, classOut ='character'))
	}
	theans[[jx]] <- paste0(termtmp,collapse='') 
# collapse that list - and put decimal back in, ISO std
	theans[[jx]] <- paste0(c('0', thedec, unlist(theans[[jx]]), collapse='') )
	theans[[jx]] <- paste0(c(negval[[jx]],theans[[jx]]),collapse='')
} #end of jx loop

return(unlist(theans) ) 
}



