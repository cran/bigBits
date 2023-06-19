# Revisions
# May 2023: fixed bug related to lists vs. bigz objects.  Force numerics to "floor" to be integers. Returns non-invisibly now. 
#       Fixed bug that rejected  XXXe(+)YY
#	Added capability to recognize negative numbers and "carry" the sign to the output. 
#  Now input or output binaries in 2's complement as an option
# Fixed innumerable bugs related to extremely large integer inputs and conversions between various types and 2s comp
# May 2022 - collapse allChar[..] before stuffing into each theans[[jl]]
# catch bug when input is zero
# 

# IMPORTANT:  
#  When binSize is too small to fit , we override it. 
# Otherwise, We only extend size to specified binSize, OR if 2's comp is invoked for the output, we will extend to smallest 4N bitsize >= binSize 

base2base <- function(x,frombase=10, tobase=2, classOut=c('bigz', 'mpfr', 'numeric','character') , binSize = 0, inTwosComp = FALSE,  outTwosComp = FALSE ) {
allChar <- c(0:9,letters)# validate by checking allChar[1:frombase]
theans <-list()
inclass <- class(x)
# force nonzero binsize to power of 4
if(binSize >0) binSize <- binSize + 3 - (binSize -1) %%4
# For safety later on, and to be sure we 'catch' 2s comp negative numbers
if (frombase !=2){
	inTwosComp = FALSE
	} else {
		if(inTwosComp) x <- as.character(x)
	}
if (Rmpfr::is.mpfr(x)) x <- Rmpfr::.mpfr2bigz(x)
# check for gmp::is.bigz() and convert that to char as well
 if(!is.list(x)) x <- as.list(x)
for (jl in 1:length(x) ) {
	# a flag for negatives
	isPos = TRUE 
# nonlists are happy being indexed with [[k]][1] as are charstrings 
# kill off zeros and things like 1.23e-4 
	if( !is.na(suppressWarnings(as.numeric(x[[jl]])) ) && abs(as.numeric(x[[jl]]) ) < 1){
		theans[[jl]] <- '0'
		next
	}
	if (is.numeric(x[[jl]]) || gmp::is.bigz(x[[jl]])){
# remove decimal part
		if(is.numeric(x[[jl]])){
			x[[jl]] <- floor(abs(x[[jl]] )) *sign(x[[jl]])	
# catch numerics with exponents here by converting		
			x[[jl]] <- as.bigz(x[[jl]])				
		}
# this covers numerics and bigz.  mpfrs were prev. converted to bigz.		
		if (x[[jl]] < 0) {
			isPos = FALSE
			x[[jl]] <- -x[[jl]]
		} 	 
 		xchar <- as.character(x[[jl]])
		xtmp <- unlist(strsplit(xchar,''))		
	} else { 	
# it's char strings 
		if (length(x[[jl]]) > 1) {
			xchar <- paste0(x[[jl]],sep='',collapse='')
		}  else xchar <- x[[jl]]
# clear any whitespace
		xchar <- gsub('[\\s]{1,}','',xchar,perl=TRUE)
# check for neg sign (but not one in an exponent. the call to noExp() needs to see that)
		if (length(grep('^-', xchar))) {
			isPos = FALSE
			xchar <- gsub('^-','',xchar)
		}
# get rid of plus sign just in case user is a dope
		if (length(grep('[+]', xchar))) {
			xchar <- gsub('[+]','',xchar)
		}	
		xtmp <- unlist(strsplit(xchar,''))
#  check for 2scomp incoming
		if(frombase == 2 && inTwosComp  && xtmp[1] =='1') {
			isPos = FALSE
	# calculate the binary string for positive xtmp 
# first,force input to 4N here: 
			xlen <- length(xtmp)
			if (xlen == 1){
				xtmp = c('1','1','1','1')
				xlen = 4
			} 
			if (xlen%%4) {
# since x is a neg 2scomp, always extend with lots of '1' 				
				xtmp <- c(xtmp[1], rep('1',times = 4-xlen%%4), xtmp[2:xlen] )
			}
			
#  do the "simple" trick : flip all bits and add one. 
	# first, get 1s complement first element will now become '0' , don't lose it!
		compOne <- !(as.logical(as.numeric(xtmp) ) )
		compOne <- as.character(as.numeric(compOne) )  #all ones and zeros
# now look for RH zeros to simplify "adding one" 
		blen <- length(compOne)
		 # testit <- foob
		getzer <- rev(grep('0',compOne)) # just for ease		
		# there is ALWAYS at least one zero now
#	first add one and then check whether MSb is now one
		compOne[getzer[1]] <- '1'
		if(getzer[1] < blen) {
			compOne[(getzer[1]+1):blen] <- '0'
		}
		if(compOne[1] == '1' ){ 
			compOne <- c('0',compOne)
				} 
		xtmp <- compOne	
# this is necessary because previous xchar may not have had a necessary lead '0'		
		xchar <- paste0(c(xtmp),sep = '', collapse='')
# If it was all zeros, we leave it alone.					
		}			  
# check for floats as strings
#if there's an 'e' , hack the decimal point location and take the floor
# but if decimal and NO e, take floor()
if(frombase == 10 && length(grep('e',xchar))) {
			xchar <- noExp(xchar)
			xtmp <- unlist(strsplit(xchar,split=''))
#  don't  make x[[jl]] numeric as that buggers bigz, for one thing. 		
		} else 	if(length(grep('[.]', xchar) ) ) {
			xchar <- gsub('[.].{,}$','',xchar)
			warning('data to right of decimal removed. Failure may occur.')
			xtmp <- unlist(strsplit(xchar,split=''))	
		}	
	if (frombase == 16) {
		xchar <- gsub('^0x','',xchar) # just in case
		xtmp <- unlist(strsplit(xchar,split=''))
	}
	xtmp <- tolower(xtmp)	
	if(!(2<=frombase && 36 >= frombase && 2<=tobase && 36>= tobase)){
		stop('Both bases must be in range 2:36')
		}
	} #end of "else'   preparing numeric and char strings	
	if(frombase < 36 && any(xtmp%in%allChar[(frombase+1):36] ) ) {
		badboys <- xtmp[xtmp%in%allChar[(frombase+1):36] ]
		warning( c('Character(s) ', badboys,' not allowed in selected frombase') )
		next # i.e. skip this item
		}
	#  check whether incoming is > max_integer
	# use length(xtmp)  to determine max power invoked for xtmp in base(frombase)
	maxpow <-frombase^length(xtmp) 
	if(maxpow < 2147483647) {   #2^31-1
#  strtoi won't overflow **because** I limited size of input to this 'if'	
#HOWEVER: stroi will "kill off" the lead zero if input is 2s comp	
		foo <- strtoi(xchar,frombase)
		# powM is numeric even when foo is bigz
		powM <- floor(log(foo,tobase)) # how many places needed
		mout <- rep('0',powM+1) 
	#remember leftmost element of vector is index 1
#when powM = 0 this would awry
		if (powM > 0) {
			for (jp in powM:1) {
				divtmp <- tobase^jp
				mout[jp+1] <- foo%/%divtmp 
				foo <- foo%%divtmp
				}
			}
		# whatever's left is final digit
		mout[1] <- foo
# here, remembering mout is in rev order, add  zeros for 2scmp
# force length to 4N bits only do this if tobase == 2
		if(inTwosComp == TRUE && frombase == 2 && tobase == 2) {
			# mout <- c(mout, '0')
			mlen <- length(mout)
			if (mlen%%4) {
				mout <- c(mout, rep('0',times = 4 - mlen%%4))
			}	
		} 
		} else {
#big number so use gmp and do things the hard way,loop over x terms
			foo <- gmp::as.bigz(0)   
			# get the index order right!
				xrev <- rev(xtmp)
			for (jg in length(xtmp):1){
		#convert a:z into a number
				tmpval <- which(allChar == xrev[jg]) - 1 
# if bad, skip this element				
				if (!length(tmpval)) {
				  warning(c("Can't convert. Check for illegal symbol in input x[",jl,']'))
				  next
				  }
				foo <- foo + tmpval * (gmp::as.bigz(frombase)^(jg-1) )
			    }
	# powM is numeric even when foo is bigz
# log(gmp) comes out with precision error but NOT if I make 'tobase' bigz as well!

			gtobase <- gmp::as.bigz(tobase)
			powM <- floor(log(foo,gtobase)) # how many places will I need
			mout <- rep('0',powM+1) 
			if(powM >0 ) {	
		#remember leftmost element of vector is index 1 
				for (jp in powM:1) {
					divtmp <- gtobase^jp
					mout[jp+1] <- as.character(foo%/%divtmp )
					foo <- foo%%divtmp
					}
			}

	# whatever's left is final digit
	 	mout[[1]] <- as.character(foo)
	 	}
# remember I forced inTwosComp to be false if inbase !=2 	
	if(isPos && inTwosComp == FALSE && outTwosComp == TRUE && tobase == 2){
# also check for binsize. 
#insert lead zero to ensure the output is proper 2s comp form
		ktmp <- allChar[rev(as.numeric(mout)+1)]
		klen <- length(ktmp)
		ktmp <- c('0',rep('0',times = max(0,binSize - klen)), ktmp)
		# Since I forced binSize%%4 to be zero, I shouldn't need to pad here
		klen <- length(ktmp)
		ktmp <- c(rep('0',times = 3 -(3+klen)%%4),ktmp)
		theans[[jl]] <- paste0(c(ktmp) ,sep='',collapse='')
	} else if (isPos && inTwosComp == TRUE && outTwosComp == TRUE && frombase == 2 && tobase == 2)  {
			# just need to add lead zeros
			ktmp <- allChar[rev(as.numeric(mout)+1)]
			klen <- length(ktmp)
			ktmp <- c(rep('0',times = max(0,binSize - klen)), ktmp)
			theans[[jl]] <- paste0(c(ktmp) ,sep='',collapse='')
		} else {
	theans[[jl]] <- paste0(allChar[rev(as.numeric(mout)+1)],sep='',collapse='')
	} #this finishes if/else/else from if(is &&In&&OUT&&tobase)
	 if (!isPos) {
			if (tobase==2 && outTwosComp) { 
# FIRST flip all the bits!
# I made xchar stuff positive for 2s comp inputs
# But I didn't make theans[[]] include the lead zero
				jltmp <- unlist(strsplit(theans[[jl]][1],'') )
#  theans will be "normal" binary so watch for a lead '1'
				if (jltmp[1] == '1') jltmp <- c('0',jltmp)
# extend to 4N right here
				blen <- length(jltmp)
# skip the 'if' by being creative with (4-1)
# yes '0' since still working with the positive value here
				jltmp <- c(rep('0',times = 3 -(3+blen)%%4),jltmp)
				jltmp <- as.character(as.numeric((!as.numeric(jltmp) ) ) )
				blen <- length(jltmp)
# NOTE: when using 2s comp, even a positive number requires a lead "extra" zero as a placeholder.   				
# with my byte-extending above, IF binSize > 0 and other stuff,
#  don't add a bit. MSB will always be '0' coming
# Otherwise , just prepend a '1' 
# Case 1:  as.numeric(theans[[jl]][1])  == 0 means return 0 . that's all.
# Case 2: first lengthen jltmp to match binSize if necessary. Then do the "add 1" thing
				getzer <- rev(grep('0',jltmp)) # just for ease				
				if (as.numeric(theans[[jl]][1])  == 0 ) {
					jltmp <- rep('0' , times = max(blen, binSize) ) #max(2, binSize) ) 
				} else {
# since theans is not zero, there will be zeros in the flipped version				
					if (getzer[1] < blen){
						jltmp[getzer[1]] <- '1'
						jltmp[(getzer[1]+1):blen] <- '0'
					} else {
						# it was all zeros; getzer[1] == blen
						jltmp[getzer[1]] <- '1'
					}
#	ALSO:  'move' the existing lead '1'to the lead position!
					jltmp <- c(jltmp[1], rep('1',times=max(0,binSize-blen)), jltmp[2:blen])				
				} #end of first 'else' 	
				theans[[jl]] <- paste0(c(jltmp), sep='', collapse='')					
	#	this else follows if(tobase == 2 && outTwosComp)
			} else 	theans[[jl]] <- paste0(c('-',theans[[jl]]), sep='', collapse='')
				
		} # end of if !isPos
} #end of for jl
# as.bigz thinks a lead '0' means octal, so to be on the safe side
# strip lead zeros 
if (tobase == 10 ) {
	switch(classOut[1], 
		'numeric'= for(jj in 1:length(theans)) theans[[jj]] <- as.numeric(theans[[jj]]),
		'bigz'= {	
#if theans is just '0' this deletes it. So  skip single-char inputs			
			for(jj in 1:length(theans) ){
				if (nchar(theans[[jj]]) > 1){
					theans[[jj]]  <- gsub('^0{1,}', '', theans[[jj]])
					theans[[jj]]  <- gsub('^-0{1,}', '-', theans[[jj]])
				}
				theans[[jj]] <- gmp::as.bigz(theans[[jj]])
			} 
		},
		'mpfr'= for(jj in 1:length(theans) ) theans[[jj]] <- Rmpfr::.bigz2mpfr(gmp::as.bigz(theans[[jj]]))
	)
}
return(theans)
}
