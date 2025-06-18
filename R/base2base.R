# Revisions
# June 2025 found & fixed bugs largely related to handling various forms of zero
# feb 2024 allow "binSize" to prepend zeroes to any base (as string)
# June-July 2023 fixed bug related to log() fun returning floating-point precision
#   errors that caused floor() to be too small. 
# May 2023: fixed bug related to lists vs. bigz objects.  Force numerics to "floor" to be integers. Returns non-invisibly now. 
#       Fixed bug that rejected  XXXe(+)YY
#	Added capability to recognize negative numbers and "carry" the sign to the output. 
#  Now input or output binaries in 2's complement as an option
# Fixed innumerable bugs related to extremely large integer inputs and conversions between various types and 2s comp
# May 2022 - collapse allChar[..] before stuffing into each theans[[jl]]
# catch bug when input is zero
# 

# TODO
# accept any value. separate integer and fraction & pass fraction to fracB2B.
#    But warn that bigq will NEVER be exact.

# IMPORTANT:  
#  When binSize is too small to fit , we override it. 
# Otherwise, We only extend size to specified binSize, OR if 2's comp is invoked for the output, we will extend to smallest 4N bitsize >= binSize 
 

base2base <- function(x, frombase=10, tobase=2, classOut=c('bigz', 'mpfr', 'numeric','character') , binSize = 0, inTwosComp = FALSE,  outTwosComp = FALSE ) {
if(!(2<=frombase && 36 >= frombase && 2<=tobase && 36>= tobase)){
	stop('Both bases must be in range 2:36')
}
classOut <- match.arg(classOut)
allChar <- c(0:9,letters)# validate by checking allChar[1:frombase]
theans <-list()  
inclass <- class(x)
#  pick the decimal point character based on Sys.localeconv()[1]	
thedec <- Sys.localeconv()[1]

# Feb 2024 moved this forcing inside the "if output is binary" sections.
# force nonzero binsize to power of 4
#if(binSize >0) binSize <- binSize + 3 - (binSize -1) %%4

# For safety later on, and to be sure we 'catch' 2s comp negative numbers
if (frombase !=2){
	inTwosComp = FALSE
	} else {
		if(inTwosComp) x <- as.character(x)
	}
# another safety
if (tobase !=2) outTwosComp = FALSE else {
# force nonzero binsize to power of 4
	if(binSize >0) binSize <- binSize + 3 - (binSize -1) %%4
	}
if (Rmpfr::is.mpfr(x)) x <- Rmpfr::.mpfr2bigz(x)
# check for gmp::is.bigz() and convert that to char as well
 if(!is.list(x)) x <- as.list(x)
for (jl in 1:length(x) ) {
	# a flag for negatives
	isPos = TRUE 
# nonlists are happy being indexed with [[k]][1] as are charstrings 
# kill off zeros and vals < 1,  like 1.23e-4 
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
	} else { 	# it's char strings 
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
		if(frombase == 2 && inTwosComp  && xtmp[1] =='1') {
			isPos = FALSE
	# calculate the binary string for positive xtmp first,force input to 4N here: 
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
# check for floats as strings if there's an 'e' , locate decimal point ; take the floor
# but if decimal and NO e, take floor()
#subtle bug: if idiot entered a hex, e.g.  '3e4', this if() will catch it.  Best I can do is make sure there's at least one char BEFORE an "e"
# Fixed 29aug2023
	if(frombase == 10 && length(grep('e',xchar)) && grep('e',xtmp)>1 ) {
				xchar <- noExp(xchar)
				xtmp <- unlist(strsplit(xchar,split=''))
	#  don't  make x[[jl]] numeric as that buggers bigz, for one thing. 
			} else 	if(length(grep(paste0('[',thedec,']'), xchar) ) ) {
# if it's xxx.(anything),that {,} requires a first zero {0,}
				xchar <- gsub(paste0('[',thedec,'].{0,}$'),'',xchar)
				warning('data to right of decimal removed. Failure may occur.')
				xtmp <- unlist(strsplit(xchar,split=''))	
			}	
	if (frombase == 16) {
		xchar <- gsub('^0x','',xchar) # just in case
		xtmp <- unlist(strsplit(xchar,split=''))
	}
	xtmp <- tolower(xtmp)	

	} #end of "else'   preparing numeric and char strings
	if(frombase < 36 && any(xtmp%in%allChar[(frombase+1):36] ) ) {
		badboys <- xtmp[xtmp%in%allChar[(frombase+1):36] ]
		warning( c('Character(s) "', badboys,'" not allowed in selected frombase') )
		theans[[jl]] <- '%no'   
		next # i.e. skip this item
		}
# Now start the conversion process  check whether incoming is > max_integer
# use length(xtmp)  to determine max power invoked for xtmp in base(frombase)
	maxpow <-frombase^length(xtmp) 
	if(maxpow < 2147483647) {   #2^31-1
#  strtoi won't overflow **because** I limited size of input to this 'if'	
#HOWEVER: stroi will "kill off" the lead zero if input is 2s comp
# TODO: catch NA which can occur if a UTF8 char snuck into the input x	
		foo <- strtoi(xchar,frombase)
#New June 2025: catch zero values here
# same fix for precision rounding errors as the "else" below
		if(foo > 0) {		
		powM <- floor(log(foo,tobase)) +1  # how many places needed

		mout <- rep('0',powM+1) 
		} else {
			powM = 0
			mout <- rep('0',times = max(16,binSize)+1)
			}
	#remember leftmost element of vector is index 1
#when powM = 0 this would cause 'trouble'
		if (powM > 0) {
			for (jp in powM:1) {
				divtmp <- tobase^jp
				mout[jp+1] <- foo%/%divtmp 
				foo <- foo%%divtmp
				}
			}
		# whatever's left is final digit
		mout[1] <- foo
# if MSdigit is zero, delete it
		if(mout[powM+1] == 0 )	mout <- mout[-(powM+1)]				
#  mout is in rev order, add  zeros for 2scmp and force length to 4N bits
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
			xrev <- rev(xtmp)
			for (jg in length(xtmp):1){
		#convert a:z into a number
				tmpval <- which(allChar == xrev[jg]) - 1 
# if bad, skip this element				
				if (!length(tmpval)) {
				  warning(c("Can't convert. Check for illegal symbol in input x[[",jl,']]'))
				  		theans[[jl]] <- '%no' 
				  next
				  }
				foo <- foo + tmpval * (gmp::as.bigz(frombase)^(jg-1) )
			    }
	# powM is numeric even when foo is bigz
			gtobase <- gmp::as.bigz(tobase)
# Fix same bug as above: if input is lots of zeros, need to adjust powM			
# foo == 2^62 returns a log 61.99999..fix by just adding one
			if(foo > 0 ) {
				powM <- floor(log(foo,gtobase)) + 1 # how many places will I need
				mout <- rep('0',powM+1) 
			} else{
				powM = 0
				mout <- rep('0',times = max(16,binSize)+1)
			}
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
# now remove that 'safety' digit if it's zero (mout is still in rev order)
		if(mout[powM+1] == 0 )	mout <- mout[-(powM+1)] 	
	 		 	}
# just start with:
# CHANGE Jan 24: split so all isPos get handled either in binary code or in all other bases. 
#	if(isPos && tobase == 2){
	if(isPos) {
		if(tobase == 2){
			ktmp <- allChar[rev(as.numeric(mout)+1)]
			klen <- length(ktmp)
			if(outTwosComp && ktmp[1] == '1'){
				ktmp <- c('0',ktmp)
				klen <- length(ktmp)
			}
# now expand to binsize
			ktmp <- c(rep('0',times = max(0,binSize - klen)), ktmp)
			klen <- length(ktmp)
	# need this for when binSize is zero
			ktmp <- c(rep('0',times = 3 -(3+klen)%%4),ktmp)
			theans[[jl]] <- paste0(c(ktmp) ,sep='',collapse='')
		} else {
# all other bases, positive value
#   don't want to collapse jltmp until after adding the zeros
			jltmp <- allChar[rev(as.numeric(mout)+1)]
			jlen <- length(jltmp)
			jltmp <- c(rep('0',times = max(0,binSize - jlen)), jltmp)
# June 2025: this was a bug I missed -- forgot to collapse it
#			theans[[jl]] <- jltmp 
			theans[[jl]] <- paste0(jltmp ,sep = '', collapse='')
		}	#end of if else tobase==2
	}  else {
# it's !isPos 
# remember outTwosComp is forced to FALSE if tobase != 2		
			theans[[jl]] <- paste0(allChar[rev(as.numeric(mout)+1)],sep='',collapse='')
# I made xchar stuff positive for 2s comp inputs
# But I didn't make theans[[]] include the lead zero
			jltmp <- unlist(strsplit(theans[[jl]][1],'') )
			if (tobase==2 && outTwosComp) { 
# FIRST flip all the bits!
#  theans will be "normal" binary so watch for a lead '1'
				if (jltmp[1] == '1') jltmp <- c('0',jltmp)
# extend to 4N right here
				blen <- length(jltmp)
# skip the 'if' by being creative with (4-1)
# yes '0' since still working with the positive value here
				jltmp <- c(rep('0',times = 3 -(3+blen)%%4),jltmp)
				jltmp <- as.character(as.numeric((!as.numeric(jltmp) ) ) )
				blen <- length(jltmp)
# with my byte-extending above, IF binSize > 0 and other stuff,
#  don't add a bit. MSB will always be '0' coming Otherwise , just prepend a '1' 
# Case 1:  as.numeric(theans[[jl]][1])  == 0 means return 0 . that's all.
# Case 2: first lengthen jltmp to match binSize if necessary. Then do the "add 1" thing
				getzer <- rev(grep('0',jltmp)) # just for ease				
				if (as.numeric(theans[[jl]][1])  == 0 ) {
					jltmp <- rep('0' , times = max(blen, binSize) ) 
				} else {
# since theans is not zero, there will be zeros in the flipped version				
					if (getzer[1] < blen){
						jltmp[getzer[1]] <- '1'
						jltmp[(getzer[1]+1):blen] <- '0'
					} else {
						# it was all zeros; getzer[1] == blen
						jltmp[getzer[1]] <- '1'
					}
#'move' the existing lead '1'to the lead position
					jltmp <- c(jltmp[1], rep('1',times=max(0,binSize-blen)), jltmp[2:blen])	
				} #end of if else 	as.numeric theans
				theans[[jl]] <- paste0(c(jltmp), sep='', collapse='')					
#	this else follows if(tobase == 2 && outTwosComp)
			} else {
	# i.e. not both tobase ==2 AND outTwoscomp true
# now expand to binSize
				if(tobase == 2) {
					jlen <- length(jltmp)
					jltmp <- c(rep('0',times = max(0,binSize - jlen)), jltmp)
					jlen <- length(jltmp)
# need this for when binSize is zero
					jltmp <- c(rep('0',times = 3 -(3+jlen)%%4),jltmp)	
					theans[[jl]] <- paste0(c(jltmp), sep='', collapse='')
				}else {
#   stick zeros in front of any other  base.	
					jlen <- length(jltmp)
					jltmp <- c(rep('0',times = max(0,binSize - jlen)), jltmp)
					theans[[jl]] <- paste0(jltmp,sep='',collapse='')
					}
				theans[[jl]] <- paste0(c('-',theans[[jl]]), sep='', collapse='')
			}  #end of if tobase ==2 else	
				
		} # end of the "else" for !isPos
#	} # end of if isPos
} #end of for jl
# as.bigz thinks a lead '0' means octal, so to be  safe  strip lead zeros 
if (tobase == 10 ) {
	switch(classOut[1], 
		'numeric'= for(jj in 1:length(theans)) theans[[jj]] <- as.numeric(theans[[jj]]),
		'bigz'= {	
#if theans is just '0' this deletes it. So  skip single-char inputs	
#  extend '0' after this.  the output is "" , which is hard to identify		
			for(jj in 1:length(theans) ){
				if (nchar(theans[[jj]]) > 1){
					theans[[jj]]  <- gsub('^0{1,}', '', theans[[jj]])
					theans[[jj]]  <- gsub('^-0{1,}', '-', theans[[jj]])
				}
# new fix
				if(!nchar(theans[jj])) theans[[jj]] <- paste0(rep('0', times= max(16,binSize)), sep='', collapse = '')			
				theans[[jj]] <- gmp::as.bigz(theans[[jj]])
			} 
		},
# but if want this to work, have to remove and put back the zeroes because again bigz 
# thinks I fed it an octal.		
		'mpfr'=  {
			for(jj in 1:length(theans) ) {
				if (nchar(theans[[jj]]) > 1){
					theans[[jj]]  <- gsub('^0{1,}', '', theans[[jj]])
					theans[[jj]]  <- gsub('^-0{1,}', '-', theans[[jj]])
					}
# new fix
				if(!nchar(theans[jj])) theans[[jj]] <- paste0(rep('0', times=max(16,binSize)), sep='', collapse = '')			

			 theans[[jj]] <- Rmpfr::.bigz2mpfr(gmp::as.bigz(theans[[jj]]))
			 }
		},
	#default I think. Not sure why this worked before
	   ''
	)
}
return(theans)
}
