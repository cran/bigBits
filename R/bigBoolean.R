#  bitwise for giant integers 
# the base::bitw* funcs appear to recycle silently.
# inTwosComp is checked if inBase ==2 ,
# the default is TRUE to match bitw* funcs
# base2base will call noExp if needed.  
bigOr <- function(x, y, inBase = 10, outBase = 10, inTwosComp = TRUE) {
xlen <- length(x)
ylen <- length(y)
if(ylen < xlen) {
	y <- rep(y, length=xlen)
} else if (xlen < ylen) x <- rep(x, length=ylen)
out <- gmp::as.bigz(rep(0,times=length(x))) 
for(jj in 1:length(x)) {
	thebins <-buildBinaries(x[[jj]], y[[jj]], inBase)
	out[[jj]] <- gmp::as.bigz(unlist(base2base(paste0(bitwOr(thebins$xbin, thebins$ybin), sep='', collapse=''), 2, 10, inTwosComp = TRUE) ))
}
# provide output in source 'base',which will be char whenever not 10
#make mpfr from bigz to ensure precision.
	if(is(x,'mpfr') || is(x,'mpfr1')) {
		out <- Rmpfr::.bigz2mpfr(out)
	} else if(is(x,'numeric')) {
		out <- as.numeric(out)
	} else if(is(x,'character')) {
		outc <- rep('0',times=length(out))		
	for (jc in 1:length(out)) {
		outc[jc] <- as.character(base2base(out[jc],10,outBase, outTwosComp = inTwosComp)[[1]] )
		}
	out <- outc
	} 
 return(out) 
}
 
bigAnd <- function(x, y, inBase = 10, outBase = 10,  inTwosComp = TRUE) { 
if(!length(x) || is.na(x)  ) browser()
xlen <- length(x)
ylen <- length(y)
if(ylen < xlen) {
	y <- rep(y, length=xlen)
} else if (xlen < ylen) x <- rep(x, length=ylen)
out <- gmp::as.bigz(rep(0,times=length(x))) 
for(jj in 1:length(x)) {
	thebins <-buildBinaries(x[[jj]], y[[jj]], inBase)
	theand <- paste0(bitwAnd(thebins$xbin, thebins$ybin), sep='', collapse='')
	out[[jj]] <- gmp::as.bigz(unlist(base2base(theand,2,10, inTwosComp=TRUE) ))
	}
# provide output in source 'base'  
	if(is(x,'mpfr') || is(x,'mpfr1')) {
		out <- Rmpfr::.bigz2mpfr(out)
	} else if(is(x,'numeric')) {
		out <- as.numeric(out)
	} else if(is(x,'character')) {
	outc <- rep('0',times=length(out))		
	for (jc in 1:length(out)) {
		outc[jc] <- as.character(base2base(out[jc],10,outBase, outTwosComp = inTwosComp)[[1]] )
		}
	out <- outc
	} 
 return(out) 
}

bigXor <- function(x, y, inBase = 10, outBase = 10, inTwosComp = TRUE) {
xlen <- length(x)
ylen <- length(y)
if(ylen < xlen) {
	y <- rep(y, length=xlen)
} else if (xlen < ylen) x <- rep(x, length=ylen)
out <- gmp::as.bigz(rep(0,times=length(x))) 
for(jj in 1:length(x)) {
	thebins <-buildBinaries(x[[jj]], y[[jj]], inBase)
	out[[jj]] <- gmp::as.bigz(unlist(base2base(paste0(bitwXor(thebins$xbin, thebins$ybin), sep='', collapse=''), 2, 10,inTwosComp = TRUE) ))
	}
# provide output in source 'base'  
	#make mpfr from bigz anyway to ensure precision.
	if(is(x,'mpfr') || is(x,'mpfr1')) {
		out <- Rmpfr::.bigz2mpfr(out)
	} else if(is(x,'numeric')) {
		out <- as.numeric(out)
	} else if(is(x,'character')) {
		outc <- rep('0',times=length(out))		
			for (jc in 1:length(out)) {
				outc[jc] <- as.character(base2base(out[jc],10,outBase, outTwosComp = inTwosComp)[[1]] )
				}
			out <- outc
	} 
 return(out) 
}

#   note- outTwosComp ignored unless outBase is 2
bigNot <- function(x,  inBase = 10, outBase = 10,  binSize = 32, inTwosComp = TRUE, outTwosComp = TRUE) {
 out <- gmp::as.bigz(rep(0,times=length(x)))
# Safety:
	if(inBase != 2) inTwosComp = FALSE
# thebins$xbin will always a binary 2's comp string 
	if(inTwosComp ) {
#  get 1s complement
		for(jj in 1:length(x)) {
			thebins <-buildBinaries(x[jj], y=NULL, inTwosComp = inTwosComp,  inBase, binSize = binSize)
			compOne <- !(as.logical(unlist(thebins$xbin)) )
			compOne <- as.character(as.numeric(compOne) )  #all ones and zeros
# compOne is now NOT(x) and is in 2s comp format
# forcing out[[]] to be bigz for first step		
			otmp <- unlist(paste0(compOne,sep='',collapse=''))
			out[[jj]] <- base2base(otmp,2,10, inTwosComp = TRUE)[[1]]	
		}
# this else is the end of "if(inTwosComp)"
	} else {
	# default output of buildbinaries is 2s comp
		for(jj in 1:length(x)) { 
			thebins <-buildBinaries(x[jj], y=NULL, inBase, binSize = binSize)		
			otmp <- base2base(paste0(as.numeric(!thebins$xbin), sep='', collapse=''), 2, 10, inTwosComp = TRUE)  
			out[[jj]] <- otmp[[1]]  # leave it if is bigz
		}

	} #end of else
# now convert to class of input bigz , numeric, mpfr only work with base10 .
		 if(is(x,'mpfr') || is(x,'mpfr1')) {
			out <- Rmpfr::.bigz2mpfr(out)
		} else if(is(x,'numeric')) {
			out <- as.numeric(out)
		} else if(is(x,'character')) {
			outc <- rep('0',times=length(out))		
				for (jc in 1:length(out)) {
					outc[jc] <- as.character(base2base(out[jc],10,outBase, outTwosComp = inTwosComp)[[1]] )
					}
				out <- outc
		} 
 return(out) 
}

#  have to specify 2s comp or not when inBase ==2
#  binsize allows truncated shifted value which then maps to 2's comp
# possibly negative number 
#Note:  rep() has a default argument "times" ==1 , so failure to enter a "shift" arg
# results in shift value of 1
bigShiftL <- function(x, shift = 1,  inBase = 10 , outBase = 10, binSize = 32, inTwosComp = TRUE) {

 out <- gmp::as.bigz(rep(0,times=length(x)))
	for(jj in 1:length(x)) {
	thebins <- buildBinaries(x[[jj]], y=NULL, inBase,inTwosComp = inTwosComp)
		shifted <- c(as.character(thebins$xbin), rep('0', times=shift))
# check binSize and truncate the shifted bits if necessary
# base2base will take care of making the final result a proper 4*N bits
		if (binSize > 0) {
			binSize <- binSize + 3 - (binSize -1) %%4 
			truncbin <- max(32,binSize) #32 is bitwShiftL limit
			if (truncbin < length(shifted)) {
				shifted <- rev(rev(shifted)[1:truncbin])
			}
	# otherwise do nothing because the shifted value is in range		
		}
		otmp <- unlist(paste0(shifted,sep='',collapse='') )
		
# provide output in source 'base' and class. for binary, always return 2s comp
		out[jj] <- base2base(otmp,2, 10, binSize= binSize, inTwosComp=TRUE, outTwosComp=TRUE)[[1]] 
# now convert classes of base10 items. Note: while base2base returns same class as input,
#  the input to bigShift* is not the class returned when converting 'out' in the line above. but out[] is bigz by default when inBase == 10 
}
	if(is(x,'mpfr') || is(x,'mpfr1')) {
		out <- Rmpfr::.bigz2mpfr(gmp::as.bigz(out))
	} else if(is(x,'numeric')) {
		out <- as.numeric(out)
	} else if (is(x,'character')){
# have to reset type - use a temp
			charout <- rep('0',times=length(out))	
			for (jc in 1:length(out)) {				
			charout[jc] <- as.character((base2base(out[jc], 10, outBase,binSize=binSize, inTwosComp = TRUE, outTwosComp = inTwosComp) )[[1]]  )
			}
			out <- charout 
	}

 return(out) 
}

# PROBLEM with  negative numbers.  bitwShiftR assumes 32-bit binary 2scomp.  But given an arbitrary size input, the output , for a shift of one, move -1 (11111...) to 2^(N-1) -1 , where N includes te sign bit. This func will default to max(inBase,32,min_rq'd_for_magnitude_of_x)
# fixedBUG: sending x==0 causes trouble in indices
# fixedBUg: failed to pass input binSize to calls 
bigShiftR <- function(x, shift = 1,  inBase = 10, outBase = 10, binSize = 32, inTwosComp = TRUE) {
#default binary size is 32 to match bitwShiftR 	
if(!length(x) || is.na(x)) browser()
 out <- gmp::as.bigz(rep(0,times=length(x)))
	for(jj in 1:length(x)) {
	thebins <-buildBinaries(x[[jj]], y=NULL, inBase, binSize = 32, inTwosComp = inTwosComp)
		xlen = length(thebins$xbin)
		shifted <- thebins$xbin[1:(xlen-shift)]
# now make sure to add zeros to LH end to refill to 4N, at least 32 long
		shifted <- c(rep(0,times= shift),shifted)		
		otmp <- unlist(paste0(shifted,sep='',collapse='') )	
# convert to 10 and then back to inbase
		out[[jj]] <- base2base(otmp, 2, 10, binSize = binSize, inTwosComp=TRUE, outTwosComp=TRUE)[[1]]
	}
# provide output in source 'inBase'
	if(is(x,'mpfr') || is(x,'mpfr1')) {
		out <- Rmpfr::.bigz2mpfr(gmp::as.bigz(out[[1]]))
	} else if(is(x,'numeric')) {
		out <- as.numeric(out[[1]])
	} else if (is(x,'character')){
	# have to reset type - use a temp
			charout <- rep('0',times=length(out))
# since b2b defaults to binSize 0, need to spec in here
			for (jc in 1:length(out)) {				
			charout[jc] <- as.character((base2base(out[jc], 10, outBase, binSize = binSize, inTwosComp = TRUE, outTwosComp = inTwosComp) )[[1]]  )
			}
			out <- charout 
	}
 return(out) 
}

# what should I do with negative numbers? with 2scomplement numbers? "undefined & thus platform and compiler - dependent."  So I"m gonna convert any 2s comp input to unsigned plus a neg sign and work with that.  
bigRotate <- function(x, shift,  inBase = 10,binSize = 32, outBase = 10, inTwosComp = TRUE) {
#default binary size is 32 to match bitwShiftR 	
 shift = floor(shift[1])
 out <- rep('0',times = length(x))  
 for(jj in 1:length(x)) {
 		bintmp <- base2base(x[[jj]],inBase,2, binSize = binSize, inTwosComp = inTwosComp, outTwosComp = FALSE)[[1]]
#now all inputs will have a neg sign if negative. 
 		isPos = TRUE
 		if(length(grep('-',bintmp)) ) {
 			isPos = FALSE
 			bintmp <- gsub('^-','',bintmp)
 		}
 		bintmp <- strsplit(bintmp,'')[[1]]
 		xlen = length(bintmp)
 	# now rotate the bits.	
 		otmp  <- unlist(paste0(c(bintmp[ ((1:xlen) + shift -1) %%(xlen) + 1]),collapse=''))
 		out[jj] <- base2base(otmp, 2, outBase, binSize=binSize, outTwosComp = FALSE, classOut = "character")[[1]]
 		if(!isPos) out[jj] <- paste0('-',out[jj], collapse='')
 	}	
# provide output in source 'inBase'
	if(outBase ==10) {
		if(is(x,'mpfr') || is(x,'mpfr1')) {
			out <- Rmpfr::.bigz2mpfr(gmp::as.bigz(out))
		} else if(is(x,'numeric')) {
			out <- as.numeric(out)
		} else if(is(x,'bigz')) out <- gmp::as.bigz(out)
	}	
 return(out) 
}


# Output must be 2scomp to handle neg numbers correctly 
# Funcs which call this MUST specify binSize if they want a specific wordlength
# note also that the call to base2base will force length to 4N bits
buildBinaries <- function(x, y= NULL,inBase, inTwosComp = FALSE, binSize = 32) {
	if(length(x) > 1){
		warning('Only first element of x will be used. ')
		x <- x[[1]]
	}
	xbin <- unlist(base2base(x,inBase,2, inTwosComp = inTwosComp, binSize=binSize, outTwosComp = TRUE) )
	#now extract the charstrings
	if(length(y) ) {
		if( length(y) > 1 ) {
			warning('Only first element of y will be used. ')
			y <- y[[1]]
		}
		ybin <- unlist(base2base(y,inBase,2, inTwosComp = inTwosComp, binSize=binSize, outTwosComp = TRUE ) )
# if either xbin or ybin is negative, need to keep the LHbit '1' 
# AND need to fill in with '1' not zeros 		
		if(!length(xbin) || !length(ybin)) {
			stop('bad inputs; could not convert.')
		}
		lendiff <- nchar(xbin) -nchar(ybin)
		if(abs(lendiff) > 0 ){
# fill with lead digit, as that's how 2's comp works
#  put a conditional such that length==1 inputs are treated differently. 
			ytmp <- unlist(strsplit(ybin,split=''))
			xtmp <- unlist(strsplit(xbin,split= '') )
			if (length(ytmp) > 1){	
				ybin <- c(ytmp[1], rep(ytmp[1], times = max(0, +lendiff)), ytmp[2:length(ytmp)])	
			} else ybin <- c(ybin,rep(ytmp,times = max(0, +lendiff)))
			ybin <- as.numeric(ybin)
			if (length(xtmp) > 1){
				xbin <- c(xtmp[1], rep(xtmp[1], times = max(0, -lendiff)), xtmp[2:length(xtmp)])
			} else xbin < c(xtmp, rep(xtmp, times= max(0, -lendiff)))
			xbin <- as.numeric(xbin)
		} else {
			ybin <- as.numeric(unlist(strsplit(ybin,'') ))
			xbin <- as.numeric(unlist(strsplit(xbin,'') ))
		}				
	} else {
		ybin = 0 #placeholder
		xbin <- as.numeric(unlist(strsplit(xbin,'' ) )) 
	}
	return(list(xbin=xbin, ybin=ybin))
}



