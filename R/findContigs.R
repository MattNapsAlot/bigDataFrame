.findContigs <-
	function(indx)
{
	indx <- as.numeric(indx)
	if(any(is.na(indx))) stop("missing values")
	
	if(any(indx < 0)) stop("indices must be positive integers")

	parts <- list()

	diffs <- indx[-1] - indx[-length(indx)]
	
	mk <- diffs != 1L
	if(all(!mk)){ 
		parts[[1]] <- indx
		return(parts)	
	}
	breaks <- which(mk) + 1L

	for(i in 1:length(breaks)){
		if(i == 1L){
			parts[[i]] <- indx[1:(breaks[i] - 1L)]
		}else{
			parts[[i]] <- indx[breaks[i - 1L]:(breaks[i] - 1L)]
		}
	}
	if(length(indx) > 1L){
		if(breaks[i] != length(indx)){
			parts[[length(breaks) + 1L]] <- indx[breaks[i]:length(indx)]
		}else{
			parts[[length(breaks) + 1L]] <- indx[breaks[i]]
		}
	}

	parts
}

