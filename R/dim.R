setMethod(
        f = "dim",
        signature = "BigDataFrame",
        definition = function(x){
                return(c(nrow(x), ncol(x)))
        }
)

setMethod(
	f = "dim<-",
	signature = signature("BigDataFrame"),
	definition = function(x, value){
		value <- as.numeric(value)
		if(any(is.na(value))) stop("the dims contain missing values")	
						
		if(!("/all.data/nrow" %in% HDF5Summary(hdfFile(x))$datasetsummary))
			nrow(x) <- value[1]
		if(!("/all.data/ncol" %in% HDF5Summary(hdfFile(x))$datasetsummary))
			ncol(x) <- value[2]
		x		
	}
)

