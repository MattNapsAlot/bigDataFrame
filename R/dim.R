setMethod(
        f = "dim",
        signature = "BigDataFrame",
        definition = function(x){
                return(c(nRow(x), nCol(x)))
        }
)

setMethod(
	f = "dim<-",
	signature = signature("BigDataFrame"),
	definition = function(x, value){
		value <- as.numeric(value)
		if(any(is.na(value)) stop("the dims contain missing values")	
		
		
	}
)

