setMethod(
        f = "dimnames",
        signature = "BigDataFrame",
        definition = function(x){
                dims <- list()
                dims[[1]] <- rownames(x)
                dims[[2]] <- colnames(x)
                dims
        }
)

setMethod(
        f = "dimnames<-",
        signature = "BigDataFrame",
        definition = function(x, value){
                rownames(x) <- value[[1]]
                colnames(x) <- value[[2]]
        	x
	}
)
