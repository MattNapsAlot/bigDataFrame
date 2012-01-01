
setMethod(
	f = "as.vector",
	signature = "BigDataFrame",
	definition = function(x){
		as.vector(as.data.frame(x))
	}
)
