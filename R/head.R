setMethod(
	f = "head",
	signature = "BigDataFrame",
	definition = function(x, n = 6L){
		x[1:min(n, nrow(x)),]
	}
)


