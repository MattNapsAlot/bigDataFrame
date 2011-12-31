setMethod(
	f = "tail",
	signature = "BigDataFrame",
	definition = function(x, n = 6L){
		x[max(1, nrow(x) - n + 1):nrow(x),]
	}
)
