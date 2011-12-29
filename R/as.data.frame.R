setMethod(
	f = "as.data.frame",
	signature = signature("BigDataFrame"),
	definition = function(x, row.names = NULL, optional = FALSE, ...){
		x[1:nrow(x), 1:ncol(x)]
	}
)
