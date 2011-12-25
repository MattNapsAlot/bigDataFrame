setMethod(
	f = "BigDataFrame",
	signature = "character",
	definition = function(x){
		new(Class="BigDataFrame", hdfFile=x)
	}
)
