setMethod(
	f = "BigDataFrame",
	signature = signature("character", "missing")
	definition = function(hdf5FilePath){
		new(Class="BigDataFrame", hdfFile=hdf5FilePath)
	}
)

setMethod(
	f = "BigDataFrame",
	signature = signature("missing", "missing")
	definition = function(){
		df <- new(Class="BigDataFrame", hdfFile=tempfile())
		
		## write dimensions
		dim(df) <- c(0,0)
		
		df
	}
)


setMethod(
	f = "BigDataFrame",
	signature = signature("missing", "data.frame"),
	definition = function(data){
		df <- BigDataFrame()
		colnames(df) <- colnames(data)
		rownames(df) <- rownames(data)
		
		df[1:nrow(data), 1:ncol(data)] <- data 
		
		df
	}
)
