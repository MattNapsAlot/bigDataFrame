setMethod(
	f = "rownames",
	signature = "BigDataFrame",
	definition = function(x){
		if(!("/all.data/rowNames" %in% HDF5Summary(hdfFile(x))$datasetsummary)) return(NULL)
		HDF5ReadData(hdfFile(x), "/all.data/rowNames")[,1]
	}
)

setMethod(
	f = "rownames<-",
	signature = "BigDataFrame",
	definition = function(x, value){
		if(nrow(x) != length(value))
			stop("dims don't match")
		HDF5WriteData(hdfFile(x), "/all.data/rowNames", value, options=list(overwrite=TRUE))
		x
	}
)



