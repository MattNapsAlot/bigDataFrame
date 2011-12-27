setMethod(
	f = "colClasses",
	signature = "BigDataFrame",
	definition = function(x){
		if(!("/all.data/colClasses" %in% HDF5Summary(hdfFile(x))$datasetsummary))
			return(NULL)
		classes <- HDF5ReadData(hdfFile(x), "/all.data/colClasses")
		classes[,1]
	}
)

setMethod(
	f = "colClasses<-",
	signature = "BigDataFrame",
	definition = function(x, value){
		if(length(value) != ncol(x)) 
			stop("number of classes must match the number of rows")
		HDF5WriteData(hdfFile(x), "/all.data/colClasses", value, options=list(overwrite=TRUE))
		x
	}

)


