setMethod(
	f = "rownames",
	signature = "BigDataFrame",
	definition = function(x){
		return(HDF5ReadData(hdfFile(x), "/all.data/rowNames")[,1])
	}
)

setMethod(
	f = "rownames<-",
	signature = "BigDataFrame",
	definition = function(x, value){
		HDF5WriteData(hdfFile(x), "/all.data/rowNames")
	}
)



