setMethod(
        f = "ncol",
        signature = "BigDataFrame",
        definition = function(x){
		if(!("/all.data/ncol" %in% HDF5Summary(hdfFile(x))$datasetsummary)) return(0L)
		as.numeric(HDF5ReadData(hdfFile(x), "/all.data/ncol"))
        }
)

setMethod(
	f = "ncol<-",
	signature = "BigDataFrame",
	definition = function(x, value){
		value <- as.numeric(value)
                if(length(value) != 1L) stop("provide a single value")
                if(any(is.na(value))) stop("No value provided")
                HDF5WriteData(hdfFile(x), "/all.data/ncol", value, options=list(overwrite = TRUE))
		x
	}
)

