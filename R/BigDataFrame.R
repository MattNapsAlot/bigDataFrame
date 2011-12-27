setMethod(
	f = "BigDataFrame",
	signature = signature("character", "missing"),
	definition = function(hdf5FilePath){
		new(Class="BigDataFrame", hdfFile=hdf5FilePath)
	}
)

setMethod(
	f = "BigDataFrame",
	signature = signature("missing", "missing"),
	definition = function(){
		df <- new(Class="BigDataFrame", hdfFile=tempfile(fileext=".h5"))
		
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
		
		nrow(df) <- nrow(data)
                ncol(df) <- ncol(data)
		
		colnames(df) <- colnames(data)
		rownames(df) <- rownames(data)
		
		colClasses(df) <- as.character(lapply(data,function(x){class(x[1])}))
			
		HDF5WriteData(hdfFile(df), "/all.data/dataValues", data)
		##df[1:nrow(data), 1:ncol(data)] <- data 
		
		df
	}
)
