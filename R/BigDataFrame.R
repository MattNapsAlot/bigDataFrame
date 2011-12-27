setMethod(
	f = "show",
	signature = "BigDataFrame",
	definition = function(object){
		cat(sprintf("An object of class %s\n", class(object)))
		cat("Only the first 5 rows and 10 columns are shown\n")

		col.cnt <- min(10L, ncol(object))
		row.cnt <- min(5L, nrow(object))
		dd <- data.frame(HDF5ReadData(hdfFile(object), "/all.data/dataValues")[1:row.cnt, 1:col.cnt], stringsAsFactors=FALSE, row.names=rownames(object)[1:row.cnt])
		names(dd) <- names(object)[1:col.cnt]

		classes <- colClasses(object)[1:col.cnt]
		lapply(1:col.cnt, function(i){storage.mode(dd[,i]) <- classes[i]})
		
		show(dd)
	}
)

setMethod(
	f = "[",
	signature = "BigDataFrame",
	definition = function(x, i, j, ...){
		dd <- data.frame(HDF5ReadData(hdfFile(x), "/all.data/dataValues"), stringsAsFactors=F)
		if(missing(i))
			i <- 1:nrow(x)
		if(missing(j))
			j <- 1:ncol(x)
		dd <- dd[i,j]
		names(dd) <- names(x)[i]
		rownames(dd) <- rownames(x)[j]
		classes <- colClasses(x)[j]
		lapply(j, function(i){storage.mode(dd[,i]) <- classes[i]})
		dd
	}
)

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
			
		HDF5WriteData(hdfFile(df), "/all.data/dataValues", as.matrix(data))
		##df[1:nrow(data), 1:ncol(data)] <- data 
			
		df
	}
)
