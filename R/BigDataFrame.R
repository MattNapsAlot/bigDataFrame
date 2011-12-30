setMethod(
	f = "show",
	signature = "BigDataFrame",
	definition = function(object){
		cat(sprintf("An object of class %s\n", class(object)))
		cat("Only the first 5 rows and 10 columns are shown\n")
		dd <- object:q[1:min(5, nrow(object)), 1:min(10,ncol(object))]
		show(dd)
	}
)

setMethod(
	f = "[<-",
	signature = "BigDataFrame",
	definition = function(x, i, j, value){
		if(missing(i))
                        i <- 1:nrow(x)
                if(missing(j))
                        j <- 1:ncol(x)		
		
		if(!all(dim(value) == c(i,j))) stop("replacement did not match dimensions of data to be replaced")
                if(nrow(x) < i || ncol(x) < j) stop("index out of bounds")
		
		## read in the rows
		iParts <- bigDataFrame:::.findContigs(i)
		for(ii in 1:length(iParts)){
			rows <- x[iParts[[ii]],]
			
			## modify the values
			if(is.null(dim(value))){
				rows[j] <- value
			}else{
				rows[, j] <- value[iParts[[ii]],]
			}
			
			if(!is.null(dim(rows)) && all(dim(rows) == dim(x))){
				## write back the modified rows
				HDF5WriteData(hdfFile(x), "/all.data/dataValues", as.matrix(rows), options=list(overwrite=TRUE))	
			}else{
				HDF5WriteData(hdfFile(x), "/all.data/dataValues", as.matrix(rows), options=list(startindex=(iParts[[ii]][1] - 1), nrows=length(iParts[[ii]]), overwrite=TRUE))
			}
		} 
		x
	}
)

setMethod(
	f = "[",
	signature = "BigDataFrame",
	definition = function(x, i, j, ...){
		if(missing(i))
			i <- 1:nrow(x)
		if(missing(j))
			j <- 1:ncol(x)
		
		if((length(unique(i)) == nrow(x)) && (length(unique(j)) == ncol(x))){
			dd <- data.frame(HDF5ReadData(hdfFile(x), "/all.data/dataValues"), stringsAsFactors=FALSE)
			rownames(dd) <- rownames(x)[i]
                        names(dd) <- names(x)[j]
			classes <- colClasses(x)[j]
                        for(ii in 1:ncol(dd)){
				this.class <- classes[ii]
				if(this.class=="factor"){
					if(is.null(levels(x))){
						dd[,ii] <- factor(dd[,ii]) 	
					}else{
						dd[,ii] <- factor(dd[,ii], levels=levels(x)[ii])
					}
				}else{
					storage.mode(dd[,ii]) <- classes[ii]
				}
			}
			return(dd[i,j])
		}
		dd <- NULL
		iParts <- .findContigs(i)
		for(ii in 1:length(iParts)){
			tmp <- HDF5ReadData(hdfFile(x), "/all.data/dataValues", options=list(startindex=(iParts[[ii]][1] - 1), nrows=length(iParts[[ii]])))
			if(is.null(dd)){
				dd <- tmp[,j]
			}else{
				dd <- rbind(dd, tmp)[,j]
			}
		}
		if(length(i) > 1){
			dd <- data.frame(dd, stringsAsFactors=FALSE)
			rownames(dd) <- rownames(x)[i]
			names(dd) <- names(x)[j]
			classes <- colClasses(x)[j]
			lapply(1:ncol(dd), function(i){storage.mode(dd[,i]) <- classes[i]})
		}else{
			storage.mode(dd) <- colClasses(x)[i]
		}
		dd[i,j]
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
