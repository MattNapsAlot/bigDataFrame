setMethod(
	f = "show",
	signature = "BigDataFrame",
	definition = function(object){
		cat(sprintf("An object of class %s\n", class(object)))
		if(all(dim(object) == c(0,0))) {
			cat("empty BigDataFrame\n")
			return(NULL)
		}
		cat("Only the first 5 rows and 10 columns are shown\n")
		dd <- object[1:min(5, nrow(object)), 1:min(10,ncol(object))]
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
		
		if(!all(dim(value) == c(length(i),length(j)))) stop("replacement did not match dimensions of data to be replaced")
		if(any(i < 0) || any(j < 0)) stop("negative indicex are not yet supported")	
		if(any(j > ncol(x))) stop("index out of bounds: column expansion not yet supported")	
		## write the rows
		iParts <- bigDataFrame:::.findContigs(i)
		for(ii in 1:length(iParts)){
			
			## look for assignment rows that already exist
			mk <- iParts[[ii]] %in% 1:nrow(x)
			if(any(mk)){	
				rows <- x[iParts[[ii]][mk],]
			}else{
				rows <- x[0,]
			}
	
			## find any gaps in the assignment rows
			gapIndices <- c()
			if(min(iParts[[ii]]) > (nrow(x) + 1))
				gapIndices <- (nrow(x) + 1):min(iParts[[ii]] - 1)


			## modify the values
			if(is.null(dim(value))){
				rows[1:length(iParts[[ii]]),j] <- value
			}else{
				rows[1:length(iParts[[ii]]),j] <- value[1:length(iParts[[ii]]),]
			}

			## fill the gaps
			if(length(gapIndices) > 0){
				gapFill <- x[0,]
				gapFill[1:length(gapIndices),] <- NA
				rows <- rbind(gapFill, rows)
				iParts[[ii]] <- c(gapIndices, iParts[[ii]])
			}
			
			if(!is.null(dim(rows)) && all(dim(rows) == dim(x)) && all(i %in% 1:nrow(x))){
				## write back the modified rows
				HDF5WriteData(hdfFile(x), "/all.data/dataValues", as.matrix(rows), options=list(overwrite=TRUE))	
			}else{
				HDF5WriteData(hdfFile(x), "/all.data/dataValues", as.matrix(rows), options=list(startindex=(iParts[[ii]][1] - 1), nrows=length(iParts[[ii]]), overwrite=TRUE))
			}
			
			if(any(!mk)){
				## update the number of rows and columns
				nrow(x) <- max(nrow(x), iParts[[ii]])
				
				## update the row names
				rownames(x)[iParts[[ii]]] <- iParts[[ii]]	
			}
		}
		
		mk <- j > ncol(x)
		if(any(mk)){	
                	## update the number of columns and column names
			ncol(x) <- max(ncol(x), j)
			colnames(x)[j[mk]] <- paste("V", j[mk], sep="")
			
			## update the column classes
			for(jj in j[mk]){
				colClasses(x)[jj] <- storage.mode(value[jj])
			}
		}
		x
	}
)

setMethod(
	f = "[",
	signature = "BigDataFrame",
	definition = function(x, i, j, ...){
		####
		## fill in missing index args
		####
		missingJ <- FALSE
		if(missing(i))
			i <- 1:nrow(x)
		if(missing(j)){
			missingJ <- TRUE
			j <- 1:ncol(x)
		}		

		####
		## Get the data as a matrix
		####
		if(length(i) == 1 && i == 0L){
			## Accessing the "zeroth" row
			dd <- matrix(nrow=0, ncol=ncol(x))
		}else if((length(unique(i)) == nrow(x)) && (length(unique(j)) == ncol(x))){
			## Accessing the entire data set
			dd <- HDF5ReadData(hdfFile(x), "/all.data/dataValues")
			if(length(i) == 1)
				dd <- dd[i,j]
		}else{
			## taking a slice
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
		}

	
		####
		## Convert matrix to a data frame
		####
		if(length(i) == 1 && i != 0){
			dd <- data.frame(t(data.frame(dd, stringsAsFactors=FALSE)), stringsAsFactors=FALSE)
		}else{
			dd <- data.frame(dd, stringsAsFactors=FALSE)
		}
		
		if(nrow(dd) > 0)
			rownames(dd) <- rownames(x)[i]
                if(ncol(dd) > 0)
			names(dd) <- names(x)[j]
		

		####
		## Set the column classes
		####
		classes <- colClasses(x)[j]
		level.vals <- levels(x)[j]

		for(jj in 1:ncol(dd)){
                	if(classes[jj]=="factor"){
                        	if(is.null(level.vals[[jj]])){
                                	dd[,jj] <- factor(dd[,jj])
                                }else{
                                	dd[,jj] <- factor(dd[,jj], levels=levels(x)[[jj]])
                                }
                        }else{
                        	storage.mode(dd[,jj]) <- classes[jj]
                        }
		}

		####
		## return the matrix
		####
		if(!missingJ && length(j) == 1)
			dd <- dd[,1]
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
		
		## set the levels for factors
		factorLevels <- lapply(data,levels)
		names(factorLevels) <- NULL
		
		levels(df) <- factorLevels

		df
	}
)
