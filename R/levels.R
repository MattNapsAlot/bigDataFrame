setMethod(
	f = "levels",
	signature = "BigDataFrame",
	definition = function(x){
		ll <- list()
		nms <- names(x)
		ss <- HDF5Summary(hdfFile(x))
		for(i in 1:length(nms)){
			if(sprintf("/all.data/levels/%s",nms[i]) %in% ss$datasetsummary){
				ll[[i]] <- HDF5ReadData(hdfFile(x), sprintf("/all.data/levels/%s",nms[i]))[,1]
			}else{
				ll[[i]] <- NULL
			}
		}
		if(length(ll) == 0L) ll <- NULL
		ll
	}
)

setMethod(
	f = "levels<-",
	signature = "BigDataFrame",
	definition = function(x, value){
		if(!is.list(value)) stop("levels must be a list")
		if(length(value) != length(names(x))) stop("provide a levels array for each colName")
		nms <- names(x)
		for(i in which(!sapply(value,is.null)))
			HDF5WriteData(hdfFile(x), sprintf("/all.data/levels/%s", nms[i]), value[[i]], options=(overwrite=TRUE))
		x
	}
)


