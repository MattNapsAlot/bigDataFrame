setGeneric(
	name = "BigDataFrame",
	def = function(hdf5FilePath){
		standardGeneric("BigDataFrame")
	}
)

setGeneric(
	name = 'hdfFile',
	def = function(x){
		standardGeneric("hdfFile")
	}
)

setGeneric(
	name = "names"
)

setGeneric(
	name = "rownames"
)

setGeneric(
	name = "colnames"
)

setGeneric(
	name = "dim"
)

setGeneric(
	name = "names<-"
)

setGeneric(
        name = "colnames<-"
)

setGeneric(
        name = "rownames<-"
)

setGeneric(
	name = "nrow"
)

setGeneric(
	name = "ncol"
)

