setGeneric(
        name = "colClasses<-",
def = function(x, value){
		standardGeneric("colClasses<-")
        }
)

setGeneric(
	name = "colClasses",
	def = function(x){
		standardGeneric("colClasses")
	}
)

setGeneric(
	name = "BigDataFrame",
	def = function(hdf5FilePath, data){
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
	name = "as.data.frame"
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

setGeneric(
        name = "nrow<-",
	def = function(x, value){
		standardGeneric("nrow<-")
	}
)

setGeneric(
        name = "ncol<-",
	def = function(x, value){
                standardGeneric("ncol<-")
        }
)

