setMethod(
        f = "names",
        signature = "BigDataFrame",
        definition = function(x){
		colnames(x)
        }
)

setMethod(
        f = "names<-",
        signature = "BigDataFrame",
        definition = function(x, value){
        	colnames(x) <- value
	}
)

setMethod(
        f = "colnames",
        signature = "BigDataFrame",
        definition = function(x){
                HDF5ReadData(hdfFile(x), "/all.data/colNames")[,1]
        }
)

setMethod(
        f = "colnames<-",
        signature = "BigDataFrame",
        definition = function(x, value){
                HDF5WriteData(hdfFile(x), "/all.dat/colNames")
        }
)

