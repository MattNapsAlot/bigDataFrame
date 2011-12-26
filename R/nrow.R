setMethod(
        f = "nrow",
        signature = "BigDataFrame",
        definition = function(x){
                return(as.numeric(HDF5ReadData(hdfFile(x), "/all.data/nrow")))
        }
)
