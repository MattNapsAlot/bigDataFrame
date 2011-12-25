setMethod(
        f = "ncol",
        signature = "BigDataFrame",
        definition = function(x){
                return(as.numeric(HDF5ReadData(hdfFile(x), "ncol")))
        }
)
