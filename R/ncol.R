setMethod(
        f = "ncol",
        signature = "BigDataFrame",
        definition = function(x){
                as.numeric(HDF5ReadData(hdfFile(x), "/all.data/ncol"))
        }
)
