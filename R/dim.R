setMethod(
        f = "dim",
        signature = "BigDataFrame",
        definition = function(x){
                return(c(nRow(x), nCol(x)))
        }
)
