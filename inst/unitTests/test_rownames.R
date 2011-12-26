.setUp <-
        function()
{
        rHDF5::HDF5WriteData(file.path(tempdir(), "rowNames.h5"), 'all.data/rowNames', c("row1", "row2", "row3"))
	rHDF5::HDF5WriteData(file.path(tempdir(), "rowNames.h5"), 'all.data/nrow', 3)
}

.tearDown <-
        function()
{
        if(file.exists(file.path(tempdir(), "myFile.h5")))
                file.remove(file.path(tempdir(), "myFile.h5"))
        if(file.exists(file.path(tempdir(), "rowNames.h5")))
                file.remove(file.path(tempdir(), "rowNames.h5"))
}

unitTestNames <-
        function()
{
        df <- BigDataFrame(file.path(tempdir(), "rowNames.h5"))
        checkTrue(all(rownames(df) == c("row1", "row2", "row3")))
}

unitTestSetNames <-
	function()
{
	df <- BigDataFrame(file.path(tempdir(), "rowNames.h5"))
	rownames(df) <- 1:3
        checkTrue(all(rownames(df) == 1:3))
	checkException(rownames(df) <- 1:4)
	checkException(rownames(df) <- 1:2)
}

