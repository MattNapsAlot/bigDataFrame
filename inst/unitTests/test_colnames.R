.setUp <-
	function()
{
	rHDF5::HDF5WriteData(file.path(tempdir(), "colNames.h5"), 'all.data/colNames', c("col1", "col2", "col3"))
	rHDF5::HDF5WriteData(file.path(tempdir(), "colNames.h5"), 'all.data/ncol', 3)
}

.tearDown <-
	function()
{
	if(file.exists(file.path(tempdir(), "myFile.h5")))
		file.remove(file.path(tempdir(), "myFile.h5"))
	if(file.exists(file.path(tempdir(), "colNames.h5")))
		file.remove(file.path(tempdir(), "colNames.h5"))
}

uniTestNullColNames <-
	function()
{
	df <- BigDataFrame()
	checkEquals(colnames(df), NULL)
}

unitTestNullNames <-
	function()
{
	df <- BigDataFrame()
        checkEquals(names(df), NULL)

}


unitTestNames <-
	function()
{
	df <- BigDataFrame(file.path(tempdir(), "colNames.h5"))
	checkTrue(all(names(df) == c("col1", "col2", "col3")))
	checkTrue(all(colnames(df) == c("col1", "col2", "col3")))
}

unitTestSetNames <-
        function()
{
        df <- BigDataFrame(file.path(tempdir(), "colNames.h5"))
        names(df) <- 1:3
	checkTrue(all((names(df) == 1:3)))

	checkException(names(df) <- 1:4)
	checkException(names(df) <- 1:2)
}

unitTestSetColNames <-
        function()
{
        df <- BigDataFrame(file.path(tempdir(), "colNames.h5"))
        colnames(df) <- 1:3
        checkTrue(all(colnames(df) == 1:3))

        checkException(colnames(df) <- 1:4)
	checkException(colnames(df) <- 1:2)
}


