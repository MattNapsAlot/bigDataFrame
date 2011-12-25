.setUp <-
	function()
{
	rHDF5::HDF5WriteData(file.path(tempdir(), "colNames.h5"), 'all.data/colNames', c("col1", "col2", "col3"))
}

.tearDown <-
	function()
{
	if(file.exists(file.path(tempdir(), "myFile.h5")))
		file.remove(file.path(tempdir(), "myFile.h5"))
	if(file.exists(file.path(tempdir(), "colNames.h5")))
		file.remove(file.path(tempdir(), "colNames.h5"))
}

unitTestNames <-
	function()
{
	df <- BigDataFrame(file.path(tempdir(), "colNames.h5"))
	checkTrue(all(names(df) == c("col1", "col2", "col3")))
	checkTrue(all(colnames(df) == c("col1", "col2", "col3")))
}
