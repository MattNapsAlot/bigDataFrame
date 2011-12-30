.setUp <-
	function()
{
	levels.vals <- list()
	levels.vals[[1]] <- c("character", "numeric", "integer")
	rHDF5::HDF5WriteData(file.path(tempdir(), "getLevels.h5"), "/all.data/levels/trait", as.matrix(levels.vals[[1]]), options=list(forceNewFile=TRUE))
	rHDF5::HDF5WriteData(file.path(tempdir(), "getLevels.h5"), "/all.data/colNames", "trait")
	rHDF5::HDF5WriteData(file.path(tempdir(), "setLevels.h5"), "/all.data/colNames", "trait", options=list(forceNewFile=TRUE))
}

.tearDown <-
	function()
{
	if(file.exists(file.path(tempfile(), "getLevels.h5")))
		file.remove(file.path(tempfile(), "getLevels.h5"))

	if(file.exists(file.path(tempfile(), "setLevels.h5")))
                file.remove(file.path(tempfile(), "setLevels.h5"))
}

unitTestGetLevels <-
	function()
{
	levels.vals <- list()
        levels.vals[[1]] <- c("character", "numeric", "integer")
	x <- BigDataFrame(hdf5FilePath=file.path(tempdir(), "getLevels.h5"))
	ll <- levels(x)
	checkEquals(length(ll), length(levels.vals))
}

unitTestNoLevels <-
	function()
{
	x <- BigDataFrame(hdf5FilePath=file.path(tempdir(), "setLevels.h5"))
	checkTrue(is.null(levels(x)[[1]]))
}

unitTestSetFactors <-
	function()
{
	x <- BigDataFrame(hdf5FilePath=file.path(tempdir(), "setLevels.h5"))
	levels.vals <- list()	
	levels.vals[[1]] <- c("one", "two")
	levels(x) <- levels.vals
	checkEquals(length(levels(x)), 1)
	checkTrue(all(levels(x)[[1]] == c("one", "two")))
}


