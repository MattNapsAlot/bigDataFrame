.setUp <-
	function()
{
	rHDF5::HDF5WriteData(file.path(tempdir(), "colClasses.h5"), "/all.data/colClasses", c("numeric", "character"))
	rHDF5::HDF5WriteData(file.path(tempdir(), "colClasses.h5"), "/all.data/ncol", 2)
}


.tearDown <-
	function()
{
	if(file.exists(file.path(tempdir(), "colClasses.h5")))
		file.remove(file.path(tempdir(), "colClasses.h5"))
}

unitTestColClasses <-
	function()
{
	df <- new(Class="BigDataFrame", hdfFile=file.path(tempdir(), "colClasses.h5"))
	checkTrue(all(colClasses(df) == c("numeric", "character")))

}

unitTestBadNumClasses <-
	function()
{
	df <- new(Class="BigDataFrame", hdfFile=file.path(tempdir(), "colClasses.h5"))
	checkException(colClasses(df) <- c("foo","bar", "boo"))
	checkException(colClasses(df) <- "foo")
}

unitTestSetColClasses <-
	function()
{
	df <- new(Class="BigDataFrame", hdfFile=file.path(tempdir(), "colClasses.h5"))
	colClasses(df) <- c('character', 'numeric')
	checkTrue(all(colClasses(df) == c('character', 'numeric')))
}

unitTestDataFrameColClasses <-
	function()
{
	dd <- data.frame(x=1:3, y=c('a','b','c'), stringsAsFactors = FALSE)
	df <- BigDataFrame(data=dd)
	checkTrue(all(as.character(lapply(dd,function(x){class(x[1])})) == colClasses(df)))
}

unitTestFactorSingleRowSingleCol <-
	function()
{
	dd <- data.frame(y=c('a'))
        df <- BigDataFrame(data=dd)
        checkTrue(all(as.character(lapply(dd,function(x){class(x[1])})) == colClasses(df)))
}

unitTestFactorSingleRowTwoCol <-
        function()
{
        dd <- data.frame(x=1,y=c('a'))
        df <- BigDataFrame(data=dd)
        checkTrue(all(as.character(lapply(dd,function(x){class(x[1])})) == colClasses(df)))
	checkEquals(storage.mode(dd[1,1]), storage.mode(df[1,1]))
	checkEquals(storage.mode(dd[1,2]), storage.mode(df[1,2]))
}
