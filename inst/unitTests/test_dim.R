.setUp <-
	function()
{
}

.tearDown <-
	function()
{
}


unitTestSetDims <-
	function()
{
	df <- new(Class="BigDataFrame", hdfFile=tempfile(fileext=".h5"))
	checkTrue(all(dim(df) == 0))

	checkEquals(length(dim(df)), 2)
	
	dim(df) <- c(0,0,0)
	checkTrue(all(dim(df) == c(0,0)))

	dim(df) <- c(1,1)
	checkTrue(all(dim(df) == c(0,0)))


        df <- new(Class="BigDataFrame", hdfFile=tempfile(fileext=".h5"))
	checkTrue(all(dim(df) == 0))
	
	dim(df) <- c(10,20)
	checkTrue(all(dim(df) == c(10,20)))

	dim(df) <- c(1,1)
	checkTrue(all(dim(df) == c(10,20))) 
}

