.setUp <-
	function()
{

}


.tearDown <-
	function()
{

}


unitTestDataFrameConstructor <-
	function()
{
	data <- data.frame(diag(10))
	df <- BigDataFrame(data=data)
	checkTrue(all(rownames(data) == rownames(df)))
	checkTrue(all(colnames(data) == colnames(df)))

	checkEquals(nrow(data), nrow(df))
	checkEquals(ncol(data), ncol(df))

}
