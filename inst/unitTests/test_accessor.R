.setUp <-
	function()
{
}

.tearDown <-
	function()
{
}

unitTestAccessor <-
	function()
{
	dd <- data.frame(diag(10), stringsAsFactors=FALSE)
	df <- BigDataFrame(data=dd)
	
	checkTrue(all(df[1:2, 1:2] == dd[1:2, 1:2]))

}


