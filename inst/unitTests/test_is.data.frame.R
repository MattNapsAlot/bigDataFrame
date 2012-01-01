unitTestIsDataFrame <-
	function()
{
	dd <- data.frame(diag(10))
	x <- BigDataFrame(data=dd)
	checkTrue(is.data.frame(x))
}
