unitTestAsVector <-
	function()
{
	dd <- data.frame(diag(10))
	x <- BigDataFrame(data=dd)
	
	checkTrue(all(as.vector(dd) == as.vector(x)))
}
