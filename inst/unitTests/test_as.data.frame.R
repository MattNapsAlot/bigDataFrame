unitTestDataFrame <-
	function()
{
	dd <- data.frame(diag(10))
	df <- BigDataFrame(data=dd)
	xx <- as.data.frame(df)
	checkTrue(all(xx == dd))
	checkTrue(all(names(xx) == names(dd)))
	checkTrue(all(rownames(xx) == rownames(dd)))
}
