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

unitFactorLevels <-
	function()
{
	dd <- data.frame(aColName = c("one", "two", "one", "three", "one"))
	x <- BigDataFrame(data=dd)
	checkTrue(all(levels(x) == levels(dd[,1])))
}

unitFactorLevels2 <-
        function()
{
        dd <- data.frame(aColName = c("one", "two", "one", "three", "one"), stringsAsFactors=FALSE)
        dd[,1] <- factor(dd[,1], levels=c("one", "two", "three"))
	x <- BigDataFrame(data=dd)
        checkTrue(all(levels(x) == levels(dd[,1])))
}


