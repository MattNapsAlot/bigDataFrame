unitTestFindContigs <-
	function()
{
	parts <- bigDataFrame:::.findContigs(c(1:5, 7, 10:100))
	checkTrue(all(parts[[1]] == 1:5))
	checkEquals(parts[[2]], 7)
	checkTrue(all(parts[[3]] == 10:100))

	checkEquals(length(parts), 3)

	parts <- bigDataFrame:::.findContigs(c(1:5, 7, 10:100, 9))
        checkTrue(all(parts[[1]] == 1:5))
        checkEquals(parts[[2]], 7)
        checkTrue(all(parts[[3]] == 10:100))
	checkEquals(parts[[4]], 9)
}

unitTestNegInt <-
	function()
{
	checkException(bigDataFrame:::.findContigs(c(1:5, -1)))
}

unitTestZeroInt <-
	function()
{
	parts <- bigDataFrame:::.findContigs(c(1:5, 0))
	checkEquals(length(parts), 2)
	checkTrue(all(parts[[1]] == 1:5))
	checkEquals(parts[[2]], 0)
}

unitTestFloatingPoint <-
	function()
{
	## not implemented
}

unitTestOneContig <-
	function()
{
	indx <- 1:10
	parts <- bigDataFrame:::.findContigs(indx)
	checkEquals(length(parts), 1L)
	checkTrue(all(parts[[1]] == 1:10))
}
