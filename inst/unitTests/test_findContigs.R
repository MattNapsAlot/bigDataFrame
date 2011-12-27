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
	checkException(bigDataFrame:::.findContigs(c(1:5, 0)))
}

unitTestFloatingPoint <-
	function()
{
	## not implemented
}

