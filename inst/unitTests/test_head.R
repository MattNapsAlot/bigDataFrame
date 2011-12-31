

unitTestAllRows <-
	function()
{
	dd <- data.frame(diag(6L))
	x <- BigDataFrame(data=dd)
	checkEquals(nrow(head(x)), nrow(head(dd)))
	checkEquals(ncol(head(x)), ncol(head(dd)))
	checkTrue(all(head(x) == head(dd)))
}

unitTestHead <-
	function()
{
	dd <- data.frame(diag(6L))
        x <- BigDataFrame(data=dd)
        checkEquals(nrow(head(x, 3)), nrow(head(dd, 3)))
        checkEquals(ncol(head(x, 3)), ncol(head(dd, 3)))
        checkTrue(all(head(x, 3) == head(dd, 3)))

}

unitTestTooManyRows <-
	function()
{
        dd <- data.frame(diag(5L))
        x <- BigDataFrame(data=dd)
        checkEquals(nrow(head(x)), nrow(head(dd)))
        checkEquals(ncol(head(x)), ncol(head(dd)))
        checkTrue(all(head(x) == head(dd)))
}

unitTestSpecifyRows <-
	function()
{
	dd <- data.frame(diag(6L))
        x <- BigDataFrame(data=dd)
        checkEquals(nrow(head(x, 7)), nrow(head(dd, 7)))
        checkEquals(ncol(head(x, 7)), ncol(head(dd, 7)))
        checkTrue(all(head(x, 7) == head(dd, 7)))

	checkEquals(nrow(head(x, 3)), nrow(head(dd, 3)))
        checkEquals(ncol(head(x, 3)), ncol(head(dd, 3)))
        checkTrue(all(head(x, 3) == head(dd, 3)))
}
