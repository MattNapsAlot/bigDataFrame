
unitTestFetchDimnames <-
	function()
{
	dd <- data.frame(diag(3))
	x <- BigDataFrame(data=dd)
	
	checkEquals(length(dimnames(x)), length(dimnames(dd)))
	checkTrue(all(dimnames(dd)[[1]] == dimnames(x)[[1]]))
	checkTrue(all(dimnames(dd)[[2]] == dimnames(x)[[2]]))
}

unitTestAssignDimnames <-
	function()
{
	dd <- data.frame(diag(3))
        x <- BigDataFrame(data=dd)

	nms <- list()
	nms[[1]] <- 3:5
	nms[[2]] <- c("row1", "row2", "row3")
	
	dimnames(dd) <- nms
	dimnames(x) <- nms

        checkEquals(length(dimnames(x)), length(dimnames(dd)))
        checkTrue(all(dimnames(dd)[[1]] == dimnames(x)[[1]]))
        checkTrue(all(dimnames(dd)[[2]] == dimnames(x)[[2]]))
}



