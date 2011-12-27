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

unitTestRowSlice <-
	function()
{
	dd <- data.frame(diag(100), stringsAsFactors=FALSE)
        names(dd) <- paste("col", 1:ncol(dd), sep="")
	
	df <- BigDataFrame(data=dd)

	indx <- c(2:10, 11, 13:20, 1, 99)
		
	checkTrue(all(dd[indx,] == df[indx,]))
	checkTrue(all(names(dd[indx,]) == names(df[indx,])))
	checkTrue(all(rownames(dd[indx,]) == rownames(df[indx,])))
}

unitTestSingleRow <-
	function()
{
	dd <- data.frame(diag(100), stringsAsFactors=FALSE)
        names(dd) <- paste("col", 1:ncol(dd), sep="")

        df <- BigDataFrame(data=dd)

        indx <- 10

        checkTrue(all(dd[indx,] == df[indx,]))
        checkTrue(all(names(dd[indx,]) == names(df[indx,])))
        checkTrue(all(rownames(dd[indx,]) == rownames(df[indx,])))

}

unitTestAllRows <-
	function()
{
	dd <- data.frame(diag(100), stringsAsFactors=FALSE)
        names(dd) <- paste("col", 1:ncol(dd), sep="")

        df <- BigDataFrame(data=dd)

        indx <- 1:nrow(dd)

        checkTrue(all(dd[indx,] == df[indx,]))
        checkTrue(all(names(dd[indx,]) == names(df[indx,])))
        checkTrue(all(rownames(dd[indx,]) == rownames(df[indx,])))
}

unitTestColSlice <-
	function()
{
	dd <- data.frame(diag(100), stringsAsFactors=FALSE)
        names(dd) <- paste("col", 1:ncol(dd), sep="")

        df <- BigDataFrame(data=dd)

        indx <- c(2:10, 11, 13:20, 1, 99)

        checkTrue(all(dd[,indx] == df[,indx]))
        checkTrue(all(names(dd[,indx]) == names(df[,indx])))
        checkTrue(all(rownames(dd[,indx]) == rownames(df[,indx])))
}

unitTestSingleCol <-
	function()
{
	dd <- data.frame(diag(100), stringsAsFactors=FALSE)
        names(dd) <- paste("col", 1:ncol(dd), sep="")

        df <- BigDataFrame(data=dd)

        indx <- 10

        checkTrue(all(dd[,indx] == df[,indx]))
        checkTrue(all(names(dd[,indx]) == names(df[,indx])))
        checkTrue(all(rownames(dd[,indx]) == rownames(df[,indx])))

}

unitTestAllCols <-
	function()
{
	dd <- data.frame(diag(100), stringsAsFactors=FALSE)
        names(dd) <- paste("col", 1:ncol(dd), sep="")

        df <- BigDataFrame(data=dd)

        indx <- 1:nrow(dd)

        checkTrue(all(dd[,indx] == df[,indx]))
        checkTrue(all(names(dd[,indx]) == names(df[,indx])))
        checkTrue(all(rownames(dd[,indx]) == rownames(df[,indx])))
}


unitTestRowColSlice <-
	function()
{
	dd <- data.frame(diag(100), stringsAsFactors=FALSE)
        names(dd) <- paste("col", 1:ncol(dd), sep="")

        df <- BigDataFrame(data=dd)

        indx <- c(2:10, 11, 13:20, 1, 99)

        checkTrue(all(dd[,indx] == df[,indx]))
        checkTrue(all(names(dd[,indx]) == names(df[,indx])))
        checkTrue(all(rownames(dd[,indx]) == rownames(df[,indx])))
	
}


unitTestSingleRowCol <-
	function()
{
	dd <- data.frame(diag(100), stringsAsFactors=FALSE)
        names(dd) <- paste("col", 1:ncol(dd), sep="")

        df <- BigDataFrame(data=dd)


        checkTrue(all(dd[2,2] == df[2,2]))
}



