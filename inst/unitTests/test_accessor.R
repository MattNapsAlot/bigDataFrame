unitTestAccessor <-
	function()
{
	dd <- data.frame(diag(10), stringsAsFactors=FALSE)
	df <- BigDataFrame(data=dd)
	
	checkTrue(all(df[1:2, 1:2] == dd[1:2, 1:2]))

}

unitTestAllDataNoIndices <-
	function()
{
	dd <- data.frame(diag(10), stringsAsFactors=FALSE)
        df <- BigDataFrame(data=dd)

        checkTrue(all(df[] == dd))
}

unitTestAllData <-
	function()
{
	dd <- data.frame(diag(10), stringsAsFactors=FALSE)
        df <- BigDataFrame(data=dd)

        checkTrue(all(df[1:nrow(df), 1:ncol(df)] == dd))
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

unitTestAllRowsSingle <-
	function()
{
	dd <- data.frame(x=1,y=c('a'))
	df <- BigDataFrame(data=dd)
	checkTrue(all(dd[1,] == df[1,]))
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

	checkTrue(all(dd[1:10, 1:10] == df[1:10, 1:10]))
}


unitTestSingleRowCol <-
	function()
{
	dd <- data.frame(diag(100), stringsAsFactors=FALSE)
        names(dd) <- paste("col", 1:ncol(dd), sep="")

        df <- BigDataFrame(data=dd)


        checkTrue(all(dd[2,2] == df[2,2]))
}


####
## Disabled this test until an efficient way to transform column classes is implemented
## M.Furia 2-Jan-2011
####
if(FALSE){
unitTestColClasses <-
	function()
{
	dd <- data.frame(numeric=seq(1,2,by=0.22), integer=rep(2L,5), character=c("a","b","c","d","e"), factor=c("one","two","one","one","two"), stringsAsFactors=FALSE)
	dd$factor <- as.factor(dd$factor)
	
	x <- BigDataFrame(data=dd)
	dd2 <- as.data.frame(x)
	
	checkTrue(all(names(lapply(dd, storage.mode)) == names(lapply(dd2, storage.mode))))	
	checkTrue(all(as.character(lapply(dd, storage.mode)) == as.character(lapply(dd2, storage.mode))))

	checkTrue(all(dd[-1]==dd2[-1]))
	checkTrue(all(abs(dd[1] - dd2[1]) <= .Machine$double.eps))
}
}
unitTestSingleCol <-
	function()
{
	dd <- data.frame(aColName = c("one", "two", "one", "three", "one"))
	x <- BigDataFrame(data=dd)
	checkTrue(all(dd[,1] == x[,1]))
}

unitTestZeroRow <-
	function()
{
	dd <- data.frame(aColName = c("one", "two", "one", "three", "one"))
        x <- BigDataFrame(data=dd)
        checkTrue(all(dim(x[0,]) == dim(dd[0,])))
}

unitTestOneColumnSelectAll <-
	function()
{
	dd <- data.frame(aColName = c("one", "two", "one", "three", "one"))
        x <- BigDataFrame(data=dd)
	checkTrue(!is.null(dim(x[])))
	checkTrue(all(dim(x[]) == dim(dd)))
}
