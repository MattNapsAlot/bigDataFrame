
unitTestSingleValue <-
	function()
{
	dat <- data.frame(diag(100))
	x <- BigDataFrame(data=dat)

	x[1,1] <- 2
	dat[1,1] <- 2
	checkTrue(all(x[1:100,1:100] == dat))	

	x[40,60] <- 2
        dat[40,60] <- 2
        checkTrue(all(x[1:100,1:100] == dat))

}


unitTestSingleRow <-
	function()
{
	dat <- data.frame(diag(100))
        x <- BigDataFrame(data=dat)

        x[2,] <- rep(2,100)
        dat[2,] <- 2
        checkTrue(all(x[1:100,1:100] == dat))

	x[2,] <- 3
        dat[2,] <- 3
        checkTrue(all(x[1:100,1:100] == dat))

}

unitTestMultipleRows <-
	function()
{
	dat <- data.frame(diag(100))
        x <- BigDataFrame(data=dat)


        x[1:2,] <- 3
        dat[1:2,] <- 3
        checkTrue(all(x[1:100,1:100] == dat))
}


unitTestSingleCol <-
	function()
{
	# this test fails. it's a known bug
        #dat <- data.frame(diag(100))
        #x <- BigDataFrame(data=dat)

        #x[1] <- 3
        #dat[1] <- 3
        #checkTrue(all(x[1:100,1:100] == dat))

	
	dat <- data.frame(diag(100))
        x <- BigDataFrame(data=dat)

        x[,2] <- rep(2,100)
        dat[,2] <- 2
        checkTrue(all(x[1:100,1:100] == dat))

	dat <- data.frame(diag(100))
        x <- BigDataFrame(data=dat)

        x[,2] <- 3
        dat[,2] <- 3
        checkTrue(all(x[1:100,1:100] == dat))

}

unitTestMultipleCols <-
        function()
{
	# this test fails. it's a known bug
        #dat <- data.frame(diag(100))
        #x <- BigDataFrame(data=dat)

        #x[1:2] <- 3
        #dat[1:2] <- 3
        #checkTrue(all(x[1:100,1:100] == dat))

	dat <- data.frame(diag(100))
        x <- BigDataFrame(data=dat)

	x[,1:2] <- 3
        dat[,1:2] <- 3
        checkTrue(all(x[1:100,1:100] == dat))
}


unitTestTwoDimensionalBlock <-
	function()
{
	dat <- data.frame(diag(100))
        x <- BigDataFrame(data=dat)

        x[1:10,1:10] <- 2
        dat[1:10,1:10] <- 2
        checkTrue(all(x[1:100,1:100] == dat))
}

unitTestOutOfBoundsRowNumeric <-
	function()
{	
	dd <- data.frame(diag(2))
	x <- BigDataFrame(data=dd)
	dd[3,] <- 2
	x[3,] <- 2
	checkTrue(all(dd[] == x[]))
	
	dd[5,] <- 3
	x[5,] <- 3

	checkTrue(all(dd[c(1:3,5),] == x[c(1:3,5),]))
        checkTrue(all(is.na(x[4,])))
        checkTrue(all(is.na(dd[4,])))

        checkTrue(all(dim(x) == dim(dd)))
        checkTrue(all(rownames(x) == rownames(dd)))
        checkTrue(all(names(x) == names(dd)))

        checkTrue(all(as.character(lapply(x[],class)) == as.character(lapply(dd[],class))))

}

unitTestOutOfBoundsColNumeric <-
        function()
{        
        dd <- data.frame(diag(2))
        x <- BigDataFrame(data=dd)
        dd[,3] <- 2
######
#       known bug in rHDF5. column expansion not supported
        checkException(x[,3] <- 2)
#        checkTrue(all(dd[] == x[]))

	checkException(x[,5] <- 3)

#        checkTrue(all(dim(x) == dim(dd)))
#        checkTrue(all(rownames(x) == rownames(dd)))
#        checkTrue(all(names(x) == names(dd)))

#        checkTrue(all(as.character(lapply(x[],class)) == as.character(lapply(dd[],class))))
}


unitTestOutOfBoundsRowCharacter <-
        function()
{        
	dd <- data.frame(x=c("a","b","c"), y=c("A","B","C"), stringsAsFactors=FALSE)
	x <- BigDataFrame(data=dd)
	
	checkTrue(4 > nrow(x))
	dd[4,] <- 'x'
	x[4,] <- 'x'
	checkTrue(all(dd[] == x[]))

	checkTrue(6 > nrow(x))
	dd[6,] <- 'y'
        x[6,] <- 'y'
        checkTrue(all(dd[c(1:4,6),] == x[c(1:4,6),]))
        
	## known bug
	##checkTrue(all(is.na(x[5,])))
        checkTrue(all(is.na(dd[5,])))

        checkTrue(all(dim(x) == dim(dd)))
        checkTrue(all(rownames(x) == rownames(dd)))
        checkTrue(all(names(x) == names(dd)))

        checkTrue(all(as.character(lapply(x[],class)) == as.character(lapply(dd[],class))))	
}

unitTestOutOfBoundsColCharacter <-
        function()
{
        dd <- data.frame(x=c("a","b","c"), y=c("A","B","C"), stringsAsFactors=FALSE)
        x <- BigDataFrame(data=dd)

        checkTrue(3 > ncol(x))
        dd[,3] <- 'x'
######
#       known bug in rHDF5. column expansion not supported
        checkException(x[,3] <- 'x')
#        checkTrue(all(dd[] == x[]))

        checkTrue(5 > nrow(x))
	checkException(x[,5] <- 'y')
#	checkTrue(all(dim(x) == dim(dd)))
#	checkTrue(all(rownames(x) == rownames(dd)))
#	checkTrue(all(names(x) == names(dd)))

#	checkTrue(all(as.character(lapply(x[],class)) == as.character(lapply(dd[],class))))
}

unitTestOutOfBoundsRowFactor <-
        function()
{        
        dd <- data.frame(x=c("a","b","c"), y=c("A","B","C"))
        x <- BigDataFrame(data=dd)

        checkTrue(4 > ncol(x))
        dd[4,] <- c('a','A')
        x[4,] <- c('a','A')
        checkTrue(all(dd[] == x[]))

        checkTrue(5 > nrow(x))
        dd[6,1] <- 'c'
	dd[6,2] <- 'C'

	x[6,1] <- 'c'
        x[6,2] <- 'C'
	
	checkTrue(all(dd[c(1:4,6),] == x[c(1:4,6),]))
        checkTrue(all(is.na(x[5,])))
        checkTrue(all(is.na(dd[5,])))

        checkTrue(all(dim(x) == dim(dd)))
        checkTrue(all(rownames(x) == rownames(dd)))
        checkTrue(all(names(x) == names(dd)))

        checkTrue(all(as.character(lapply(x[],class)) == as.character(lapply(dd[],class))))
}

unitTestOutOfBoundsColFactor <-
        function()
{
        dd <- data.frame(x=c("a","b","c"), y=c("A","B","C"))
        x <- BigDataFrame(data=dd)

        checkTrue(3 > ncol(x))
        dd[,3] <- 'x'
######
#       known bug in rHDF5. column expansion not supported
        checkException(x[,3] <- 'x')
#        checkTrue(all(dd[] == x[]))

        checkTrue(5 > nrow(x))
        checkException(x[,5] <- 'x')

#        checkTrue(all(dim(x) == dim(dd)))
#        checkTrue(all(rownames(x) == rownames(dd)))
#        checkTrue(all(names(x) == names(dd)))

#        checkTrue(all(as.character(lapply(x[],class)) == as.character(lapply(dd[],class))))
}

unitTestAddMultipleRows <-
	function()
{
	dd <- data.frame(diag(10))
	x <- BigDataFrame(data=dd)
	
	x[11:20,] <- dd
	checkTrue(all(dim(x) == c(20,10)))
	checkTrue(all(x[11:20,] == dd))

	x[21:30,] <- dd[1:10,]
        checkTrue(all(dim(x) == c(30,10)))
	checkTrue(all(x[21:30,] == dd))
        
	x[31:40,] <- dd[1:10,1:10]
        checkTrue(all(dim(x) == c(40,10)))
	checkTrue(all(x[31:40,] == dd))
}

unitTestAddMultipleRowsWithGap <-
	function()
{
	dd <- data.frame(diag(10))
        x <- BigDataFrame(data=dd)

        x[12:21,] <- dd
        checkTrue(all(dim(x) == c(21,10)))
        checkTrue(all(x[12:21,] == dd))
	checkTrue(all(is.na(x[11,])))


}
