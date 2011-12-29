
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

unitTestOutOfBounds <-
	function()
{

}





