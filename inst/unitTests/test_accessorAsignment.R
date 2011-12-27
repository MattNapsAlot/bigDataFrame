
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

unitTestSingleCol <-
	function()
{
	dat <- data.frame(diag(100))
        x <- BigDataFrame(data=dat)

        x[,2] <- rep(2,100)
        dat[,2] <- 2
        checkTrue(all(x[1:100,1:100] == dat))

        x[,2] <- 3
        dat[,2] <- 3
        checkTrue(all(x[1:100,1:100] == dat))

}

unitTestTwoDimensionalBlock <-
	function()
{

}

unitTestOutOfBounds <-
	function()
{

}





