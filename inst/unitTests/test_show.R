unitTestShow <-
	function()
{
	dd <- data.frame(diag(6L))
        x <- BigDataFrame(data=dd)
	show(x)
}
