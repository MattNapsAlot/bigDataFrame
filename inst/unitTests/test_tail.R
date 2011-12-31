initTestAllRows <-
       function()
{
        dd <- data.frame(diag(6L))
        x <- BigDataFrame(data=dd)
        checkEquals(nrow(tail(x)), nrow(tail(dd)))
        checkEquals(ncol(tail(x)), ncol(tail(dd)))
        checkTrue(all(tail(x) == tail(dd)))
}

unitTestTail <-
        function()
{
        dd <- data.frame(diag(6L))
        x <- BigDataFrame(data=dd)
        checkEquals(nrow(tail(x, 3)), nrow(tail(dd, 3)))
        checkEquals(ncol(tail(x, 3)), ncol(tail(dd, 3)))
        checkTrue(all(tail(x, 3) == tail(dd, 3)))

}

unitTestTooManyRows <-
        function()
{
        dd <- data.frame(diag(5L))
        x <- BigDataFrame(data=dd)
        checkEquals(nrow(tail(x)), nrow(tail(dd)))
        checkEquals(ncol(tail(x)), ncol(tail(dd)))
        checkTrue(all(tail(x) == tail(dd)))
}

unitTestSpecifyRows <-
        function()
{
        dd <- data.frame(diag(6L))
        x <- BigDataFrame(data=dd)
        checkEquals(nrow(tail(x, 7)), nrow(tail(dd, 7)))
        checkEquals(ncol(tail(x, 7)), ncol(tail(dd, 7)))
        checkTrue(all(tail(x, 7) == tail(dd, 7)))

        checkEquals(nrow(tail(x, 3)), nrow(tail(dd, 3)))
        checkEquals(ncol(tail(x, 3)), ncol(tail(dd, 3)))
        checkTrue(all(tail(x, 3) == tail(dd, 3)))
}
