#' Function irt.ccc
#' 
#' Calculates Category Characteristic Curve.
#' @export


irt.ccc <- function (diff = c(-2, 2, 3), x = seq(-4, 4, length.out = 200)) 
{
    diff = c(0, diff)
    m1 = length(diff)
    res = list()
    for (x1 in 1:length(diff)) {
        s2 = sum(diff[1:x1])
        sum1 = 0
        for (s1 in 1:m1) {
            s3 = sum(diff[1:s1])
            sum1 = sum1 + exp(s1 * x - s3)
        }
        s3 = sum(diff[1:x1])
        res[[x1]] = exp(x1 * x - s2)/(sum1)
    }
    return(res)
}
