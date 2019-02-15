#' Function irt.icc
#' 
#' Calculates Item Characteristic Curve.
#' @export


irt.icc <- function (diff = 0, disc = 1, guess = 0, x = seq(-4, 4, length.out = 200)) 
{
    return(as.numeric(guess + (1 - guess) * (1/(1 + exp(-disc * 
        (x - diff))))))
}
