#' Function compare.models
#' 
#' Compares model fit of different IRT models.
#' @export


compare.models <- function (dat1, mirt = F) 
{
    library(TAM)
    comp = matrix(nrow = 3, ncol = 2 + as.numeric(mirt))
    rownames(comp) = c("logLik", "AIC", "BIC")
    d = apply(dat1, c(1, 2), as.numeric)
    m1 = tam.mml(d, control = list(progress = F))
    comp[1, 1] = logLik(m1)
    comp[2, 1] = AIC(m1)
    comp[3, 1] = BIC(m1)
    m2 = tam.mml.2pl(d, control = list(progress = F))
    comp[1, 2] = logLik(m2)
    comp[2, 2] = AIC(m2)
    comp[3, 2] = BIC(m2)
    if (mirt) {
        library(mirt)
        m3 = mirt(d, 2)
        comp[1, 3] = logLik(m2)
        comp[2, 2] = AIC(m2)
        comp[3, 2] = BIC(m2)
        colnames(comp) = c("1PL", "2PL", "mirt")
    }
    else colnames(comp) = c("1PL", "2PL")
    return(comp)
}
