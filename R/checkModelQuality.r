#' Function checkModelQuality
#' 
#' Returns information on model fit and psychomeric quality based on data.
#' @export


checkModelQuality <- function (data) 
{
    library(psych)
    a = suppressWarnings(alpha(data))
    print("Model Fit")
    ta = tam.mml(data, control = list(progress = F))
    mf = tam.modelfit(ta, progress = F)
    iff = tam.fit(ta, progress = F)
    l = c(length(data[1, ]), ta$EAP, tam.wle(ta, progress = F)$WLE.rel[1], 
        a$total$std.alpha, min(a$item.stats$r.cor), min(iff$itemfit$Infit_p), 
        min(iff$itemfit$Outfit_p), min(iff$itemfit$Infit_pholm), 
        min(iff$itemfit$Outfit_pholm), mf$statlist$pmaxX2, mf$stat.MADaQ3$p, 
        (-mf$fitstat[[1]]), (-mf$fitstat[[2]]), (-mf$fitstat[[3]]))
    names(l) = c("items", "EAPrel", "WLErel", "alpha", "minTrennschärfe", 
        "minInfit_p", "minOutfit_p", "minInfit_pholm", "minOutfit_pholm", 
        "pmaxX2", "MADaQ3$p", "MADCOV", "SRMR", "SRMSR")
    return(cbind(`Goodness of fit` = l))
}
