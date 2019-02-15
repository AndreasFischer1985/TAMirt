#' Function checkItemQuality
#' 
#' Returns information on item fit based on data.
#' @export


checkItemQuality <- function (data, deleted = NULL) 
{
    names = seq(1:dim(data)[2])
    if (length(deleted) > 0) 
        names = names[-deleted]
    if (length(deleted) > 0) 
        data = data[, -deleted]
    ta = tam.mml(data, control = list(progress = F))
    mf = tam.modelfit(ta, progress = F)
    iff = tam.fit(ta, progress = F)
    d = cbind(names, (apply(data.frame(iff$itemfit[["Infit_p"]], 
        iff$itemfit[["Outfit_p"]]), 1, min)), (apply(data.frame(iff$itemfit[["Infit_pholm"]], 
        iff$itemfit[["Outfit_pholm"]]), 1, min)), (-1 * IRT.itemfit(ta)$RMSD[, 
        2]), (-1 * apply(abs(mf$aQ3.matr), 1, mean, na.rm = T)), 
        (mf$chisquare.itemfit$p.holm), -iff$itemfit[["Infit"]], 
        -iff$itemfit[["Outfit"]])
    colnames(d) = c("Nummer", "Fit_p", "Fit_pholm", "-RMSD", 
        "-MaQ3", "X2_ph", "-infit", "-outfit")
    return(d)
}
