#' Function checkItemQuality
#' 
#' Returns information on item fit based on data.
#' @export


checkItemQuality <- function (data, exclude = NULL, cutoffs = c(0.2, 2, 2, 0.3), 
    reduce = T) 
{
    library(TAM)
    names = seq(1:dim(data)[2])
    if (length(exclude) > 0) 
        names = names[-exclude]
    if (length(exclude) > 0) 
        data = data[, -exclude]
    ta = TAM::tam.mml(data, control = list(progress = F))
    mf = TAM::tam.modelfit(ta, progress = F)
    iff = TAM::tam.fit(ta, progress = F)
    d = NULL
    if (is.null(cutoffs)) 
        d = cbind((IRT.itemfit(ta)$RMSD[, 2]), iff$itemfit[["Infit"]], 
            iff$itemfit[["Outfit"]], (1 * apply(abs(mf$aQ3.matr), 
                1, mean, na.rm = T)))
    else d = cbind((IRT.itemfit(ta)$RMSD[, 2]) <= cutoffs[1], 
        iff$itemfit[["Infit"]] <= cutoffs[2], iff$itemfit[["Outfit"]] <= 
            cutoffs[3], (1 * apply(abs(mf$aQ3.matr), 1, mean, 
            na.rm = T)) <= cutoffs[4])
    colnames(d) = c("Infit", "Outfit", "RMSD", "MaQ3")
    rownames(d) = names
    if (reduce) 
        return(rowSums(d) == dim(d)[2])
    else return(d)
}
