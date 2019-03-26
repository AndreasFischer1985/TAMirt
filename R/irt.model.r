#' Function irt.model
#' 
#' Returns IRT-model based on binary response data.
#' @export


irt.model <- function (data, control = list(progress = F), reduce = T, ...) 
{
    if (reduce) 
        if (length(which(apply(dat, 2, var) == 0)) > 0) 
            data = data[, -which(apply(dat, 2, var) == 0)]
    model = TAM::tam.mml(data, control = control, ...)
    model$xsi$xsi = model$xsi$xsi - mean(model$xsi$xsi)
    attr(model, "thetas") = TAM::tam.wle(model, progress = F)$theta
    attr(model, "etas") = model$xsi$xsi
    attr(model, "slopes") = model$B[, 2, ]
    return(model)
}
