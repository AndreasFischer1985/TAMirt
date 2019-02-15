#' Function plotTIC
#' 
#' Plots Test Information Curve based on a TAM model.
#' @export


plotTIC <- function (model, main = "IRT - Test Information Curve") 
plot(TAM::IRT.informationCurves(model), main = main)
