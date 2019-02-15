#' Function random.data
#' 
#' Generates random data based on the Rasch model.
#' @export


random.data <- function (simulatedPersons = 100, simulatedItems = 10, addMissings = 10) 
{
    ir = function(x, y) {
        return(as.numeric((1/(1 + exp(-(x - y)))) > runif(length(x))))
    }
    latVar = rnorm(simulatedPersons)
    latDif = rnorm(simulatedItems)
    d = numeric(length(latVar))
    for (i in latDif) d = data.frame(d, ir(latVar, i))
    d = d[, -1]
    d
    names(d) = paste0("item", c(1:length(latDif)))
    head(d)
    if (addMissings > 0) 
        d[1:addMissings, 1] = NA
    return(d)
}
