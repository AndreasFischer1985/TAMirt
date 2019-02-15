#' Function absolutelyWright
#' 
#' Plots Wright Map based on data.
#' @export


absolutelyWright <- function (dat1) 
{
    library(WrightMap)
    library(TAM)
    library(psych)
    TAM_mod1 = tam.mml(dat1, control = list(progress = F))
    TAM_mod1$xsi$xsi = TAM_mod1$xsi$xsi - mean(TAM_mod1$xsi$xsi)
    TAM_wle1 <- tam.wle(TAM_mod1, progress = F)
    TAM_abilities1 <- TAM_wle1$theta
    TAM_difficulties1 <- TAM_mod1$xsi$xsi
    names(TAM_difficulties1) = names(dat1)
    TAM_slopes1 <- TAM_mod1$B[, 2, ]
    abilities = data.frame(TAM_abilities1)
    difficulties = data.frame(TAM_difficulties1)
    wrightMap(abilities, difficulties, main.title = "Wright Map", 
        axis.persons = "Persons", axis.items = "Items", axis.logits = "Logits", 
        item.prop = 0.5, show.axis.logits = "R", item.side = itemClassic, 
        dim.names = c(paste("Theta ( Alpha =", round(alpha(apply(dat1, 
            c(1, 2), as.numeric))$total$raw_alpha, 2), ")")), 
        dim.color = c("#C6DBEF"))
    n = dim(dat1)[1]
    title(sub = substitute(paste(italic("Notes."), " Items are represented on the right as item number and score number (separated by a dot). Items are separated by vertical lines. N=", 
        n)), cex.sub = 0.7, adj = 0, xpd = T)
    return(list(abilities, difficulties))
}
