#' Function plotQ3
#' 
#' Plots Matirx of absolute aQ3-values.
#' @export


plotQ3 <- function (model, cutoff = 0.3) 
{
    palette <- colorRampPalette(c(rgb(1, 1, 1), rgb(0/255, 84/255, 
        122/255)))(256)
    aQ3 = TAM::tam.modelfit(model, progress = F)$aQ3.matr
    dev.new()
    image(abs(aQ3), xaxt = "n", yaxt = "n", col = palette, zlim = c(0, 
        1))
    for (i in 1:2) axis(i, at = seq(0, 1, length.out = dim(aQ3)[2]), 
        seq(1, dim(aQ3)[2]), cex.axis = 0.5)
    s = seq(0, 1, length.out = dim(aQ3)[2])
    for (i1 in 1:dim(aQ3)[2]) for (i2 in 1:dim(aQ3)[2]) text(x = s[i1], 
        y = s[i2], round(aQ3[i1, i2], 2), cex = 0.5, col = c(rgb(0, 
            0, 0, 0), "red")[(abs(aQ3[i1, i2]) > cutoff) + 1])
    legend(0.8, 1.15, c("0", "1"), fill = palette[c(1, length(palette))], 
        xpd = NA)
    title("adjusted Q3-matrix")
}
