#' Function plotICC
#' 
#' Plots Item Characteristic Curves based on a vector of item difficulties.
#' @export


plotICC <- function (difficulties, slopes = seq(1:length(difficulties)) * 
    0 + 1, main = "Item Characteristic Curves", labeling = T, 
    xlim = NULL, lwd = 2, col = NULL) 
{
    listNames <- function(diff, i) {
        if (is.null(names(diff))) 
            names(diff) = 1:length(diff)
        return(paste(names(diff[(round(diff, 1)) == (round(diff[i], 
            1))]), sep = " ", collapse = ", "))
    }
    if (is.null(xlim)) 
        xlim = c(min(difficulties) - 1, max(difficulties) + 1)
    if (!is.null(dim(difficulties))) {
        rn = rownames(difficulties)
        difficulties = difficulties[, 1]
        names(difficulties) = rn
    }
    if (is.null(names(difficulties))) 
        names(difficulties) = 1:length(difficulties)
    plot(c(min(difficulties), max(difficulties)), c(0, 1), type = "n", 
        xlab = "difficulty", ylab = "probability", xlim = xlim, 
        main = main)
    color.no = ((difficulties - min(difficulties))/(max(difficulties) - 
        min(difficulties))) * 100 + 1
    if (is.null(col)) 
        col = rainbow(101)
    if (length(col) != 101) 
        col = rep(col[1], 101)
    x = seq(min(xlim) - 1, max(xlim) + 1, length.out = 200)
    for (i in 1:length(difficulties)) {
        lines(x = x, y = irt.icc(difficulties[i], slopes[i], 
            x = x), col = col[color.no[i]], lwd = lwd)
        if (labeling) 
            text(round(difficulties[i], digits = 1), 0.5, listNames(difficulties, 
                i), srt = 90)
    }
}
