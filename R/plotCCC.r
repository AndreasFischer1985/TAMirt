#' Function plotCCC
#' 
#' Plots Category Characteristic Curves based on a named vector of item difficulties.
#' @export


plotCCC <- function (difficulties, diff.names = NULL, main = "Category Characteristic Curves", 
    labeling = T, lwd = 2, xlim = NULL, col = NULL, ...) 
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
        if (!is.null(diff.names)) 
            names(difficulties) = diff.names
        else stop("Please provide either diff.names or a named vector of difficulties")
    plot(c(min(difficulties), max(difficulties)), c(0, 1), type = "n", 
        xlab = "difficulty", ylab = "probability", xlim = xlim, 
        main = main)
    items = as.factor(gsub("_Cat[0-9]*$", "", names(difficulties)))
    if (is.null(col)) 
        col = rainbow(length(levels(items)))
    if (length(col) != length(levels(items))) 
        col = rep(col, length(levels(items)))
    x = seq(min(xlim) - 1, max(xlim) + 1, length.out = 200)
    for (i in 1:length(levels(items))) {
        diff = difficulties[which(items == levels(items)[i])]
        res = irt.ccc(diff = diff, x = x)
        for (j in 1:length(res)) lines(x, res[[j]], lwd = lwd, 
            col = col[i], ...)
    }
    if (labeling) 
        for (i in 1:length(difficulties)) text(round(difficulties[i], 
            digits = 1), 0.5, listNames(difficulties, i), srt = 90)
}
