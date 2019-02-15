#' Function copy.table
#' 
#' Copies a table to the clipboard.
#' @export


copy.table <- function (obj, size = 4096) 
{
    clip <- paste("clipboard-", size, sep = "")
    f <- file(description = clip, open = "w")
    write.table(obj, f, row.names = FALSE, sep = "\t")
    close(f)
}
