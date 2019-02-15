#' Function paste.table
#' 
#' Returns a table from the clipboard.
#' @export


paste.table <- function (head = F) 
{
    f <- file(description = "clipboard", open = "r")
    df <- read.table(f, sep = "\t", header = head)
    close(f)
    return(data.frame(df))
}
