#This file implements generic specific functions
#modified: March 2021
#author:   J. Kuckartz - jkuckartz1984@hotmail.com


#' Copy to Windows clipboard
#'
#' Copy content (for example a data.table) to the Windows clipboard so it becomes
#' available for paste-actions in Windows applications.
#' @param x Content to copy.
#' @return Nothing (but copies \code{x} to clipboard).
#' @family data.table functions
#' @export
copytoclipboard <- function(x) {
  # original from https://www.johndcook.com/blog/r_excel_clipboard/
  # modified to make clipboard 131 Mb
  # modified to handle lists
  if (typeof(x)=="list") {
    x=sapply(x, "length<-", max(lengths(x)))
  }
  write.table(x, paste0("clipboard-", 2^17), sep="\t", row.names=FALSE)
}
