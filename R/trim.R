#' Trims leading and trailing whitespaces
#'
#' This function trims leading and trailing whitespaces
#' @param x string to trim
#' @keywords trim leading trailing whitespaces
#' @export
#' @examples
#' df <- trim(" String with leading and trailing whitespaces. ")
#' String with leading and trailing whitespaces.
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
