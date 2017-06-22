#' Trims leading and trailing whitespaces
#'
#' This function trims leading and trailing whitespaces
#' @param x string to trim
#' @keywords trim leading trailing whitespaces
#' @importFrom magrittr %>%
#' @export
#' @examples
#' df <- trim(" text containing leading and trailing whitespaces. ")
#' #text containing leading and trailing whitespaces.
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
