regex_escape <- function(x) {
  chars <- c("*", ".", "?", "^", "+", "$", "|", "(", ")", "[", "]", "{", "}", "\\")
  gsub(paste0("([\\", paste0(collapse = "\\", chars), "])"), "\\\\\\1", x, perl = TRUE)
}

uses_rcpp <- function(pkg) {
  grepl("\\bRcpp\\b", perl = TRUE, packageDescription(pkg)$LinkingTo)
}
