regex_escape <- function(x) {
  chars <- c("*", ".", "?", "^", "+", "$", "|", "(", ")", "[", "]", "{", "}", "\\")
  gsub(paste0("([\\", paste0(collapse = "\\", chars), "])"), "\\\\\\1", x, perl = TRUE)
}

uses_rcpp <- function(pkg) {
  grepl("\\bRcpp\\b", perl = TRUE, packageDescription(pkg)$LinkingTo)
}

upper <- function(x) {
   gsub("\\b(.)", "\\U\\1", x, perl = TRUE)
}

assert <- function(x, message = "") {
  if(!isTRUE(x)) {
    stop(message, call. = FALSE)
  }
}

`%||%` <- function(x, y) if (is.null(x) || (is.character(x) && !nzchar(x))) y else x
`%==%` <- function(x, y) identical(x, y)

# convert a specific warning to an error
as.error <- function(e) {
  class(e) <- c("simpleError", "error", "condition")
  e
}

compact <- function(x) {
  is_empty <- vapply(x, function(x) length(x) == 0, logical(1))
  x[!is_empty]
}

Compiled <- function(...) {
   structure(
     list(...),
     class = "compiled")
}

print.compiled <- function(x, ...) {
  language <- x$language
  if (language == "c++") {
    language <- "c"
  }
  cat(crayon::bold(x$language, "source:", paste0(x$path, "#L", x$start, "-L", x$end)),
    highlite::highlight_string(x$content, language = language), sep = "\n")
}

captures <- function(x, m) {
  assert(is.character(x), "'x' must be a character vector")
  assert(class(m) == "integer" &&
    identical(names(attributes(m)), c("match.length", "useBytes", "capture.start", "capture.length", "capture.names")),
    "'m' must be the result of 'regexpr()' with 'perl = TRUE'")

  starts <- attr(m, "capture.start")
  strings <- substring(x, starts, starts + attr(m, "capture.length") - 1L)
  res <- data.frame(matrix(strings, ncol = NCOL(starts)), stringsAsFactors = FALSE)
  colnames(res) <- auto_name(attr(m, "capture.names"))
  res[m == -1, ] <- NA_character_
  res
}

auto_name <- function(names) {
  missing <- names == ""
  if (all(!missing)) {
    return(names)
  }
  names[missing] <- seq_along(names)[missing]
  names
}

gh <- memoise::memoise(gh::gh)

paths <- function(...) {
  args <- list(...)
  args[-1] <- gsub("^[/\\]", "", args[-1])
  args[-length(args)] <- gsub("[/\\]$", "", args[-length(args)])
  paste(args[nzchar(args)], collapse = "/")
}
