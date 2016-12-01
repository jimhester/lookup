#' @importFrom memoise memoise
#' @importFrom utils flush.console
#' @importFrom httr with_config config
NULL

bq <- function(x) {
  paste0("`", x, "`")
}

regex_escape <- function(x) {
  chars <- c("*", ".", "?", "^", "+", "$", "|", "(", ")", "[", "]", "{", "}", "\\")
  gsub(paste0("([\\", paste0(collapse = "\\", chars), "])"), "\\\\\\1", x, perl = TRUE)
}

uses_rcpp <- function(pkg) {
  grepl("\\bRcpp\\b", perl = TRUE, tryCatch(packageDescription(pkg)$LinkingTo, warning = function(e) NULL) %||% FALSE)
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

#' @export
print.compiled <- function(x, ...) {
  language <- x$language
  if (language == "c++") {
    language <- "c"
  }
  cat(bold(x$language, "source:", paste0(x$path, "#L", x$start, "-L", x$end)),
    highlight_string(x$content, language = language), sep = "\n")
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

spin <- local({
  spinner <- c("-", "/", "|", "\\")
  i <- 0
  time <- NA
  function() {
    if (is.na(time)) {
      time <<- Sys.time()
    }
    if (Sys.time() - time > .1) {
      cat("\r", spinner[i + 1], sep = "")
      i <<- (i + 1) %% 4
      time <<- Sys.time()
    }
  }
})

progress <- function(down, up) {
  if (interactive()) {
    spin()
  }
  TRUE
}

gh <- memoise(function(...) {
  on.exit({
    flush.console()
    cat("\r")
  })
  with_config(config(noprogress = FALSE, progressfunction = progress), {
    gh::gh(...)
  })
})

paths <- function(...) {
  args <- compact(list(...))
  args[-1] <- gsub("^[/\\]", "", args[-1])
  args[-length(args)] <- gsub("[/\\]$", "", args[-length(args)])
  paste(args[nzchar(args)], collapse = "/")
}

# any function using unlist or c was dropping the classnames,
# so need to brute force copy the objects
flatten_list <- function(x, class) {

  res <- list()
  itr <- 1L
  assign_item <- function(x) {
    if (inherits(x, class)) {
      res[[itr]] <<- x
      itr <<- itr + 1L
    }
    else if (is.list(x)) {
      lapply(x, assign_item)
    }
  }
  assign_item(x)
  res

}

# adapted from BiocInstlaler:::.getAnswer
get_answer <- function(msg, allowed, default) {
  if (!interactive()) {
    return(default)
  }
  repeat {
    cat(msg)
    answer <- readLines(n = 1)
    if (answer %in% allowed)
      return(answer)
  }
}

msg <- function(x, ..., width = getOption("width"), nl = TRUE) {
  txt <- strwrap(x, width = width, exdent = 2)
  cat(paste(txt, collapse = "\n"), if (isTRUE(nl)) "\n")
}

# From gaborcsardi/crayon/utils.r
multicol <- function(x) {
  max_len <- max(nchar(x))
  to_add <- max_len - nchar(x) + 1
  x <- paste0(x, substring(paste0(collapse = "", rep(" ", max_len + 2)), 1, to_add))
  screen_width <- getOption("width")
  num_cols <- trunc(screen_width / max_len)
  num_rows <- ceiling(length(x) / num_cols)
  x <- c(x, rep("", num_cols * num_rows - length(x)))
  xm <- matrix(x, ncol = num_cols, byrow = TRUE)
  paste0(apply(xm, 1, paste, collapse = ""), "\n")
}
