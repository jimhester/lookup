#' @importFrom memoise memoise
#' @importFrom utils flush.console
#' @importFrom httr with_config config
#' @importFrom withr with_envvar
#' @importFrom crayon cyan strip_style
NULL

p <- function(..., .collapse = NULL, .sep = "") {
  args <- list(...)
  named <- has_names(args)

  if (any(named)) {
    # Interpret $ as in perl / shell
    for (i in which(named)) {
      args[!named] <- lapply(args[!named], gsub, pattern = regex_escape(paste0("$", names(args)[[i]])), replacement = backslash_escape(args[[i]]))
    }
    args <- args[!named]
  }
  paste(args, collapse = .collapse, sep = .sep)
}

is_named <- function(x) {
  all(has_names(x))
}
has_names <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !(is.na(nms) | nms == "")
  }
}

bq <- function(x) {
  paste0("`", x, "`")
}

backslash_escape <- function(x) {
  gsub("\\\\", "\\\\\\\\", x, fixed = FALSE)
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
print.compiled <- function(x, ..., highlight = crayon::has_color()) {
  language <- x$language
  if (language == "c++") {
    language <- "c"
  }
  heading <- paste0("// ", bold(x$language, "source:", paste0(x$path, "#L", x$start, "-L", x$end)))
  body <- x$content
  if (isTRUE(highlight)) {
    body <- highlight_string(body, language = language)
  }
  if (rstudioapi::hasFun("navigateToFile")) {
    view_str(paste0(heading, "\n", body), paste0(x$name, ".", x$language))
  } else {
    cat(heading, body, sep = "\n")
  }
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
  args[-1] <- lapply(args[-1], gsub, pattern = "^[/\\]", replacement = "")
  args[-length(args)] <- lapply(args[-length(args)], gsub, pattern = "[/\\]$", replacement = "")
  do.call(file.path, args[nzchar(args)])
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

method_dialog <- function(funs, res, type) {
  names(res) <- funs
  if (length(res) > 1) {
    alphabetically <- order(funs)

    funs <- funs[alphabetically]
    res <- res[alphabetically]

    nums <- as.character(seq_along(funs))
    width_nums <- max(nchar(nums))
    cat(multicol(paste0(cyan(sprintf(paste0("%", width_nums, "s"), nums)), "| ", funs)), sep = "")
    ans <- get_answer(paste0(bold("Which", type, "method(s)?"), " (1-", length(funs), ", [A]ll): "), c(seq_along(funs), "A"), "A")

    if (ans != "A") {
      res <- res[as.integer(ans)]
    }
  }

  res
}

# From gaborcsardi/crayon/R/utils.r
multicol <- function(x) {
  xs <- strip_style(x)
  max_len <- max(nchar(xs))
  to_add <- max_len - nchar(xs) + 1
  x <- paste0(x, substring(paste0(collapse = "", rep(" ", max_len + 2)), 1, to_add))
  screen_width <- getOption("width")
  num_cols <- trunc(screen_width / max_len)
  num_rows <- ceiling(length(x) / num_cols)
  x <- c(x, rep("", num_cols * num_rows - length(x)))
  xm <- matrix(x, ncol = num_cols, byrow = TRUE)
  paste0(apply(xm, 1, paste, collapse = ""), "\n")
}

# From gaborcsardi/prettycode/R/print.R, gaborcsardi/prettycode/R/utils.R
page_str <- function(str) {
    cat(str, sep = "\n", file = tmp <- tempfile())
    on.exit(unlink(tmp))
    with_envvar(
      c("LESS" = "-R", action = "prefix"),
      file.show(tmp)
    )
}

# view file in RStudio
view_str <- function(str, name) {
  cat(str, sep = "\n", file = tmp <- file.path(tempdir(), name))
  #on.exit(unlink(tmp))
  rstudioapi::callFun("navigateToFile", tmp)
}

should_page <- function(src) {
  is_interactive() && is_terminal() && length(src) > num_lines()
}

num_lines <- function() {
  tryCatch(
    as.numeric(system("tput lines", intern = TRUE)),
    error = function(e) NA_integer_
  )
}

is_interactive <- function() interactive()

is_terminal <- function() {
  isatty(stdin()) &&
    Sys.getenv("RSTUDIO") != 1 &&
    Sys.getenv("R_GUI_APP_VERSION") == "" &&
    .Platform$GUI != "Rgui" &&
    !identical(getOption("STERM"), "iESS") &&
    Sys.getenv("EMACS") != "t"
}

should_page <- function(src) {
  is_interactive() && is_terminal() && length(src) > num_lines()
}

bioc_branch <- function() {
  if (BiocInstaller::isDevel()) {
    "master"
  } else {
    paste0("release-", BiocInstaller::biocVersion())
  }
}
