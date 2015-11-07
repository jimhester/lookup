library(gh)

lookup <- function(x, visible = FALSE, ...) {
  if (!is.character(x)) {
    name <- as.character(substitute(x))
  }

  switch(typeof(x),
         closure = print_function(name, x, visible, ...),
         special =,
         builtin = print_compiled(name, x, visible, ...))
}

print_compiled <- function(name, def, visible, ...) {
  map <- names_map()

  source_definition(map[name])
}

print_function <- function(name, def, visible, ...) {
  if (isS4(x)) {
    return(print_s4(x, ...))
  } else if (is_S3_generic(def)) {
    print_s3_generic(name, visible = visible)
  } else {
    print(getAnywhere(name), visible = visible)
  }
}

`%==%` <- function(x, y) {
  identical(x, as.name(y))
}

is_S3_generic <- function(x) {
  body <- body(x)
  body[[1]] %==% "UseMethod" ||
      (body[[1]] %==% "{" &&
       body[[2]] %==% "UseMethod")
}

print_s3_generic <- function(x, visible = TRUE, ...) {
  m <- attr(methods(x), "info")
  if (isTRUE(visible)) {
    m <- m[m$visible == TRUE, ]
  }
  print(lapply(rownames(m), getAnywhere))
}

print.getAnywhere <- function(x, ...) {
  package <- grepl("^package:", x$where)
  namespace <- grepl("^namespace:", x$where)
  defs <- x$objs[namespace]
  name <- if(any(package)) {
    paste0(sub("package:", "", x$where[package]), "::", x$name)
  } else if (any(namespace)) {
    paste0(sub("namespace:", "", x$where[namespace]), ":::", x$name)
  } else {
    x$name
  }

  cat(crayon::bold(name), "\n")
  lapply(defs, print)
  invisible()
}

github_content <- memoise::memoise(function(path, branch = "master") {
  readLines(paste(sep = "/", "https://raw.githubusercontent.com/wch/r-source", branch, path))
})

names_map <- function(branch = "master") {
  x <- github_content("src/main/names.c", branch = branch)
  m <- regexpr("^\\{\"([^\"]+)\",[[:space:]]+([^,]+)", x, perl = TRUE)
  res <- captures(x, m)
  res <- res[m != -1, ]
  res
  setNames(res$X2, res$X1)
}

captures <- function(x, m) {
  starts <- attr(m, "capture.start")
  res <- substring(x, starts, starts + attr(m, "capture.length") - 1L)
  data.frame(matrix(res, ncol = NCOL(starts)))
}

source_definition <- function(x) {
  search <- gh("/search/code", q = paste("in:file", "repo:wch/r-source", "path:src/main", "language:c", x))
  paths <- vapply(search$items, `[[`, character(1), "path")
  compact(lapply(paths, function(path) {
    lines <- github_content(path)
    start <- grep(paste0(x, "\\([^)(]+)[[:space:]]*"), lines)
    if (length(start)) {
      ends <- grep("^}[[:space:]]*$", lines)
      end <- ends[ends > start][1]
      content <- paste0(collapse = "\n", lines[seq(start, end)])
      Source(path = path, start = start, end = end, content = content)
    }
  }))
}

compact <- function(x) {
  is_empty <- vapply(x, function(x) length(x) == 0, logical(1))
  x[!is_empty]
}

Source <- function(path, start, end, content) {
   structure(
     list(
       path = path,
       start = start,
       end = end,
       content = content),
     class = "source")
}

print.source <- function(x, ...) {
  cat(crayon::bold(paste0(x$path, "#L", x$start, "-L", x$end)),
    x$content, sep = "\n")
}
