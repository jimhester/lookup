library(gh)

lookup <- function(x, visible = FALSE, ...) {
  if (!is.character(x)) {
    name <- as.character(substitute(x))
  }

  switch(typeof(x),
         closure = print_function(name, x, visible, ...),
         special =,
         builtin = print_compiled(name, x, "primitive", visible, ...))
}

print_compiled <- function(name, def, type, visible, ...) {
  map <- names_map()
  source_name <- map[name]

  lapply(r_source_definition(source_name),
    function(x) {
      x$name <- name
      x$type <- type
      x
    })
}

print_function <- function(name, def, visible, ...) {
  if (is_internal(def)) {
    body <- body(def)
    return(print_compiled(name = as.character(body[[length(body)]][[2]][[1]]),
      def = def, type = "internal", visible = visible, ...))
  }
  if (isS4(x)) {
    return(print_s4(x, ...))
  } else if (is_S3_generic(def)) {
    return(print_s3_generic(name, visible = visible))
  } else {
    print(getAnywhere(name), visible = visible)
  }
}

`%==%` <- function(x, y) {
  identical(x, as.name(y))
}

is_internal <- function(x) {
  body <- body(x)
  body[[length(body)]][[1]] %==% ".Internal"
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

package_github_content <- memoise::memoise(function(package, path, branch = "master") {
  readLines(paste(sep = "/", "https://raw.githubusercontent.com/cran", package, branch, path))
})

r_github_content <- memoise::memoise(function(path, branch = "master") {
  readLines(paste(sep = "/", "https://raw.githubusercontent.com/wch/r-source", branch, path))
})

names_map <- function(branch = "master") {
  x <- r_github_content("src/main/names.c", branch = branch)
  m <- regexpr("^\\{\"([^\"]+)\",[[:space:]]*([^,]+)", x, perl = TRUE)
  res <- captures(x, m)
  res <- res[m != -1, ]
  setNames(res[[2]], res[[1]])
}

captures <- function(x, m) {
  starts <- attr(m, "capture.start")
  res <- substring(x, starts, starts + attr(m, "capture.length") - 1L)
  data.frame(matrix(res, ncol = NCOL(starts)), stringsAsFactors = FALSE)
}

gh <- memoise::memoise(gh::gh)

parse_source <- function(name, lines) {
  start <- grep(paste0("[[:space:]]+", name, "\\([^)(]+)[[:space:]]*"), lines)
  if (length(start)) {
    ends <- grep("^}[[:space:]]*$", lines)
    end <- ends[ends > start][1]
    content <- paste0(collapse = "\n", lines[seq(start, end)])
    Compiled(path = path, start = start, end = end, content = content)
  }
}

r_source_definition <- function(x) {
  response <- gh("/search/code", q = paste("in:file", "repo:wch/r-source", "path:src/main", "language:c", x))
  paths <- vapply(response$items, `[[`, character(1), "path")
  compact(lapply(paths, function(path) {
    parse_source(x, r_github_content(path))
  }))
}

package_source_definition <- function(package, x) {
  response <- gh("/search/code", q = paste("in:file", paste0("repo:cran/", package), "path:src/", "language:c", "language:c++", x))
  paths <- vapply(response$items, `[[`, character(1), "path")
  compact(lapply(paths, function(path) {
    parse_source(x, package_github_content(package, path))
  }))
}

compact <- function(x) {
  is_empty <- vapply(x, function(x) length(x) == 0, logical(1))
  x[!is_empty]
}

Compiled <- function(path, start, end, content, name = "", type = "") {
   structure(
     list(
       path = path,
       start = start,
       end = end,
       content = content,
       name = name,
       type = type),
     class = "compiled")
}

print.compiled <- function(x, ...) {
  cat(crayon::bold(paste0(upper(x$type), " function ", x$name, ": ", x$path, "#L", x$start, "-L", x$end)),
    x$content, sep = "\n")
}

upper <- function(x) {
   gsub("\\b(.)", "\\U\\1", x, perl = TRUE)
}
