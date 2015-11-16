lookup <- function(x, envir = parent.frame(), all = FALSE, ...) {
  fun <- list()
  if (!is.character(x)) {
    nme <- substitute(x)
    if (nme == "x") {
      env <- if (is.primitive(x)) {
        fun$package <- "base"

      } else {
        fun$package <- environmentName(environment(x))
      }
      fun$name <- Filter(function(xx) identical(x, get(xx, envir = env)), ls(envir = env))
    } else {
    fun[c("package", "name")] <- parse_name(nme)
    }
    fun$def <- x
  } else {
    fun[c("package", "name")] <- parse_name(x)
    fun$def <- get(fun$name, envir = fun$package %||% envir, mode = "function")
  }
  fun$package <- fun$package %||% environmentName(environment(fun$def))
  fun$type <- typeof(fun$def)
  fun$visible <- is_visible(fun$name, envir = envir)
  class(fun) <- "lookup"

  switch(fun$type,
         closure = lookup_closure(fun, envir = envir, all = all, ...),
         special =,
         builtin = lookup_special(fun, envir = envir, all = all, ...),
         stop("Function of type: ", fun$type, " not supported!", call. = FALSE))
}

loaded_functions <- memoise::memoise(function(envs = loadedNamespaces()) {
  fnames <- lapply(envs,
    function(e) ls(envir = asNamespace(e), all.names = TRUE))
  data.frame(name = unlist(fnames),
    package = rep.int(envs, lengths(fnames)),
    stringsAsFactors = FALSE)
})

#print.function <- function(x, ...) print.lookup(lookup(x, ...))

parse_name <- function(x) {
  if (is.name(x)) {
    return(list(package = NULL, name = as.character(x)))
  }
  if (is.call(x) && (x[[1]] %==% "::" || x[[2]] %==% ":::")) {
    return(list(package = asNamespace(as.character(x[[2]])),
                name = as.character(x[[3]])))
  }
  if (is.character(x)) {
    split <- strsplit(x, ":::?")[[1]]
    res <- if (length(split) == 2) {
      list(package = split[[1]],
           name = split[[2]])
    } else {
      list(package = NULL,
           name = split[[1]])
    }
    return(res)
  }
  stop("Cannot handle input ", x, " of type: ", typeof(x), call. = FALSE)
}

lookup_special <- function(fun, envir = parent.frame(), ...) {
  map <- names_map()
  source_name <- map[fun$name]

  r_source_definition(source_name)
}

lookup_closure <- function(fun, envir = parent.frame(), all = FALSE, ...) {
  if (pryr:::is_internal(fun$def)) {
    fun$internal <- lookup_special(list(name = pryr:::internal_name(fun$def)))
  }
  if (pryr::is_s3_method(fun$name)) {
    fun$type <- append(fun$type, "S3 method", 0)
  }
  if (pryr::is_s3_generic(fun$name)) {
    fun$type <- append(fun$type, "S3 generic", 0)
    fun$S3_methods <- lookup_S3_methods(fun, envir = envir, all = all)
  }
  if (isS4(fun$def)) {
    fun$type <- append(fun$type, "S4 generic", 0)
    fun$S4_methods <- lookup_S4_methods(fun, envir = envir, all = all)
  }
  fun
}

`%==%` <- function(x, y) {
  identical(x, as.name(y))
}

lookup_S3_methods <- function(f, envir = parent.frame(), all = FALSE, ...) {

  S3_methods <- methods(f$name)
  S3_methods <- S3_methods[attr(S3_methods, "info")$isS4 == FALSE]

  funs <- loaded_functions()
  res <- funs[match(S3_methods, funs$name), ]
  Map(function(name, package) { lookup(name, asNamespace(package)) }, res$name, res$package)
}

print.lookup <- function(x, envir = parent.frame(), ..., highlight = Sys.which("highlight")) {
  lookup <- if (x$visible) "::" else ":::"

  cat(crayon::bold(x$package, lookup, x$name, sep = ""), " [", paste(collapse = ", ", x$type), "]\n", sep = "")
  cat(highlight_output(base::print.function(x$def), highlight, "r"), sep = "\n")
  if (!is.null(x$internal)) {
    lapply(x$internal, print, envir = envir, highlight = highlight)
  }
  if (!is.null(x$S3_methods)) {
    lapply(x$S3_methods, print, envir = envir, highlight = highlight)
  }
  invisible(x)
}



highlight_output <- function(code, path, type = "r") {
  if (nzchar(path)) {
    tmp <- tempfile()
    on.exit(unlink(tmp))
    capture.output(force(code), file = tmp)
    system2(path, args = c("-f", "-S", type, "-O", "ansi", tmp), stdout = TRUE)
  } else {
    capture.output(force(code))
  }
}

is_visible <- function(x, envir = parent.frame()) {
  tryCatch(is.function(get(x, envir = envir, mode = "function")),
           error = function(e) FALSE)
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

parse_source <- function(name, path) {
  lines <- r_github_content(path)
  start <- grep(paste0("SEXP[[:space:]]+attribute_hidden[[:space:]]+", name, "\\([^)(]+)[[:space:]]*"), lines)
  if (length(start)) {
    ends <- grep("^}[[:space:]]*/?.*$", lines)
    end <- ends[ends > start][1]
    content <- paste0(collapse = "\n", lines[seq(start, end)])
    Compiled(path = path, start = start, end = end, content = content, type = "c")
  }
}

r_source_definition <- function(x) {
  response <- gh("/search/code", q = paste("in:file", "repo:wch/r-source", "path:src/main", "language:c", x))
  paths <- vapply(response$items, `[[`, character(1), "path")
  compact(lapply(paths, function(path) {
    parse_source(x, path)
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
       type = type),
     class = "compiled")
}

print.compiled <- function(x, highlight = Sys.which("highlight"), type = "c", ...) {
  cat(crayon::bold(type, "source:", paste0(x$path, "#L", x$start, "-L", x$end)),
    highlight_output(cat(x$content), highlight, x$type), sep = "\n")
}

upper <- function(x) {
   gsub("\\b(.)", "\\U\\1", x, perl = TRUE)
}

`%||%` <- function(x, y) if (is.null(x)) { y } else { x }
