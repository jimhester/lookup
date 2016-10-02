#' @importFrom methods .S4methods getMethod isGeneric
#' @importFrom stats na.omit setNames
#' @importFrom utils .S3methods capture.output getS3method packageDescription
NULL

as_lookup <- function(x, envir = parent.frame(), ...) {
  UseMethod("as_lookup")
}

as_lookup.lookup <- function(x, envir = parent.frame(), ...) {
  x
}

as_lookup.character <- function(x, envir = parent.frame(), ...) {
  res <- list()
  res[c("package", "name")] <- parse_name(x)
  res$def <- get(res$name, envir = res$package %||% envir, mode = "function")
  res$package <- res$package %||% function_package(res$def)
  res$type <- typeof(res$def)
  res$visible <- is_visible(res$name)
  class(res) <- "lookup"
  res
}

function_package <- function(x) {
  if (is.primitive(x)) {
    return("base")
  }
  e <- environment(x)
  nme <- environmentName(e)
  while(!(nzchar(nme) || identical(e, baseenv()))) {
    e <- parent.env(e)
    nme <- environmentName(e)
  }
  if (!nzchar(nme)) {
    stop("Could not find associated package", call. = FALSE)
  }
  nme
}

as_lookup.function <- function(x, envir = parent.frame(), name = substitute(x)) {
  res <- list(def = x)

  res$package <- function_package(res$def)
  if (is.primitive(res$def)) {
    env <- baseenv()
  } else {
    env <- environment(res$def)
  }
  res$name <- Filter(function(xx) identical(x, get(xx, envir = env)), ls(env))
  res$type <- typeof(res$def)
  res$visible <- is_visible(res$name)
  class(res) <- "lookup"
  res
}

lookup <- function(x, name = substitute(x), envir = parent.frame(), all = FALSE, ...) {
  fun <- as_lookup(x, envir = envir, name = name)

  switch(fun$type,
         closure = lookup_closure(fun, envir = envir, all = all, ...),
         special =,
         builtin = lookup_internal(fun, envir = envir, all = all, ...),
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
  split <- strsplit(x, ":::?")[[1]]
  if (length(split) == 2) {
    list(package = split[[1]],
         name = split[[2]])
  } else {
    list(package = NULL,
         name = split[[1]])
  }
}

lookup_closure <- function(fun, envir = parent.frame(), all = FALSE, ...) {
  if (has_call(fun$def, ".Internal")) {
    fun$internal <- lookup_internal(list(name = call_name(fun$def, ".Internal")))
  }
  if (has_call(fun$def, ".Call")) {
    if (uses_rcpp(fun$package)) {
      fun$ccall <- lookup_rcpp(call_name(fun$def, ".Call"), fun$package)
    }
    if (is.null(fun$ccall)) {
      fun$ccall <- lookup_c_call(call_name(fun$def, ".Call"), fun$package)
    }
  }
  if (pryr::is_s3_method(fun$name)) {
    fun$type <- append(fun$type, "S3 method", 0)
  }
  if (pryr::is_s3_generic(fun$name)) {
    fun$type <- append(fun$type, "S3 generic", 0)
    fun$S3_methods <- lookup_S3_methods(fun, envir = envir, all = all)
  }
  if (isS4(fun$def)) {
    if (isGeneric(fun$name)) {
      fun$type <- append(fun$type, "S4 generic", 0)
      fun$S4_methods <- lookup_S4_methods(fun, envir = envir, all = all)
    } else {
      fun$type <- append(fun$type, "S4 method")
    }
  }
  fun
}

`%==%` <- function(x, y) {
  identical(x, as.name(y))
}

lookup_S3_methods <- function(f, envir = parent.frame(), all = FALSE, ...) {

  S3_methods <- .S3methods(f$name, envir = envir)

  classes <- sub(paste0(escape(f$name), "."), "", S3_methods)

  lapply(classes, function(class) lookup(getS3method(f$name, class), name = f$name))
}

lookup_S4_methods <- function(f, envir = parent.frame(), all = FALSE, ...) {

  S4_methods <- .S4methods(f$name)
  S4_methods_info <- attr(S4_methods, "info")

  signatures <- strsplit(sub(paste0("^", escape(f$name), ",", "(.*)-method$"), "\\1", S4_methods), ",")

  res <- Map(getMethod, f$name, signatures)
  names(res) <- signatures
  res
}

print.lookup <- function(x, envir = parent.frame(), ..., highlight = Sys.which("highlight")) {
  lookup <- if (x$visible) "::" else ":::"

  cat(crayon::bold(x$package, lookup, x$name, sep = ""), " [", paste(collapse = ", ", x$type), "]\n", sep = "")
  cat(highlight_output(base::print.function(x$def), highlight, "r"), sep = "\n")
  if (!is.null(x$internal)) {
    lapply(x$internal, print, envir = envir, highlight = highlight)
  }
  if (!is.null(x$ccall)) {
    lapply(x$ccall, print, envir = envir, highlight = highlight)
  }
  if (!is.null(x$S3_methods)) {
    lapply(x$S3_methods, print, envir = envir, highlight = highlight)
  }
  if (!is.null(x$S4_methods)) {
    lapply(x$S4_methods, print, envir = envir, highlight = highlight)
  }
  invisible(x)
}

escape <- function(x) {
  chars <- c("*", ".", "?", "^", "+", "$", "|", "(", ")", "[", "]", "{", "}", "\\")
  gsub(paste0("([\\", paste0(collapse = "\\", chars), "])"), "\\\\\\1", x, perl = TRUE)
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

attached_functions <- memoise::memoise(function(sp = search()) {
  fnames <- lapply(seq_along(sp), ls)
  data.frame(name = unlist(fnames),
    package = rep.int(sub("package:", "", sp, fixed = TRUE), lengths(fnames)),
    stringsAsFactors = FALSE)
})

is_visible <- function(x) {
  funs <- attached_functions()
  any(funs$name == x)
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

r_github_content <- memoise::memoise(function(path, branch = "master") {
  readLines(paste(sep = "/", "https://raw.githubusercontent.com/wch/r-source", branch, path))
})

names_map <- function(x = r_github_content("src/main/names.c", branch = branch),
                      branch = "master") {
  m <- regexpr("^\\{\"([^\"]+)\",[[:space:]]*([^,]+)", x, perl = TRUE)
  res <- na.omit(captures(x, m))
  setNames(res[[2]], res[[1]])
}

gh <- memoise::memoise(gh::gh)

package_source_definition <- function(package, x) {
  response <- gh("/search/code", q = paste("in:file", paste0("repo:cran/", package), "path:src/", "language:c", "language:c++", x))
  paths <- vapply(response$items, `[[`, character(1), "path")
  regex <- rcpp_symbol_map_cran(x, package)[x]
  if (any(is.na(regex))) {
    return()
  }
  compact(lapply(paths, function(path) {
      if (!grepl("RcppExports\\.cpp", path)) {
        find_cpp_function(regex, package_github_content(package, path), path)
      }
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

print.compiled <- function(x, highlight = Sys.which("highlight"), ...) {
  cat(crayon::bold(x$type, "source:", paste0(x$path, "#L", x$start, "-L", x$end)),
    highlight_output(cat(x$content), highlight, x$type), sep = "\n")
}

upper <- function(x) {
   gsub("\\b(.)", "\\U\\1", x, perl = TRUE)
}

`%||%` <- function(x, y) if (is.null(x)) { y } else { x }

assert <- function(x, message = "") {
  if(!isTRUE(x)) {
    stop(message, call. = FALSE)
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
