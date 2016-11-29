#' @importFrom methods .S4methods getMethod isGeneric
#' @importFrom stats na.omit setNames
#' @importFrom utils .S3methods capture.output getS3method packageDescription
#' @useDynLib lookup
#' @importFrom Rcpp sourceCpp
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

as_lookup.getAnywhere <- function(x, ...) {

  # getAnywhere can return multiple definitions with the same name in different namespaces
  lapply(which(!x$dups), function(idx) {
    package <- sub("^registered S3 method for \\w+ from namespace (\\w+)", "\\1", x$where[[idx]])
    package <- sub("^namespace:", "", package)
    package <- sub("^package:", "", package)
    def <- x$objs[[idx]]
    structure(
      list(
        package = package,
        name = x$name,
        def = def,
        type = typeof(def),
        visible = x$visible[[idx]]),
      class = "lookup")
  })
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

#' Lookup a function definiton
#'
#' @param x name or definition of a function
#' @param name function name if not given in x
#' @param envir the environment the function was defined in
#' @param all Whether to return just loaded definitions or all definitions
#' @param ... Additional arguments passed to internal functions
#' @export
lookup <- function(x, name = substitute(x), envir = environment(x) %||% parent.frame(), all = FALSE, ...) {
  fun <- as_lookup(x, envir = envir, name = name)

  if (fun$type %in% c("builtin", "special")) {
    fun$internal <- list(lookup_function(fun$name, type = "internal"))
  } else {
    fun$internal <- lapply(call_names(fun$def, type = ".Internal", subset = c(2, 1)), lookup_function, type = "internal")
    fun$internal <- c(fun$internal, lapply(call_names(fun$def, type = ".External", subset = c(2)), lookup_function, type = "call", package = fun$package))
    fun$ccall <- lapply(call_names(fun$def, type = ".Call", subset = c(2, 1)), lookup_function, type = "call", package = fun$package)
  }
  if (uses_rcpp(fun$package)) {
    rcpp_exports <- rcpp_exports(fun$package)
    fun$ccall <- lapply(call_names(fun$def, type = rcpp_exports, subset = c(1)), lookup_function, type = "rcpp", package = fun$package)
  }
  if (pryr::is_s3_method(fun$name, env = envir)) {
    fun$type <- append(fun$type, "S3 method", 0)
  }
  if (pryr::is_s3_generic(fun$name, env = envir)) {
    fun$type <- append(fun$type, "S3 generic", 0)
    fun$S3_methods <- lookup_S3_methods(fun, envir = envir, all = all)
  }
  if (isS4(fun$def)) {
    if (isGeneric(fun$name, where = envir)) {
      fun$type <- append(fun$type, "S4 generic", 0)
      fun$S4_methods <- lookup_S4_methods(fun, envir = envir, all = all)
    } else {
      fun$type <- append(fun$type, "S4 method")
    }
  }
  fun
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
`%==%` <- function(x, y) {
  identical(x, as.name(y))
}

lookup_S3_methods <- function(f, envir = parent.frame(), all = FALSE, ...) {

  S3_methods <- suppressWarnings(utils::.S3methods(f$name, envir = envir))

  flatten_list(lapply(S3_methods, function(name) lapply(as_lookup(getAnywhere(name)), lookup)), "lookup")
}

lookup_S4_methods <- function(f, envir = parent.frame(), all = FALSE, ...) {

  S4_methods <- .S4methods(f$name)
  S4_methods_info <- attr(S4_methods, "info")

  signatures <- strsplit(sub(paste0("^", escape(f$name), ",", "(.*)-method$"), "\\1", S4_methods), ",")

  res <- Map(getMethod, f$name, signatures)
  names(res) <- signatures
  res
}

#' @export
print.lookup <- function(x, envir = parent.frame(), ...) {
  lookup <- if (x$visible) "::" else ":::"

  cat(crayon::bold(x$package, lookup, x$name, sep = ""), " [", paste(collapse = ", ", x$type), "]\n", sep = "")
  cat(highlight_output(base::print.function(x$def), language = "r"), sep = "\n")
  lapply(x[["internal"]], print, envir = envir)
  lapply(x[["ccall"]], print, envir = envir)
  lapply(x[["S3_methods"]], print, envir = envir)
  lapply(x[["S4_methods"]], print, envir = envir)
  invisible(x)
}

escape <- function(x) {
  chars <- c("*", ".", "?", "^", "+", "$", "|", "(", ")", "[", "]", "{", "}", "\\")
  gsub(paste0("([\\", paste0(collapse = "\\", chars), "])"), "\\\\\\1", x, perl = TRUE)
}

highlight_output <- function(code, language = "r") {
  # The highlight library uses the same syntax for c and c++
  if (language == "c++") {
    language <- "c"
  }
  highlite::highlight_string(capture.output(force(code)), language = language)
}

# Rstudio open function in viewer
# Just function(x) View(x) or maybe
# (rstudioapi::findFun("sendToConsole"))('View(cat)', echo = F)

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
