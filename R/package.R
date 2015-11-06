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

names_file <- memoise::memoise(function(branch = "master") {
  readLines(paste(sep = "/", "https://raw.githubusercontent.com/wch/r-source", branch, "src/main/names.c"))
})
