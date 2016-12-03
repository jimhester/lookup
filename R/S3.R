#' @importFrom methods getGroupMembers
NULL

is_internal <- function(f) {
  if (!is.function(f) || is.primitive(f))
    return(FALSE)

  length(call_names(f, ".Internal")) > 0
}


# Functions modified from originals at pryr/R/s3.r
is_s3_generic <- function(fname, env = parent.frame()) {
  if (!nzchar(fname)) {
     return(FALSE)
  }
  if (!exists(fname, env)) return(FALSE)

  f <- get(fname, env, mode = "function")
  if (!is.function(f)) return(FALSE)

  if (is.primitive(f) || is_internal(f)) {
    is_internal_generic(fname)
  } else {
    length(call_names(f, "UseMethod")) > 0
  }
}

is_s3_method <- function(name, env = parent.frame()) {
  !is.null(find_generic(name, env))
}

stop_list <- function() {
  if (getRversion() < "3.3.0") {
    getNamespace("tools")[[".make_S3_methods_stop_list"]](NULL)
  } else {
    tools::nonS3methods(NULL)
  }
}

find_generic <- function(name, env = parent.frame()) {
  if (name %in% stop_list()) return(NULL)

  pieces <- strsplit(name, ".", fixed = TRUE)[[1]]
  n <- length(pieces)

  # No . in name, so can't be method
  if (n == 1) return(NULL)

  for(i in seq_len(n - 1)) {
    generic <- paste0(pieces[seq_len(i)], collapse = ".")
    class <- paste0(pieces[(i + 1):n], collapse = ".")
    if (is_s3_generic(generic, env)) return(c(generic, class))
  }
  NULL
}

is_internal_generic <- function(x) {
  x %in% internal_generics()
}

internal_generics <- function() {
  # Functions in S4 group generics should be the same
  group <- c(getGroupMembers("Arith"), getGroupMembers("Compare"),
    getGroupMembers("Logic"), getGroupMembers("Math"), getGroupMembers("Math2"),
    getGroupMembers("Summary"), getGroupMembers("Complex"))

  primitive <- .S3PrimitiveGenerics

  # Extracted from ?"internal generic"
  internal <- c("[", "[[", "$", "[<-", "[[<-", "$<-", "unlist",
    "cbind", "rbind", "as.vector")

  c(group, primitive, internal)
}
