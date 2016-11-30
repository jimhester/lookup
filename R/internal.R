internal_source <- function(name) {
  structure(list(name = name, type = "internal", language = "c", remote_type = "internal"), class = "internal")
}
fetch_symbol_map.internal <- function(s, branch = "trunk") {
  s$map_lines <- github_content(path = "src/main/names.c", owner = "wch", repo = "r-source", ref = branch)
  s
}
parse_symbol_map.internal <- function(s, name = s$name, ...) {
  lines <- s$map_lines
  m <- regexpr("^\\{\"([^\"]+)\",[[:space:]]*([^,]+)", lines, perl = TRUE)
  res <- na.omit(captures(lines, m))
  s$map <- setNames(res[[2]], res[[1]])
  s$search <- s$map[s$name]
  s$regex <- paste0("SEXP[[:space:]]+attribute_hidden[[:space:]]+", s$search, "\\([^)(]+\\)[[:space:]]*\\{")
  s
}

source_files.internal <- function(s, name = s$search, ...) {
  s$src_files <- github_code_search(name, path = "src/main", language = "c", owner = "wch", repo = "r-source")

  # Ignore the names.c file
  s$src_files <- s$src_files[s$src_files != "src/main/names.c"]
  s
}

fetch_source.internal <- function(s, path, branch = "trunk") {
  s$src_lines <- github_content(path = path, owner = "wch", repo = "r-source", ref = branch)
  s$src_path <- path
  s
}

parse_source.internal <- function(s, regex = s$regex) {
  s$fun_start <- s$fun_end <- s$fun_lines <- NULL

  new_lines <- cumsum(nchar(s$src_lines) + 1)
  lines <- paste(s$src_lines, collapse = "\n")
  start <- regexpr(regex, lines)
  if (length(start)) {
    end <- find_function_end(lines, start)
    if (!is.na(end)) {
      s$fun_start <- tail(which(new_lines <= start), n = 1L) + 1L
      s$fun_end <- head(which(new_lines >= end), n = 1L) - 1L
      s$fun_lines <- s$src_lines[seq(s$fun_start, s$fun_end)]
      return(s)
    }
  }
  s
}
