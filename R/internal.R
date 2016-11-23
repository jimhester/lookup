internal_source <- function(name) {
  structure(list(search = name, type = "internal", language = "c", remote_type = "internal"), class = "internal")
}
fetch_symbol_map.internal <- function(s, branch = "trunk") {
  s$map_lines <- github_content(s, path = "src/main/names.c", owner = "wch", repo = "r-source", ref = branch)
  s
}
parse_symbol_map.internal <- function(s, name = s$search, ...) {
  lines <- s$map_lines
  m <- regexpr("^\\{\"([^\"]+)\",[[:space:]]*([^,]+)", lines, perl = TRUE)
  res <- na.omit(captures(lines, m))
  s$map <- setNames(res[[2]], res[[1]])
  s$regex <- s$map[s$search]
  s
}

source_files.internal <- function(s, name = s$search, ...) {
  s$src_files <- github_code_search(s, s$map[name], path = "src/main", language = "c", owner = "wch", repo = "r-source")

  # Ignore the names.c file
  s$src_files <- s$src_files[s$src_files != "src/main/names.c"]
  s
}

fetch_source.internal <- function(s, path, branch = "trunk") {
  s$src_lines <- github_content(s, path = path, owner = "wch", repo = "r-source", ref = branch)
  s$src_path <- path
  s
}

parse_source.internal <- function(s, regex) {
  lines <- s$src_lines
  start <- grep(paste0("SEXP[[:space:]]+attribute_hidden[[:space:]]+", regex, "\\([^)(]+)[[:space:]]*"), lines)
  if (length(start)) {
    length <- find_function_end(lines[seq(start, length(lines))])
    if (!is.na(length)) {
      end <- start + length - 1
      s$fun_start <- start
      s$fun_end <- end
      s$fun_lines <- s$src_lines[seq(start, end)]
      return(s)
    }
  }
  s
}
