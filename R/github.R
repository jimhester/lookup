#' @importFrom jsonlite base64_dec
NULL

github_content <- function(path, owner, repo, ref = "master", api_url = "https://api.github.com") {
  response <- gh("/repos/:owner/:repo/contents/:path", owner = owner, repo = repo, path = path, ref = ref, .api_url = api_url)
  strsplit(rawToChar(base64_dec(response$content)), "\n", fixed = TRUE)[[1]]
}

github_code_search <- function(name, path = "src/", owner, repo, language = c("c", "c++"), api_url = "https://api.github.com") {
  response <- gh("/search/code", q = paste("in:file", paste0("repo:", owner, "/", repo), paste0("path:", path), paste0("language:", language), name, collapse = "\n"), .api_url = api_url)

  vapply(response$items, `[[`, character(1), "path")
}
