github_content <- function(s, path, owner = s$description$RemoteUsername, repo = s$description$RemoteRepo, ref = s$description$RemoteRef, api_url = s$description$RemoteHost) {
  response <- gh("/repos/:owner/:repo/contents/:path", owner = owner, repo = repo, path = path, ref = ref, .api_url = api_url)
  strsplit(rawToChar(jsonlite::base64_dec(response$content)), "\n", fixed = TRUE)[[1]]
}

github_code_search <- function(s, name = s$search, owner = s$description$RemoteUsername, repo = s$description$RemoteRepo, api_url = s$description$RemoteHost) {
  response <- gh("/search/code", q = paste("in:file", paste0("repo:", owner, "/", repo), "path:src/", "language:c", "language:c++", name), .api_url = api_url)

  vapply(response$items, `[[`, character(1), "path")
}
