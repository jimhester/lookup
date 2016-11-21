github_content <- function(s, path) {
  response <- gh("/repos/:owner/:repo/contents/:path", owner = s$RemoteUsername, repo = s$RemoteRepo, path = path, ref = s$RemoteRef)
  strsplit(rawToChar(jsonlite::base64_dec(response$content)), "\n", fixed = TRUE)[[1]]
}
