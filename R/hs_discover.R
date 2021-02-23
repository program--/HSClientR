#' @title HydroShare Discover API
#' @export
hs_discover <- function() {
    request <- httr::GET(handle = hsapi(), path = "discoverapi/")

    content <- httr::content(request)

    response <- jsonlite::fromJSON(content$resources) %>%
                tibble::as_tibble()
}