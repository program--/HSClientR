#' @title List Authenticated User Information
#' @return A [tibble][tibble::tibble-package] of *your* user details.
#' @export
hs_user <- function() {
    request <- hsapi_request("user/")

    httr::stop_for_status(request)

    content <- httr::content(request) %>%
               tibble::as_tibble()

    content
}