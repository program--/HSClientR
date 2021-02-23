#' @title Get HydroShare API Status
#' @export
hs_status <- function() {
    status <- hsapi_request()

    status_code <- httr::status_code(status)

    httr::stop_for_status(
        status,
        task = "HSClientR: Hydroshare API error."
    )

    content <- httr::content(status)

    cat(
        "[[", status_code, "]]\n",
        content$info$description, " ", content$info$version, "\n",
        "Terms of Service:   ", content$info$termsOfService, "\n",
        "HydroShare Contact: ", content$info$contact$email, "\n",
        "Making Data Accessible. With \u2665 from HSClientR.",
        sep = ""
    )
}