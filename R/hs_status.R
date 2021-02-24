#' @title Get HydroShare API Status
#' @description
#' This function calls upon the API base URL to return a status code 200.
#' If a code other than 200 is returned, the function will stop.
#' @return Output indicating API status
#' @export
hs_status <- function() {
    status <- httr::GET(handle = hsapi(),
                        path   = "hsapi/",
                        agent  = hs_agent())

    status_code <- httr::status_code(status)

    httr::stop_for_status(
        status,
        task = "HSClientR: Hydroshare API error."
    )

    content <- httr::content(status)

    cat(
        crayon::bold(
            crayon::blue("Hydro"),
            crayon::green("Share"),
            " REST API ",
            content$info$version,
            sep = ""
        ),
        " ",
        crayon::bold(crayon::green("[[", status_code, "]]", sep = "")),
        "\n",
        "Terms of Service:   ",
        crayon::cyan(content$info$termsOfService), "\n",
        "HydroShare Contact: ",
        crayon::cyan(content$info$contact$email), "\n",
        "GitHub Repository:  ",
        crayon::cyan("https://github.com/program--/HSClientR"), "\n",
        crayon::silver("Making Data Accessible."),
        " With ", crayon::red("\u2665"), " from ",
        crayon::bold("HSClientR"), ".",
        sep = ""
    )
}