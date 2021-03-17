# TODO
hs_download <- function(...) {
    UseMethod("hs_download", ...)
}

hs_download.data.frame <- function(...) {
    NULL
}

hs_download.character <- function(...) {
    NULL
}

#' @title Helper for downloading files from HydroShare
#' @param id Resource ID
#' @param path Path in resource's contents
#' @importFrom utils download.file
#' @keywords internal
#' @export
download_request <- function(id = NULL, path = NULL, ...) {
    request <- hsapi_request(
        path = paste0(
            "resource/",
            id,
            "/"
        )
    )

    httr::stop_for_status(request)

    content <- httr::content(request)

    if (identical(content$status, "Not ready")) {
        download_request(id = id, path = path)
    } else {
        download.file(
            content,
            paste0(path, "/", id),
            mode = "wb",
            ...
        )
    }
}