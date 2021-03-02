#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

## usethis namespace: start
#' @importFrom tibble tibble
## usethis namespace: end
NULL

## usethis namespace: start
#' @importFrom graphics title
## usethis namespace: end
NULL

#' @title Helper for [httr::handle][httr::handle].
#' @keywords internal
#' @export
hsapi <- function() {
    httr::handle("https://www.hydroshare.org/")
}

#' @title Helper for HSClientR's [user_agent][httr::user_agent].
#' @keywords internal
#' @export
hs_agent <- function() {
    httr::user_agent("HSClientR (https://github.com/program--/HSClientR)")
}

#' @title Helper for [authentication][httr::authenticate].
#' @return A [Token][httr::Token] if `set_header = FALSE`,
#'         otherwise an access token set in global config
#'         to be included in all requests.
#' @keywords internal
#' @export
hs_auth <- function(set_header = FALSE) {

    client_id <- "50cgkHLnpyBIXgr7i8DnXrJsr8CSO64gws0EptKO"
    client_secret <- paste0("SSq2Rrln8BLL9K5P58pl4r3r7dGXwP1u",
                            "ArT382i4gFev7llmSkWGblFfu28BUVOd",
                            "845dFufTiF27Grgfm7iVWCWhtLQdKrZh",
                            "FdcYZgRfYLw2tHP4wvcPRdCOijjTei5Q")

    app <- httr::oauth_app(
        appname = "hydroshare",
        key     = client_id,
        secret  = client_secret
    )

    endpoint <- httr::oauth_endpoint(
        authorize = "https://www.hydroshare.org/o/authorize/",
        access    = "https://www.hydroshare.org/o/token/"
    )

    token <- httr::oauth2.0_token(
        endpoint = endpoint,
        app      = app,
        type     = "application/x-www-form-urlencoded",
        cache    = TRUE
    )

    if (set_header) {
        # Get access token
        creds <- token$credentials %>%
                names() %>%
                jsonlite::fromJSON()

        # Apply to all subsequent requests
        httr::set_config(
            config = httr::add_headers(
                Authorization = paste("Bearer", creds$access_token)
            )
        )
    } else {
        token
    }
}

#' @title Helper function for:
#'        [GET][httr::GET], [POST][httr::POST],
#'        [PUT][httr::PUT], and [DELETE][httr::DELETE] requests.
#' @param path API Request Path
#' @param type Request Type (GET, POST, PUT, or DELETE)
#' @param ... Parameters passed onto a `httr` request
#' @keywords internal
#' @export
hsapi_request <- function(path, type, ...) {
    if (missing(type)) type <- "GET"

    if (identical(type, "GET")) {
        httr::GET(
            handle = hsapi(),
            path   = paste0("hsapi/", path),
            agent  = hs_agent(),
            ...
        )
    }
}

#' @title Helper function for handling \code{NULL}
#'        values returned from API calls.
#' @param content Content returned from element of
#'                [httr::content][httr::content]
#' @param is_list Is `content` a list?
#' @keywords internal
#' @export
handle_null <- function(content, is_list = FALSE) {
    if (is_list) {
        ifelse(is.null(paste(content, collapse = ", ")),
               NA,
               paste(content, collapse = ", "))
    } else {
        ifelse(is.null(content), NA, content)
    }
}