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
#' @param oauth Authenticate with OAuth2? `r lifecycle::badge("experimental")`
#' @param client_id OAuth2 Client ID (Key), required if `oauth = TRUE`
#' @param client_secret OAuth2 Client Secret (Secret), required if `oauth = TRUE`
#' @keywords internal
#' @export
hs_auth <- function(oauth = FALSE, client_id = NULL, client_secret = NULL) {
    username <- Sys.getenv("HSCLIENT_USER")
    password <- Sys.getenv("HSCLIENT_PASS")

    if (identical(username, "") & identical(password, "")) {
        stop("(HSClientR) Both HSCLIENT_USER and HSCLIENT_PASS not set.\n",
            "Please set env vars to your HydroShare ",
            "username and password, respectively.",
            call. = FALSE)
    }

    if (identical(username, "")) {
        stop("(HSClientR) HSCLIENT_USER not set.\n",
            "Please set env var to your HydroShare username.",
            call. = FALSE)
    }

    if (identical(password, "")) {
        stop("(HSClientR) HSCLIENT_PASS not set.\n",
            "Please set env var to your HydroShare password.",
            call. = FALSE)
    }

    if (oauth & !is.null(client_id) & !is.null(client_secret)) {
        app <- httr::oauth_app(
            appname = "hydroshare",
            key     = client_id,
            secret  = client_secret
        )

        endpoint <- httr::oauth_endpoint(
            authorize = "authorize",
            access    = "token",
            base_url  = "https://www.hydroshare.org/o"
        )

        httr::oauth2.0_token(
            endpoint = endpoint,
            app = app,
            client_credentials = TRUE
        )
    } else {
        httr::authenticate(username, password)
    }
}

#' @title Helper function to [POST][httr::POST] authentication
#'        and pass along session.
#' @description `r lifecycle::badge("experimental")`
#' @keywords internal
#' @export
hs_session <- function() {
    csrf <- hsapi_request("", type = "GET")$cookies$csrftoken

    httr::POST(
        "hydroshare.org/accounts/login/?next=/hsapi/",
        httr::set_cookies(csrftoken = csrf),
        body = list(
            csrfmiddlewaretoken = csrf,
            username = I(Sys.getenv("HSCLIENT_USER")),
            password = I(Sys.getenv("HSCLIENT_PASS")),
            "next" = "/hsapi/"
        ),
        encode = "form"
    )
}

#' @title Helper function for:
#'        [GET][httr::GET], [POST][httr::POST],
#'        [PUT][httr::PUT], and [DELETE][httr::DELETE] requests.
#' @param path API Request Path
#' @param type Request Type (GET, POST, PUT, or DELETE)
#' @param query API Query Parameters
#' @param auth Use basic HTTPS authentication
#' @param oauth Use OAuth2 authentication `r lifecycle::badge("experimental")`
#' @keywords internal
#' @export
hsapi_request <- function(path, type, query = NULL, auth = TRUE, oauth = TRUE) {
    if (missing(type)) type <- "GET"

    if (identical(type, "GET")) {
        if (auth) {
            auth <- hs_auth()

            httr::GET(
                handle = hsapi(),
                path   = paste0("hsapi/", path),
                query  = query,
                auth   = ifelse(identical(class(auth), "request", NULL, auth)),
                agent  = hs_agent()
            )
        } else {
            httr::GET(
                handle = hsapi(),
                path   = paste0("hsapi/", path),
                query  = query,
                agent  = hs_agent()
            )
        }
    }
}

#' @title Helper function for handling \code{NULL}
#'        values returned from API calls.
#' @param content Content returned from element of [httr::content][httr::content]
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