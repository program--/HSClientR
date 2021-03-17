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

    # Make sure SSL errors don't occur
    handle_ssl({
        if (identical(type, "GET")) {
            request <- httr::GET(
                handle = hsapi(),
                path   = paste0("hsapi/", path),
                agent  = hs_agent(),
                ...
            )
        }
    })

    request
}

#' @title Helper function for handling \code{NULL}
#'        values returned from API calls.
#' @param content Content returned from element of
#'                [httr::content][httr::content]
#' @param is_list Is `content` a list?
#' @keywords internal
handle_null <- function(content, is_list = FALSE) {
    if (is_list) {
        ifelse(is.null(paste(content, collapse = ", ")),
               NA,
               paste(content, collapse = ", "))
    } else {
        ifelse(is.null(content), NA, content)
    }
}

#' @title Helper function for handling parameters
#'        passed to `hs_resource`.
#' @description
#' Algorithmically determine
#' what function is supposed to be called based on
#' the given parameters.
#' @param ... Parameters passed to any request.
#' @keywords internal
handle_params <- function(...) {
    # Handle if multiple unique parameters
    # are passed but are set to FALSE
    params <- list(...)[!sapply(X = list(...), FUN = rlang::is_false)]
    passed_params <- names(params)

    # Check if too many unique parameters were passed
    complexity <- sum(
        c("files", "pathname", "permissions", "scimeta") %in% passed_params
    )

    if (complexity > 1) {
        rlang::abort(
            paste(
                "(hs_resource)",
                "Too many unique parameters.",
                "More than one of the following unique parameters were passed:",
                "c('files', 'pathname', 'permissions', 'scimeta')",
                sep = "\n"
            )
        )
    }

    dplyr::case_when(
        "pathname" %in% passed_params    ~ "folder",
        "files" %in% passed_params       ~ "files",
        "permissions" %in% passed_params ~ "access",
        "scimeta" %in% passed_params     ~ "scimeta",
        "id" %in% passed_params          ~ "sysmeta",
        TRUE                             ~ "search"
    )
}

# nolint start
#' @title Helper function for handling SSL Certificate errors
#'        returned from API calls.
#' @description See **details** for information on the SSL errors.
#' @param expr An expression. Internally, this is the request in \link{hsapi_request}.
#' @details
#' Generally, when an SSL Certificate issue is returned from
#' HyroShare's API, it is a result of the intermediate CA certificate
#' not being passed to the requester. Note that, we only retrieve the
#' intermediate CA certificate because the root certificate is still
#' being passed to the requester by the server. This can be verified here:
#' 
#' https://www.ssllabs.com/ssltest/analyze.html?d=hydroshare.org
#' 
#' To prevent this error, this function will catch the SSL error from
#' a test GET request and download, store, and set the `CURLOPT_CAINFO` cURL option.
#'
#' The certificate that this function retrieves is:
#'
#' ```
#' GeoTrust TLS DV RSA Mixed SHA256 2020 CA-1
#' Fingerprint SHA256: 8c43c5e340ec640f93ea774ac5353cca9042f764ff837f870d8b64763c458a41
#' Pin SHA256: n5dIU+KFaI00Y/prmvaZhqXOquF72TlPANCLxCA9HE8=
#' RSA 2048 bits (e 65537) / SHA256withRSA
#' https://www.digicert.com/kb/digicert-root-certificates.htm
#' ```
#' 
#' The certificate chain that HydroShare uses is:
#'
#' ```
#' DigiCert Global Root CA
#' -> GeoTrust TLS DV RSA Mixed SHA256 2020 CA-1
#'    -> *.hydroshare.org
#' ```
#' @keywords internal
#' @export
handle_ssl <- function(expr) {
    # Evaluation
    expr    <- rlang::enquo(expr)
    quoexpr <- rlang::quo_get_expr(expr)
    quoenv  <- rlang::quo_get_env(expr)

    # Try to evaluate `fn`
    tryCatch(
        expr = eval(quoexpr, quoenv),
        error = function(cond) {
            if ("ssl" %in% strsplit(tolower(cond$message), " ")[[1]]) {
                rlang::inform(
                    paste(
                        "(handle_ssl)",
                        "HydroShare returning SSL error...",
                        "Attempting to fix by remediating certificate chain.",
                        sep = "\n"
                    )
                )

                # Reset httr cURL config
                httr::reset_config()

                # Create tempfile
                cafile <- tempfile(
                    pattern = "GeoTrustTLSDVRSAMixedSHA2562020CA-1.crt",
                    fileext = ".pem"
                )

                # Download Intermediate CA
                cert <- download.file(
                    "https://cacerts.digicert.com/GeoTrustTLSDVRSAMixedSHA2562020CA-1.crt.pem",
                    destfile = cafile
                )

                # Set cURL to fix cert chain
                httr::set_config(config = httr::config(cainfo = cafile))
            } else {
                rlang::abort(cond$message)
            }
        }
    )

    # Evaluate expr again once SSL cert is resolved
    eval(quoexpr, quoenv)
}
# nolint end