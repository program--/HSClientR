hsapi <- function() {
    httr::handle("https://www.hydroshare.org/")
}

hs_agent <- function() {
    httr::user_agent("HSClientR (https://github.com/program--/HSClientR)")
}

hs_auth <- function() {
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

    httr::authenticate(username, password)
}

hsapi_request <- function(path, query = NULL, auth = TRUE) {
    if (auth) {
        httr::GET(
            handle = hsapi(),
            path   = paste0("hsapi/", path),
            query  = query,
            auth   = hs_auth(),
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