#' @title R6 Class representing a HydroShare Client
#' @description 
#' `r lifecycle::badge("experimental")`
#' 
#' *In development*
#' @importFrom R6 R6Class
#' @importFrom urltools param_get
#' @importFrom lifecycle badge
#' @export
HSClient <- R6::R6Class("HSClient",
    public = list(
        #' @description
        #' Initialize a HydroShare Client
        #' @return A new `HSClient` object.
        #'
        #' *Note: A message will be issued
        #' since R6-object is still in development.*
        initialize = function() {
            rlang::inform(
                crayon::cyan(crayon::bold(paste0(
                    "R6-based client still in-development.\n",
                    "Full functionality may not exist."
                )))
            )

            preamble <- crayon::bold(paste0(
                "\n",
                crayon::blue("HS"),
                crayon::green("Client"),
                crayon::blue("R"),
                " - ",
                crayon::red("R6 Client"),
                "\n"
            ))

            msg <- paste0(
                crayon::silver("=======================================\n"),
                "Thanks for using HSClientR! ",
                crayon::red("\u2665"),
                "\nYou can start a query with ",
                crayon::red("$query()"),
                "\nMake sure to authenticate with ",
                crayon::red("$authenticate()"),
                "\n"
            )

            cat(preamble,
                msg,
                crayon::silver("=======================================\n"),
                sep = "")

            invisible(self)
        },

        #' @description
        #' Print HSClient object
        #' @return `HSClient` output
        print = function() {

            if (!is.null(private$.token)) {
                private$.user_details <- hs_user()

                user_msg <- paste0(
                    "Authenticated as:\n",
                    crayon::bold(crayon::cyan(
                        private$.user_details$first_name,
                        private$.user_details$last_name
                    )),
                    "\n",
                    crayon::bold("User: "), private$.user_details$username, "\n",
                    crayon::bold("ID:   "), private$.user_details$id, "\n",
                    crayon::bold("Org:  "), private$.user_details$organization, "\n"
                )

                res_msg <- paste0(
                    crayon::bold("Last Query ($query): "),
                    private$.query, "\n",
                    crayon::bold("Query Results:\n"),
                    "\tCount: ", nrow(private$.query_results),
                    "\n"
                )

                msg <- paste0(
                    user_msg,
                    crayon::silver("=======================================\n"),
                    res_msg
                )
            } else {
                msg <- paste0(
                    "It looks like you haven't authenticated yet...\n",
                    "Please call ",
                    crayon::red('$authenticate()'),
                    " on this object to get\nOAuth2 authentication set up with ",
                    crayon::bold(crayon::blue("Hydro"), crayon::green("Share"), sep = ""),
                    "! :)",
                    "\n"
                )
            }

            cat(msg)

            invisible(self)
        },

        #' @description
        #' Perform OAuth2 Authentication
        #' with HydroShare.
        #' @return A [Token][httr::Token] object.
        #' @details
        #' Note, the access token will be set
        #' as a header automatically if `.token`
        #' is `NULL`.
        authenticate = function() {
            if (!is.null(private$.token)) {
                private$.token
            } else {
                private$.token <- hs_auth()

                # Get access token
                creds <- private$.token$credentials %>%
                         names() %>%
                         jsonlite::fromJSON()

                # Apply to all subsequent requests
                httr::set_config(
                    config = httr::add_headers(
                        Authorization = paste("Bearer", creds$access_token)
                    )
                )

                private$.token
            }

            invisible(self)
        },

        #' @description
        #' Query/Search HydroShare
        #' @param ... Query parameters. See \link{hs_search} parameters.
        #' @return R6 object
        query = function(...) {
            parameters <- lapply(sys.call()[-1], deparse)
            
            if (identical(parameters, "")) parameters <- "NONE"

            private$.query <- paste0(
                ifelse(nchar(names(parameters)) > 0, paste0(names(parameters), " = "), ""),
                unlist(parameters),
                collapse=", "
            )

            temp_results <- hs_search(...)

            private$.next_page     <- temp_results$".next"
            private$.prev_page     <- temp_results$.prev
            private$.query_results <- temp_results$results
            private$.current_resource <- 1

            print(private$.query_results)

            invisible(self)
        },

        #' @description
        #' Get current resource
        #' @return A [tibble][tibble::tibble-package] of the current resource
        get_res = function() {
            if (is.null(private$.current_resource)) {
                rlang::abort("There is no current resource. Use $query()")
            } else {
                private$.query_results[private$.current_resource, ]
            }
        },

        #' @description
        #' Get next resource
        #' @return R6 object, use `$get_res()` to get the resource tibble.
        next_res = function() {
            if (private$.current_resource == nrow(private$.query_results)) {
                rlang::abort("There is no next resource.")
            } else {
                private$.current_resource <- private$.current_resource + 1
                self$get_res()
            }

            invisible(self)
        },

        #' @description
        #' Get previous resource
        #' @return R6 object, use `$get_res()` to get the resource tibble.
        prev_res = function() {
            if (private$.current_resource == 1) {
                rlang::abort("There is no previous resource.")
            } else {
                private$.current_resource <- private$.current_resource - 1
                self$get_res()
            }

            invisible(self)
        },

        #' @description
        #' Get next search page
        #' @return R6 object
        next_page = function() {
            if (is.null(private$.next_page) | is.na(private$.next_page)) {
                rlang::abort("There is no next page.")
            } else {
                do.call(self$query, urltools::param_get(private$.next_page))
            }

            invisible(self)
        },

        #' @description
        #' Get previous resource
        #' @return R6 object
        prev_page = function() {
            if (is.null(private$.prev_page) | is.na(private$.prev_page)) {
                rlang::abort("There is no previous page.")
            } else {
                do.call(self$query, urltools::param_get(private$.prev_page))
            }

            invisible(self)
        }
    ),
    private = list(
        .user_details = NULL,
        .current_resource = NULL,
        .next_res  = NULL,
        .prev_res  = NULL,
        .next_page = NULL,
        .prev_page = NULL,
        .query = NULL,
        .query_results = NULL,
        .token = NULL
    )
)