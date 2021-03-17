#' @title List User Details for Some User
#' @param user_identifier HydroShare User Identifier
#' @return A [tibble][tibble::tibble-package] of user details.
#' @export
hs_user_details <- function(user_identifier) {
    if (missing(user_identifier)) {
        stop("(hs_user_details) user_identifer required.")
    }

    request <- hsapi_request(
        path = paste0(
            "userDetails/",
            user_identifier,
            "/"
        )
    )

    httr::stop_for_status(request)

    content <- httr::content(request) %>%
               lapply(FUN = function(attribute) {
                   if (identical(attribute, ""))
                       NA
                   else
                       attribute
               })

    tibble::tibble(
        Name         = content$name,
        Email        = content$email,
        Profile_URL  = content$url,
        Phone_Number = content$phone,
        Address      = content$address,
        Organization = content$organization,
        Website      = content$website
    )
}