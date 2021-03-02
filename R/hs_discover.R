#' @title HydroShare Discover API
#' @description Query the HydroShare Discover Page
#' @return A [tibble][tibble::tibble-package] of the first 40 results from
#'         HydroShare's [Discover Page](https://www.hydroshare.org/search/). See
#'         details for `tibble` description.
#' @details
#' ## Main `tibble`
#' The returned `tibble` contains the following columns:
#' - `title`: The title of the resource.
#' - `link`: A link to the corresponding resource.
#' - `author`: The primary author of the resource.
#' - `abstract`: The abstract of the resource.
#' - `metadata`: Metadata `tibble` associated with the resource. (see below)
#'
#' ## Metadata
#' The metadata `tibble` can be accessed via `$metadata`,
#' and contains the columns:
#' - `authors`: All authors of the resource.
#' - `contributor`: All contributors for the resource.
#' - `author_link`: A URL (if it exists) to the
#'                  primary author's HydroShare profile.
#' - `owner`: The owner(s) of the resource.
#' - `subject`: The subject tags of the resource.
#' - `created`: The date the resource was created.
#' - `modified`: The laste data the resource was modified.
#' - `availability`: The availability tags of the resource.
#' - `type`: The resource type.
#' @importFrom tidyr nest
#' @export
hs_discover <- function() {
    request <- httr::GET(
        handle = hsapi(),
        path = "discoverapi/",
        agent = hs_agent()
    )

    content <- httr::content(request)

    response <- jsonlite::fromJSON(content$resources) %>%
                tibble::as_tibble()

    response %>%
        dplyr::mutate(
            link = paste0("https://hydroshare.org", link),
            availability = paste(availability, collapse = ", "),
            authors = paste(authors, collapse = ", "),
            author_link = ifelse(
                is.na(author_link),
                NA,
                paste("https://hydroshare.org", author_link)
            ),
            owner = paste(owner, collapse = ", "),
            subject = paste(subject, collapse = "; "),
            contributor = paste(contributor, collapse = ", "),
            created = as.Date(created, "%Y-%m-%dT%H:%M:%S"),
            modified = as.Date(modified, "%Y-%m-%dT%H:%M:%S")
        ) %>%
        dplyr::select(-availabilityurl, -geo) %>%
        tidyr::nest(
            metadata = c(authors, contributor, author_link,
                         owner, subject, created,
                         modified, availability, type),
        )
}