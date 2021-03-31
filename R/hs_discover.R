#' @title HydroShare Discover API
#' @description Query the HydroShare Discover Page
#' @return A [tibble][tibble::tibble-package] of the first 40 results from
#'         HydroShare's [Discover Page](https://www.hydroshare.org/search/). See
#'         details for `tibble` description.
#' @details
#' ## Main `tibble`
#' The returned `tibble` contains the following columns:
#' - `id`: The resource's unique HydroShare ID.
#' - `title`: The title of the resource.
#' - `url`: A url to the corresponding resource.
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
#' @importFrom rlang .data
#' @export
hs_discover <- function() {
    request <- httr::GET(
        handle = hsapi(),
        path = "discoverapi/",
        agent = hs_agent()
    )

    content  <- httr::content(request)
    response <- jsonlite::fromJSON(content$resources) %>%
                tibble::as_tibble()

    response %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
            link         = paste0("https://hydroshare.org", .data$link),
            availability = paste(.data$availability, collapse = ", "),
            authors      = paste(.data$authors, collapse = ", "),
            author_link  = ifelse(
                is.na(.data$author_link),
                NA,
                paste("https://hydroshare.org", .data$author_link)
            ),
            owner        = paste(.data$owner, collapse = ", "),
            subject      = paste(.data$subject, collapse = "; "),
            contributor  = paste(.data$contributor, collapse = ", "),
            created      = as.Date(.data$created, "%Y-%m-%dT%H:%M:%S"),
            modified     = as.Date(.data$modified, "%Y-%m-%dT%H:%M:%S")
        ) %>%
        dplyr::ungroup() %>%
        dplyr::select(-.data$availabilityurl, -.data$geo) %>%
        tidyr::nest(
            metadata = c(
                .data$authors,
                .data$contributor,
                .data$author_link,
                .data$owner,
                .data$subject,
                .data$created,
                .data$modified,
                .data$availability,
                .data$type
            )
        ) %>%
        dplyr::rename(id = .data$short_id, url = .data$link) %>%
        dplyr::relocate(.data$id)
}