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
#' - `geodata`: Geodata `tibble` associated with the resource. (see below)
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
#'
#' ## Geodata
#' The geodata `tibble` can be accessed via `$geodata`,
#' and contains the columns:
#' - `short_id`: The HydroShare DOI Short ID
#' - `geo_title`: Title of resource (same as `title`)
#' - `coverage_type`: The resource's feature type
#'                    on the coverage map
#' - `north`: The latitude midpoint
#' - `east`: The longitude midpoint
#' - `northlimit`: Coverage map north latitude
#' - `southlimit`: Coverage map south latitude
#' - `eastlimit`: Coverage map east longitude
#' - `westlimit`: Coverage map west longitude
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

    geo_data <- response$geo %>%
                dplyr::rename(geo_title = title)

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
        dplyr::select(
            -availabilityurl,
            -geo
        ) %>%
        dplyr::left_join(geo_data, by = "short_id") %>%
        tidyr::nest(
            metadata = c(authors, contributor, author_link,
                            owner, subject, created,
                            modified, availability, type),
            geodata  = c(short_id, geo_title, coverage_type,
                         north, east, northlimit,
                         southlimit, eastlimit, westlimit)
        )
}