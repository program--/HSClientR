#' @title List resources
#' @param page (`integer`) A page number within the paginated result set.
#' @param count (`integer`) Number of results to return per page.
#' @param creator (`character`) The first author (name or email)
#' @param author (`character vector`) List of authors (name or email)
#' @param group (`character`) A group name (requires `edit_permissions = TRUE`)
#' @param user (`character`) Viewable by user (name or email)
#' @param owner (`character`) Owned by user (name or email)
#' @param from_date (`Date`) to get a list of resources created
#'                           on or after this date
#' @param to_date (`Date`) to get a list of resources created
#'                         on or before this date
#' @param subject (`character vector`) Comma separated list of subjects
#' @param full_text_search (`character`) get a list of resources with this text
#' @param edit_permission (`logical`) filter by edit permissions
#'                                    of user/group/owner?
#' @param published (`logical`) filter by published resources?
#' @param type (`character vector`) list of resources of the
#'                                  specified resource types. \\
#'                                  See details for possible values.
#' @param coverage_type (`character`) to get a list of resources that fall
#'                                    within the specified spatial coverage
#'                                    boundary \\
#'                                    Either: "box" or "point"
#' @param coordinates (`character vector`) list of spatial coordinates in the
#'                                         form of (north, south, east, west)
#' @param include_obselete (`logical`) Include replaced resources?
#' @return a [tibble][tibble::tibble-package]
#' @details
#' Available resource types for **type**:
#' - `GenericResource`
#' - `RasterResource`
#' - `RefTimeSeriesResource`
#' - `TimeSeriesResource`
#' - `NetcdfResource`
#' - `ModelProgramResource`
#' - `ModelInstanceResource`
#' - `ToolResource`
#' - `SWATModelInstanceResource`
#' - `GeographicFeatureResource`
#' - `ScriptResource`
#' - `CollectionResource`
#' - `MODFLOWModelInstanceResource`
#' - `CompositeResource`
#'
#' Also returned by \code{\link{hs_types}}.
#' @export
hs_resource <- function(page             = NULL,
                        count            = NULL,
                        creator          = NULL,
                        author           = NULL,
                        group            = NULL,
                        user             = NULL,
                        owner            = NULL,
                        from_date        = NULL,
                        to_date          = NULL,
                        subject          = NULL,
                        published        = NULL,
                        type             = NULL,
                        full_text_search = NULL,
                        edit_permission  = NULL,
                        coverage_type    = NULL,
                        coordinates      = NULL,
                        include_obselete = NULL) {
    request <- hsapi_request(
        path  = "resource/",
        query = list(
            page             = page,
            count            = count,
            creator          = creator,
            author           = paste(author, collapse = ", "),
            group            = group,
            user             = user,
            owner            = owner,
            from_date        = from_date,
            to_date          = to_date,
            subject          = paste(subject, collapse = ", "),
            published        = published,
            type             = type,
            full_text_search = full_text_search,
            edit_permission  = edit_permission,
            coverage_type    = coverage_type,
            north            = coordinates[1],
            south            = coordinates[2],
            east             = coordinates[3],
            west             = coordinates[4],
            include_obselete = include_obselete
        )
    )

    httr::warn_for_status(request)

    content <- httr::content(request)

    content$results %>%
        lapply(FUN = function(item) {
            tibble::tibble(
                resource_title = ifelse(
                    is.null(item$resource_title),
                    NA,
                    item$resource_title
                ),
                resource_type = ifelse(
                    is.null(item$resource_type),
                    NA,
                    item$resource_type
                ),
                resource_id = ifelse(
                    is.null(item$resource_id),
                    NA,
                    item$resource_id
                ),
                abstract = ifelse(
                    is.null(item$abstract),
                    NA,
                    item$abstract
                ),
                authors = ifelse(
                    is.null(paste(item$authors, collapse = ", ")),
                    NA,
                    paste(item$authors, collapse = ", ")
                ),
                creator = ifelse(
                    is.null(item$creator),
                    NA,
                    item$creator
                ),
                doi = ifelse(
                    is.null(item$doi),
                    NA,
                    item$doi
                ),
                public = ifelse(
                    is.null(item$public),
                    NA,
                    item$public
                ),
                discoverable = ifelse(
                    is.null(item$discoverable),
                    NA,
                    item$discoverable
                ),
                shareable = ifelse(
                    is.null(item$shareable),
                    NA,
                    item$shareable
                ),
                immutable = ifelse(
                    is.null(item$immutable),
                    NA,
                    item$immutable
                ),
                published = ifelse(
                    is.null(item$published),
                    NA,
                    item$published
                ),
                date_created = ifelse(
                    is.null(item$date_created),
                    NA,
                    item$date_created
                ),
                date_last_updated = ifelse(
                    is.null(item$date_last_updated),
                    NA,
                    item$date_last_updated
                ),
                bag_url = ifelse(
                    is.null(item$bag_url),
                    NA,
                    item$bag_url
                ),
                science_metadata_url = ifelse(
                    is.null(item$science_metadata_url),
                    NA,
                    item$science_metadata_url
                ),
                resource_map_url = ifelse(
                    is.null(item$resource_map_url),
                    NA,
                    item$resource_map_url
                ),
                resource_url = ifelse(
                    is.null(item$resource_url),
                    NA,
                    item$resource_url
                ),
                content_types = ifelse(
                    is.null(paste(item$content_types, collapse = ", ")),
                    NA,
                    paste(item$content_types, collapse = ", ")
                )
            )
        }) %>%
        dplyr::bind_rows()
}

# TODO: hs_content_types documentation
#' @title List Content Types
#' @export
hs_content_types <- function() {
    request <- hsapi_request(path = "resource/content_types/")

    httr::stop_for_status(request)

    content <- httr::content(request) %>%
               unlist() %>%
               unname()

    content
}

# TODO: hs_search
#' @title Search HydroShare Resources using solr conventions.
#' @export
hs_search <- function(page,
                      count,
                      text,
                      author,
                      contributor,
                      subject,
                      abstract,
                      resource_type,
                      content_type,
                      coverage_type,
                      availability,
                      created,
                      modified,
                      start_date,
                      end_date,
                      east,
                      north,
                      eastlimit,
                      westlimit,
                      northlimit,
                      southlimit) {
    NA
}

# TODO: hs_types documentation
#' @title List Resource Types
#' @export
hs_types <- function() {
    request <- hsapi_request("resource/types/")

    httr::stop_for_status(request)

    content <- httr::content(request) %>%
               unlist() %>%
               unname()

    content
}

# TODO: hs_user documentation
#' @title List Authenticated User Information
#' @export
hs_user <- function() {
    request <- hsapi_request("user/")

    httr::stop_for_status(request)

    content <- httr::content(request) %>%
               tibble::as_tibble()

    content
}

# TODO: hs_userDetails documentation
#' @title List User Details for Some User
#' @export
hs_userDetails <- function(user_identifier) {
    if (missing(user_identifier)) {
        stop("(hs_userDetails) user_identifer required.")
    }

    request <- hsapi_request(path = paste0("userDetails/", user_identifier))

    httr::stop_for_status(request)

    content <- httr::content(request) %>%
               tibble::as_tibble()

    content
}

# TODO: hs_by_id
# TODO: hs_id_access
# TODO: hs_id_filelist
# TODO: hs_id_files
# TODO: hs_id_metadata
# TODO: hs_id_traverse
# TODO: hs_id_folders
# TODO: hs_id_map
# TODO: hs_id_scimeta
# TODO: hs_id_sysmeta
# TODO: hs_id_ticket
# TODO: hs_scimeta
# TODO: hs_taskstatus