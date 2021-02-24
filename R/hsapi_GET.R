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
#' @return
#' A [tibble][tibble::tibble-package] with columns:
#' - `resource_title` - Title of the resource
#' - `resource_type` - Resource type
#' - `resource_id` -  ID of the resource
#' - `abstract` - Resource abstract
#' - `authors` - Authors of resource
#' - `creator` - Creator of resource
#' - `doi` - Resource DOI ID
#' - `public` - Indicates if resource if public
#' - `discoverable` - Indicates if resource is discoverable
#' - `shareable` - Indicates if resource is shareable
#' - `immutable` - Indicates if resource is immutable
#' - `published` - Indicates if resource is published
#' - `date_created` - Date resource was created
#' - `date_last_updated` - Date resource was last updated
#' - `bag_url` - URL to resource download,
#'               can be passed to \code{\link{download.file}}
#' - `science_metadata_url` - URL to science metadata
#' - `resource_map_url` - URL to resource map
#' - `resource_url` - URL to resource
#' - `content_types` - Resource content types,
#'                     see \code{\link{hs_content_types}}
#'
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
                resource_title       = handle_null(item$resource_title),
                resource_type        = handle_null(item$resource_type),
                resource_id          = handle_null(item$resource_id),
                abstract             = handle_null(item$abstract),
                authors              = handle_null(item$authors, TRUE),
                creator              = handle_null(item$creator),
                doi                  = handle_null(item$doi),
                public               = handle_null(item$public),
                discoverable         = handle_null(item$discoverable),
                shareable            = handle_null(item$shareable),
                immutable            = handle_null(item$immutable),
                published            = handle_null(item$published),
                date_created         = handle_null(item$date_created),
                date_last_updated    = handle_null(item$date_last_updated),
                bag_url              = handle_null(item$bag_url),
                science_metadata_url = handle_null(item$science_metadata_url),
                resource_map_url     = handle_null(item$resource_map_url),
                resource_url         = handle_null(item$resource_url),
                content_types        = handle_null(item$content_types)
            )
        }) %>%
        dplyr::bind_rows()
}

#' @title List Content Types
#' @return
#' A character vector of content types.
#'
#' As of 2/24/2021, the types returned will be:
#' - `GenericLogicalFile`
#' - `GeoRasterLogicalFile`
#' - `NetCDFLogicalFile`
#' - `GeoFeatureLogicalFile`
#' - `RefTimeseriesLogicalFile`
#' - `TimeSeriesLogicalFile`
#' - `FileSetLogicalFile`
#'
#' @export
hs_content_types <- function() {
    request <- hsapi_request(path = "resource/content_types/")

    httr::stop_for_status(request)

    content <- httr::content(request) %>%
               unlist() %>%
               unname()

    content
}

#' @title Search HydroShare Resources using solr conventions.
#' @param page (`integer`) A page number within the paginated result set.
#' @param count (`integer`) Number of results to return per page.
#' @param text (`character`) Search across all Resource Fields
#' @param author (`character`) Search by author
#' @param contributor *Search by contributor
#' @param subject (`character vector`) Search within subject keywords
#' @param abstract (`character`) Search within the abstract
#' @param resource_type (`character`) Search by resource type
#' @param content_type (`character vector`) Search by content type
#' @param coverage_type (`character vector`) Search by coverage type
#'                                           (point, box, period)
#' @param availability (`character vector`) Search by availability
#'                              (discoverable, public, published)
#' @param created (`Date`) Search by created date
#' @param modified (`Date`) Search by modified date
#' @param start_date (`Date`) Search by start date
#' @param end_date (`Date`) Search by end date
#' @param east (`numeric`) Search by location or box center east longitude
#' @param north (`numeric`) Search by location or box center north latitude
#' @param eastlimit (`numeric`) Search by east limit longitude
#' @param westlimit (`numeric`) Search by west limit longitude
#' @param northlimit (`numeric`) Search by north limit latitude
#' @param southlimit (`numeric`) Search by south limit latitude
#' @export
hs_search <- function(page          = NULL,
                      count         = NULL,
                      text          = NULL,
                      author        = NULL,
                      contributor   = NULL,
                      subject       = NULL,
                      abstract      = NULL,
                      resource_type = NULL,
                      content_type  = NULL,
                      coverage_type = NULL,
                      availability  = NULL,
                      created       = NULL,
                      modified      = NULL,
                      start_date    = NULL,
                      end_date      = NULL,
                      east          = NULL,
                      north         = NULL,
                      eastlimit     = NULL,
                      westlimit     = NULL,
                      northlimit    = NULL,
                      southlimit    = NULL) {

    request <- hsapi_request(
        path = "resource/search",
        query = list(
            page          = page,
            count         = count,
            text          = text,
            author        = author,
            contributor   = contributor,
            subject       = subject,
            abstract      = abstract,
            resource_type = resource_type,
            content_type  = content_type,
            coverage_type = coverage_type,
            availability  = availability,
            created       = created,
            modified      = modified,
            start_date    = start_date,
            end_date      = end_date,
            east          = east,
            north         = north,
            eastlimit     = eastlimit,
            westlimit     = westlimit,
            northlimit    = northlimit,
            southlimit    = southlimit
        )
    )

    httr::stop_for_status(request)

    content <- httr::content(request)

    content$results %>%
        lapply(FUN = function(item) {
            tibble::tibble(
                text          = handle_null(item$text),
                author        = handle_null(item$author),
                abstract      = handle_null(item$abstract),
                contributor   = handle_null(item$contributor, TRUE),
                subject       = handle_null(item$subject, TRUE),
                availability  = handle_null(item$availability, TRUE),
                created       = handle_null(item$created),
                modified      = handle_null(item$modified),
                coverage_type = handle_null(item$coverage_type, TRUE),
                east          = handle_null(item$east),
                northlimit    = handle_null(item$northlimit),
                eastlimit     = handle_null(item$eastlimit),
                southlimit    = handle_null(item$southlimit),
                westlimit     = handle_null(item$westlimit),
                start_date    = handle_null(item$start_date),
                end_date      = handle_null(item$end_date),
                resource_type = handle_null(item$resource_type),
                content_type  = handle_null(item$content_type, TRUE)
            )
        }) %>%
        dplyr::bind_rows()
}

#' @title List Resource Types
#' @return
#' A character vector containing the elements:
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
#' 
#' @description `r lifecycle::badge("experimental")`
#' 
#' @details
#' Currently, lifecycle is experimental due to requiring
#' OAuth2 authentication. There is currently limited
#' documentation on OAuth2 authentication for HydroShare.
#' @export
hs_user <- function() {
    # // hs_session()

    request <- hsapi_request("user/")

    httr::stop_for_status(request)

    content <- httr::content(request) %>%
               tibble::as_tibble()

    content
}

# TODO: hs_user_details documentation
#' @title List User Details for Some User
#' 
#' @description `r lifecycle::badge("experimental")`
#' 
#' @param user_identifier HydroShare User Identifier
#' @details
#' Currently, lifecycle is experimental due to requiring
#' OAuth2 authentication. There is currently limited
#' documentation on OAuth2 authentication for HydroShare.
#' @export
hs_user_details <- function(user_identifier) {
    if (missing(user_identifier)) {
        stop("(hs_user_details) user_identifer required.")
    }

    # // hs_session()

    request <- hsapi_request(
        path = paste0("userDetails/", user_identifier)
    )

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