# nolint start
#' @title List resources
#'
#' @description
#' This is the primary function for querying the
#' HydroShare API `/hsapi/resource` endpoint. It will return
#' a [tibble][tibble::tibble-package] corresponding to the
#' given parameters. Endpoint is chosen algorithmically based
#' on passed parameters.
#'
#' @param ... See **details**.
#'
#' @return
#' A [tibble][tibble::tibble-package]. See **details**.
#'
#' @details
#' Below is a sectioned list of all possible parameters
#' that `hs_resource` can parse, and the 
#' corresponding HydroShare API endpoint(s)/functions used.
#' 
#' Parameters marked with an asterisk (*) are required for
#' that endpoint.
#' 
#' ## Searching Resources (`hs_search`)
#' 
#' ### Endpoint:
#' 
#' - **/hsapi/resource/search**
#' 
#' ### Parameters:
#' 
#' Parameter          | Data/Object Type   | Description
#' ------------------ | ------------------ | ---------------------------------------------
#' `page`             | `integer`          | A page number within the paginated result set.
#' `count`            | `integer`          | Number of results to return per page.
#' `creator`          | `character`        | The first author (name or email)
#' `author`           | `character vector` | List of authors (name or email)
#' `group`            | `character`        | A group name (requires `edit_permissions = TRUE`)
#' `user`             | `character`        | Viewable by user (name or email)
#' `owner`            | `character`        | Owned by user (name or email)
#' `from_date`        | `Date`             | To get a list of resources created on or after this date
#' `to_date`          | `Date`             | To get a list of resources created on or before this date
#' `subject`          | `character vector` | Comma separated list of subjects
#' `full_text_search` | `character`        | Get a list of resources with this text
#' `edit_permission`  | `logical`          | Filter by edit permissions of user/group/owner?
#' `published`        | `logical`          | Filter by published resources?
#' `type`             | `character vector` | List of resources of the specified resource types. See below  possible values.
#' `coverage_type`    | `character`        | To get a list of resources that fall within the specified tial coverage boundary. Either: "box" or "point"
#' `coordinates`      | `character vector` | List of spatial coordinates in the form of (north, south, t, west)
#' `include_obselete` | `logical`          | Include replaced resources?
#' 
#' For available resource types, see \link{hs_types}.
#' 
#' ## Getting Resource Information by ID (`hs_sysmeta`)
#' 
#' ### Endpoints:
#' 
#' - **/hsapi/resource/{id}/sysmeta**
#' 
#' ### Parameters:
#' 
#' Parameters | Data/Object Type | Description
#' ---------- | ---------------- | -----------
#' `id`*      | `character`      | Resource Alphanumeric ID
#' 
#' ### Return:
#' 
#' Column                 | Description
#' ---------------------- | ---------------------
#' `resource_title`       | Title of the resource
#' `resource_type`        | Resource type
#' `resource_id`          | ID of the resource
#' `abstract`             | Resource abstract
#' `authors`              | Authors of resource
#' `creator`              | Creator of resource
#' `doi`                  | Resource DOI ID
#' `public`               | Indicates if resource if public
#' `discoverable`         | Indicates if resource is discoverable
#' `shareable`            | Indicates if resource is shareable
#' `immutable`            | Indicates if resource is immutable
#' `published`            | Indicates if resource is published
#' `date_created`         | Date resource was created
#' `date_last_updated`    | Date resource was last updated
#' `bag_url`              | URL to resource download, can be passed to \code{\link{download.file}}
#' `science_metadata_url` | URL to science metadata
#' `resource_map_url`     | URL to resource map
#' `resource_url`         | URL to resource
#' `content_types`        | Resource content types, see \code{\link{hs_content_types}}
#' 
#' ## Getting Resource Access Permissions (`hs_access`)
#' 
#' ### Endpoint:
#' 
#' - **/hsapi/resource/{id}/access**
#' 
#' ### Parameters:
#' 
#' Parameter | Data/Object Type | Description
#' --------- | ---------------- | -----------
#' `id`*     | `character`      | Resource Alphanumeric ID
#' `access`* | `logical`        | Must be `TRUE` for access query
#' 
#' ### Return:
#' 
#' Column      | Description
#' ----------- | -----------
#' `id`        | Resource Alphanumeric ID
#' `privilege` | User's privilege level
#' `user`      | User to be granted privilege
#' `resource`  | Resource to which privilege applies
#' `grantor`   | Grantor of privilege
#' 
#' ## Getting Resource File List (`hs_files`)
#' 
#' ### Endpoint:
#' 
#' - **/hsapi/resource/{id}/files**
#' 
#' ### Parameters:
#' 
#' Parameter | Data/Object Type | Description
#' --------- | ---------------- | -----------
#' `id`*     | `character`      | Resource Alphanumeric ID
#' `files`*  | `logical`        | Must be `TRUE` for files query
#' `page`    | `integer`        | Page of results to return
#' `count`   | `integer`        | Number of files to return per page
#' 
#' ### Return:
#' 
#' Column              | Description
#' ------------------- | -----------
#' `file_name`         | The filename, including the path
#' `url`               | The url to download the file
#' `size`              | The size of the file
#' `content_type`      | The content type of the file
#' `logical_file_type` | The logical file type
#' `modified_time`     | The last modified time of the file
#' `checksum`          | The file's checksum
#' 
#' ## Getting Resource Folder(s) (`hs_folder`)
#' 
#' ### Endpoint:
#' 
#' - **/hsapi/resource/{id}/folders/{pathname}/**
#' 
#' ### Parameters:
#' 
#' Parameter   | Data/Object Type | Description
#' ----------- | ---------------- | -----------
#' `id`*       | `character`      | Resource Alphanumeric ID
#' `pathname`* | `character`      | Path to folder in resource's contents
#' 
#' ### Return:
#' 
#' Column        | Description
#' ------------- | -----------
#' `resource_id` | Resource Alphanumeric ID
#' `path`        | Path to folder in resource's contents
#' `contents`    | Nested `tibble` with files and folders within `path`
#' 
#' ## Getting Resource Science Metadata (`hs_scimeta`)
#' 
#' ### Endpoints:
#' 
#' - **/hsapi/resource/{id}/scimeta**
#' 
#' ### Parameters:
#' 
#' Parameter  | Data/Object Type | Description
#' ---------- | ---------------- | -----------
#' `id`*      | `character`      | Resource Alphanumeric ID
#' `scimeta`* | `logical`        | Must be `TRUE` for scimeta query
#' 
#' ### Return:
#' 
#' Column     | Description
#' ---------- | -----------
#' `id`       | Resource Alphanumeric ID
#' `title`    | Resource Title
#' `citation` | Resource Bibliography Citation
#' `creators` | Nested `tibble` of resource creator(s)
#' `sources`  | Nested `tibble` of resource source(s)
#' `metadata` | Nested `tibble` with additional resource metadata
#'
#' @export
# nolint end
hs_resource <- function(...) {
    path <- handle_params(...)
    response <- switch(
        path,
        search  = hs_search(...),
        files   = hs_files(...),
        folder  = hs_folder(...),
        access  = hs_access(...),
        scimeta = hs_scimeta(...),
        sysmeta = hs_sysmeta(...)
    )

    response
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

# nolint start
#' @title Search HydroShare Resources using solr conventions.
#' @param page (`integer`) A page number within the paginated result set.
#' @param count (`integer`) Number of results to return per page.
#' @param text (`character`) Search across all Resource Fields
#' @param author (`character`) Search by author
#' @param ... Additional parameters passed to the HydroShare API.
#'            See **details**.
#' @return A list with `.next`, `.prev`, and `results` attributes.
#'         `results` returns a [tibble][tibble::tibble-package].
#' @details
#'
#' ## Additional Parameters:
#' 
#' Parameter       | Data/Object Type   | Description
#' --------------- | ------------------ | ---------------------
#' `contributor`   | `character vector` | Search by contributor(s)
#' `subject`       | `character vector` | Search within subject keywords
#' `abstract`      | `character`        | Search within the abstract
#' `resource_type` | `character`        | Search by resource type
#' `content_type`  | `character vector` | Search by content type(s)
#' `coverage_type` | `character vector` | Search by coverage type(s) (point, box, period)
#' `availability`  | `character vector` | Search by availabilities (discoverable, public, published)
#' `created`       | `Date`             | Search by created date
#' `modified`      | `Date`             | Search by modified date
#' `start_date`    | `Date`             | Search by start date
#' `end_date`      | `Date`             | Search by end date
#' `east`          | `numeric`          | Search by location or box center east longitude
#' `north`         | `numeric`          | Search by location or box center north latitude
#' `eastlimit`     | `numeric`          | Search by east limit longitude
#' `westlimit`     | `numeric`          | Search by west limit longitude
#' `northlimit`    | `numeric`          | Search by north limit latitude
#' `southlimit`    | `numeric`          | Search by south limit latitude
#'
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_trim
#' @export
# nolint end
hs_search <- function(page = NULL, count = NULL,
                      text = NULL, author = NULL, ...) {

    request <- hsapi_request(
        path = "resource/search",
        query = list(
            page   = page,
            count  = count,
            text   = text,
            author = author,
            ...
        )
    )

    httr::stop_for_status(request)

    content <- httr::content(request)

    tidy_content <- content$results %>%
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

    split_text <-
        stringr::str_extract_all(
            tidy_content$text,
            pattern = "(?<=\\n)(.*?)(?=\\n)"
        )

    tidier_content <-
        dplyr::mutate(
            tidy_content,
            id    = stringr::str_trim(lapply(split_text, `[[`, 1)),
            doi   = stringr::str_trim(lapply(split_text, `[[`, 2)),
            title = stringr::str_trim(lapply(split_text, `[[`, 3))
        ) %>%
        dplyr::select(-.data$text) %>%
        dplyr::relocate(.data$id, .data$title) %>%
        tidyr::nest(
            metadata = c(
                .data$doi,
                .data$created,
                .data$modified,
                .data$start_date,
                .data$end_date,
                .data$resource_type
            ),
            geodata = c(
                .data$coverage_type,
                .data$northlimit,
                .data$eastlimit,
                .data$southlimit,
                .data$westlimit
            )
        )

    response <- list()
    response$".next" <- handle_null(content$"next")
    response$".prev" <- handle_null(content$"previous")
    response$results <- tidier_content

    response
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

#' @title Access Resource Permissions
#' @param id Resource ID
#' @return A [tibble][tibble::tibble-package].
#' @export
hs_access <- function(id) {
    if (missing(id)) rlang::abort("(hs_access) id required.")

    request <- hsapi_request(
        path = paste0(
            "resource/",
            id,
            "/access/"
        )
    )

    httr::stop_for_status(request)

    content <- httr::content(request)

    tibble::tibble(
        id        = handle_null(content$id),
        privilege = handle_null(content$privilege),
        user      = handle_null(content$user),
        resource  = handle_null(content$resource),
        grantor   = handle_null(content$grantor)
    )
}

#' @title Get Resource Files
#' @param id Resource ID
#' @param page Page of results to return
#' @param count Number of files to return per page
#' @return A [tibble][tibble::tibble-package].
#' @export
hs_files <- function(id, page = NULL, count = NULL) {
    if (missing(id)) rlang::abort("(hs_files) id required.")

    request <- hsapi_request(
        path = paste0(
            "resource/",
            id,
            "/sysmeta"
        ),
        query = list(
            page = page,
            count = count
        )
    )

    httr::stop_for_status(request)

    content <- httr::content(request)

    tidy_content <-
        content$results %>%
        lapply(FUN = function(item) {
            tibble::tibble(
                file_name         = handle_null(item$file_name),
                url               = handle_null(item$url),
                size              = handle_null(item$size),
                content_type      = handle_null(item$content_type),
                logical_file_type = handle_null(item$logical_file_type),
                modified_time     = handle_null(item$modified_time),
                checksum          = handle_null(item$checksum)
            )
        }) %>%
        dplyr::bind_rows()

    response <- list()

    response$".next" <- handle_null(content$"next")
    response$".prev" <- handle_null(content$"previous")
    response$results <- tidy_content

    response
}

#' @title Get Resource Folder
#' @param id Resource ID
#' @param pathname Path to folder in resource's contents
#' @return A [tibble][tibble::tibble-package].
#' @export
hs_folder <- function(id, pathname) {
    if (missing(id) | missing(pathname)) {
        rlang::abort(
            "(hs_folder) Both id and pathname are required."
        )
    }

    request <- hsapi_request(
        path = paste0(
            "resource/",
            id,
            "folders",
            pathname
        )
    )

    httr::stop_for_status(request)

    content <- httr::content(request)

    files <- unlist(content$files)
    folders <- unlist(content$folders)
    folder_contents <-
        dplyr::bind_rows(
            tibble::tibble(
                item = handle_null(files),
                type = "file"
            ),
            tibble::tibble(
                item = handle_null(folders),
                type = "folder"
            )
        )

    response <- tibble::tibble(
        resource_id = content$resource_id,
        path = content$path
    ) %>%
        tidyr::nest(contents = folder_contents)

    response
}

#' @title Get Resource Science Metadata
#' @param id Resource ID
#' @return A [tibble][tibble::tibble-package].
#' @importFrom xml2 read_xml
#' @export
hs_scimeta <- function(id) {
    if (missing(id)) rlang::abort("(hs_scimeta) id required.")

    request <- hsapi_request(
        path = paste0(
            "resource/",
            id,
            "/scimeta"
        )
    )

    httr::stop_for_status(request)

    content <- httr::content(request, as = "raw")

    xml <- xml2::read_xml(content)

    res <- xml$RDF[[1]]

    res_title <- res$title[[1]]
    res_cite  <- res$bibliographicCitation[[1]]
    res_lang  <- res$language
    res_type  <- paste(
        unlist(res$type$Description$label),
        collapse = ", "
    )
    res_subj  <- paste(
        unlist(res[which(names[res] == "subject")]),
        collapse = ", "
    )
    res_id    <- attr(
        res$identifier$Description$hydroShareIdentifier,
        which = "resource"
    )
    mod_date  <- res[which(names(res) == "date")][[1]]$modified$value
    cre_date  <- res[which(names(res) == "date")][[2]]$created$value

    creators <- res[which(names(res) == "creator")] %>%
                lapply(FUN = function(item) {
                    unlist(item$Description)
                }) %>%
                dplyr::bind_rows()

    sources  <- res[which(names(res == "source"))] %>%
                lapply(FUN = function(item) {
                    attr(
                        item$Description$isDerivedFrom,
                        which = "resource"
                     )
                }) %>%
                unlist() %>%
                unname() %>%
                tibble::as_tibble() %>%
                dplyr::rename(sources = .data$value)

    metadata <- tibble::tibble(
        languages = res_lang,
        types = res_type,
        subjects = res_subj,
        modified = mod_date,
        created = cre_date
    )

    response <-
        tibble::tibble(
            id = res_id,
            title = res_title,
            citation = res_cite
        ) %>%
        tidyr::nest(
            creators = creators,
            sources = sources,
            metadata = metadata
        )
}

#' @title Get Resource System Metadata by ID
#' @param id Alphanumeric HydroShare Resource ID
#' @return A [tibble][tibble::tibble-package].
#' @export
hs_sysmeta <- function(id) {
    if (missing(id)) rlang::abort("(hs_sysmeta) id required.")

    request <- hsapi_request(
        path = paste0(
            "resource/",
            id,
            "/sysmeta"
        )
    )

    httr::stop_for_status(request)

    item <- httr::content(request)

    response <-
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
        ) %>%
        dplyr::bind_rows()

    response
}