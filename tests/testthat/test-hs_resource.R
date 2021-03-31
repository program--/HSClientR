sample_id  <- "6092c8a62fac45be97a09bfd0b0bf726"
sample_id2 <- "1361509511e44adfba814f6950c6e742"

test_that("hs_resource() searches correctly", {

    vcr::use_cassette("hs_search", {
        resource_search <- hs_resource(page = 3)
    })

    expect_identical(
        names(resource_search),
        c(".next", ".prev", "results")
    )

    expect_true(tibble::is_tibble(resource_search$results))

    expect_identical(
        names(resource_search$results),
        c("id", "title", "author",
            "abstract", "contributor", "subject",
            "availability", "content_type", "doi",
            "metadata", "geodata")
    )

    expect_true(
        urltools::param_get(resource_search$.next)$page == 4
    )

    expect_true(
        urltools::param_get(resource_search$.prev)$page == 2
    )
})

test_that("hs_resource() retrieves files correctly", {

    vcr::use_cassette("hs_files", {
        resource_files <- hs_resource(files = TRUE, id = sample_id)
    })

    expect_identical(
        names(resource_files),
        c(".next", ".prev", "results")
    )

    expect_true(tibble::is_tibble(resource_files$results))
    expect_identical(
        names(resource_files$results),
        c("file_name", "url", "size", "content_type",
            "logical_file_type", "modified_time", "checksum")
    )
})

test_that("hs_resource() retrieves science metadata correctly", {

    vcr::use_cassette("hs_scimeta", {
        resource_scimeta <- hs_resource(scimeta = TRUE, id = sample_id)
    })

    expect_true(tibble::is_tibble(resource_scimeta))

    expect_identical(
        names(resource_scimeta),
        c("id", "title", "citation", "creators", "metadata")
    )

    expect_true(tibble::is_tibble(resource_scimeta$creators[[1]]))
    expect_true(tibble::is_tibble(resource_scimeta$metadata[[1]]))
})

test_that("hs_resource() retrieves folder correctly", {

    vcr::use_cassette("hs_folders", {
        resource_folder  <- hs_resource(pathname = "code/", id = sample_id2)
    })

    expect_true(tibble::is_tibble(resource_folder))

    expect_identical(
        names(resource_folder),
        c("resource_id", "path", "contents")
    )

    expect_true(tibble::is_tibble(resource_folder$contents[[1]]))

    expect_identical(
        names(resource_folder$contents[[1]]),
        c("item", "type")
    )
})

test_that("hs_resource() retrieves system metadata correctly", {

    vcr::use_cassette("hs_sysmeta", {
        resource_sysmeta <- hs_resource(id = sample_id)
    })

    expect_true(tibble::is_tibble(resource_sysmeta))

    expect_identical(
        names(resource_sysmeta),
        c("resource_title", "resource_type", "resource_id",
            "abstract", "authors", "creator", "doi", "public",
            "discoverable", "shareable", "immutable", "published",
            "date_created", "date_last_updated", "bag_url",
            "science_metadata_url", "resource_map_url", "resource_url",
            "content_types")
    )

    expect_true(nrow(resource_sysmeta) == 1)
})

vcr::use_cassette("hs_files_error", {
    test_that("hs_files() when `id` isn't given", {
        expect_error(hs_files())
    })
})

vcr::use_cassette("hs_access_error", {
    test_that("hs_access() when `id` isn't given", {
        expect_error(hs_access())
    })
})

vcr::use_cassette("hs_folder_error", {
    test_that("hs_folder() when `id` isn't given", {
        expect_error(hs_folder())
    })
})

vcr::use_cassette("hs_scimeta_error", {
    test_that("hs_scimeta() when `id` isn't given", {
        expect_error(hs_scimeta())
    })
})

vcr::use_cassette("hs_sysmeta_error", {
    test_that("hs_sysmeta() when `id` isn't given", {
        expect_error(hs_sysmeta())
    })
})

vcr::use_cassette("hs_types", {
    hstypes <- hs_types()
    test_that("hs_types() returns a vector", {
        expect_type(hstypes, "character")
        expect_true(length(hstypes) > 1)
    })
})

vcr::use_cassette("hs_content_types", {
    ctypes <- hs_content_types()
    test_that("hs_content_types() returns a vector", {
        expect_type(ctypes, "character")
        expect_true(length(ctypes) > 1)
    })
})