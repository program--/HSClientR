vcr::use_cassette("hs_discover", {
    discover_query <- hs_discover()

    test_that("hs_discover() returns a tibble", {
        expect_s3_class(
            discover_query,
            c("tbl_df", "tbl", "data.frame")
        )

        expect_true(tibble::is_tibble(discover_query))

        # Ensure a JSON string isn't returned
        expect_false(is.character(discover_query))
    })

    test_that("hs_discover() returns the correct columns", {
        expect_true(length(names(discover_query)) == 6)

        expect_identical(
            names(discover_query),
            c("id", "title", "url", "author", "abstract", "metadata")
        )

        expect_true(
            tibble::is_tibble(
                discover_query[1, ]$metadata[[1]]
            )
        )

        expect_true(length(names(discover_query[1, ]$metadata[[1]])) == 9)
    })
})