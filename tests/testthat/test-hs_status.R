vcr::use_cassette("hs_status", {
    test_that("hs_status() returns output", {
        expect_output(hs_status())
    })
})