vcr::use_cassette("hs_user_details_1", {
    httr::reset_config()

    test_that("hs_user_details() returns errors", {
        expect_error(hs_user_details())
    })
})
vcr::use_cassette("hs_user_details_2", {
    httr::reset_config()
    test_that("hs_user_details() returns errors without auth", {
        expect_error(hs_user_details(8409))
    })
})

vcr::use_cassette("hs_user_details_3", {

    httr::reset_config()

    test_that("hs_user_details() returns correctly", {
        skip_on_cran()
        skip_on_ci()
        hs_auth(set_header = TRUE)
        userdet <- hs_user_details(8409)

        expect_true(tibble::is_tibble(userdet))

        expect_identical(
            names(userdet),
            c("Name", "Email", "Profile_URL", "Phone_Number",
              "Address", "Organization", "Website")
        )

        expect_identical(userdet$Name, "Singh-Mohudpur, Justin")
        expect_identical(userdet$Email, "justin@justinsingh.me")
        expect_identical(userdet$Website, "https://justinsingh.me")
    })

    httr::reset_config()
})