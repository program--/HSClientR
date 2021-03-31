vcr::use_cassette("hs_user_1", {
    httr::reset_config()

    test_that("hs_user() returns nothing without auth", {
        expect_true(all(unlist(hs_user()) == "None"))
    })
})

vcr::use_cassette("hs_user_2", {
    test_that("hs_user() returns a tibble", {
        skip_if_not(interactive())
        skip_on_cran()
        skip_on_ci()
        hs_auth(set_header = TRUE)
        me <- hs_user()
        expect_true(tibble::is_tibble(me))
        expect_identical(
            names(me),
            c("username", "email", "first_name", "id",
              "last_name", "organization", "state",
              "country", "user_type")
        )
    })

    test_that("hs_user() returns own user information", {
        skip_if_not(interactive())
        skip_on_cran()
        skip_on_ci()
        hs_auth(set_header = TRUE)
        me <- hs_user()
        expect_identical(me$username, "jsinghm")
        expect_identical(me$email, "justin@justinsingh.me")
        expect_identical(me$first_name, "Justin")
        expect_identical(me$last_name, "Singh-Mohudpur")
        expect_equal(me$id, 8409)
    })

    httr::reset_config()
})
