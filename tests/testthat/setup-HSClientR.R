library("vcr") # *Required* as vcr is set up on loading
invisible(vcr::vcr_configure(
    dir = vcr::vcr_test_path("fixtures"),
    filter_sensitive_data = list(
        "<<oauth_key>>" = Sys.getenv("HS_ACCESS_TOKEN")
    ),
    match_requests_on = "method"
))
vcr::check_cassette_names()
