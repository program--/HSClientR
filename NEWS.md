HSClientR 0.3.1 (2021-03-31)
============================

### MINOR IMPROVEMENTS

  * Updated package lifecycle to *Maturing*.
  * Updated `README` to make examples easier to read through.
  * Began work on tests! Aiming for 85% coverage at least.

### BUG FIXES

  * Fixed API response parsing issues for `hs_search()`, `hs_scimeta()`, `hs_folder()`, and a few other functions.


HSClientR 0.3.0 (2021-03-17)
============================

### NEW FEATURES

  * `hs_resource()` functionality revised. Now serves as a unified interface to `hs_search()`, `hs_files()`, `hs_folder()`, `hs_scimeta()`, and `hs_sysmeta()`. *These original functions are still usable.*
  * Started work on `hs_download()` which will allow the user to quickly download HydroShare resource files and directly load them into R (if possible).

### BUG FIXES
  * Added `handle_ssl()` which evaluates an API call and checks for cURL errors regarding SSL certificates. If an error is returned, the function attempts to fix it by downloading the HydroShare CA certificate and setting the cURL option `CURLOPT_CAPATH` to the path to the CA certificate.