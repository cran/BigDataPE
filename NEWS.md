# BigDataPE 0.0.96

## Bug Fixes

- Fixed an issue in `bdpe_fetch_chunks()` where the `offset` parameter was being formatted as a scientific double (e.g., `1e+05`), which caused the API to fail. The value is now properly formatted as an integer string, ensuring compatibility with the API which expects an exact integer format.



