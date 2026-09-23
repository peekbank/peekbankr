# Download stimulus images for the Peekbank repository

This function downloads stimulus images for selected Peekbank datasets
from the peekbank_files dataset on Redivis. It retrieves stimulus
metadata from a Peekbank database connection, constructs the full
stimulus file paths, and downloads them to a local directory.

## Usage

``` r
download_stimuli(
  con,
  local_base_dir = "stimulus_data",
  datasets = c(),
  skip_existing = TRUE
)
```

## Arguments

- con:

  A version handle created by connect_to_peekbank()

- local_base_dir:

  Local directory path where stimulus images will be saved (default:
  "stimulus_data")

- datasets:

  Character vector of dataset names to download stimuli for. If empty
  (default), downloads stimuli for all datasets.

- skip_existing:

  skip downloading a file if a file with that name already exists in
  that path locally

## Value

Returns the stimulus df with an additional column for the paths of the
downloaded stimuli

## Examples

``` r
if (FALSE) { # \dontrun{
con <- connect_to_peekbank("2025.1")

# Download stimuli for all datasets
download_stimuli(con, local_base_dir = "stimulus_data")

# Download stimuli for specific datasets
download_stimuli(con, local_base_dir = "stimulus_data",
                 datasets = c("reflook_v4", "reflook_socword"))
} # }
```
