# Check if within aoi_timepoints table, there is no duplication in all the administration_ids associated with each individual trial_id

Check if within aoi_timepoints table, there is no duplication in all the
administration_ids associated with each individual trial_id

## Usage

``` r
ds.validate_trial_uniqueness_constraint(df_aoi_timepoints)
```

## Arguments

- df_aoi_timepoints:

  the aoi_timepoints dataframe

## Value

an empty string when all the administration_ids are unique within each
trial_id; Otherwise, the error message will be returned.

## Examples

``` r
if (FALSE) { # \dontrun{
is_valid <- ds.validate_trial_uniqueness_constraint(df_aoi_timepoints = aoi_timepoints)
} # }
```
