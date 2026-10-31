# Download dataset README files

**\[superseded\]**

Use `pb$readmes()` on a
[`peekbank()`](https://peekbank.github.io/peekbankr/reference/peekbank.md)
client.

Downloads README files for Peekbank datasets from Redivis.

## Usage

``` r
get_readmes(datasets = c(), local_base_dir = "dataset_readmes")
```

## Arguments

- datasets:

  Character vector of dataset names. If empty (default), downloads
  READMEs for all datasets.

- local_base_dir:

  Directory to save README files to (default: "dataset_readmes")

## Value

No return value, called for side effects. README files are saved to the
specified directory and the path is printed via message.

## Examples

``` r
if (FALSE) { # \dontrun{
get_readmes()
get_readmes(datasets = c("pomper_saffran_2016"))
} # }
```
