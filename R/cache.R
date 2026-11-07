# We have two caches that are triggered on every read: a session cache in memory and optionally a disk cache that later sessions read from.

# cached reads, shared by every client for the life of the session
.peekbankr_env <- new.env(parent = emptyenv())

# The folder behind a cache_on_disk client. The path is absolute so that a later
# setwd() keeps using the same cache.
new_cache_dir <- function() {
  dir <- file.path(getwd(), ".pbcache")
  dir.create(dir, showWarnings = FALSE)
  # a hidden folder of downloaded data is easy to commit by accident, so we add our own internal .gitignore rule
  writeLines("*", file.path(dir, ".gitignore"))
  dir
}

cache_file <- function(pin, key) {
  if (is.null(pin$cache_dir)) return(NULL)
  file.path(pin$cache_dir, pin$redivis_tag,
            paste0(rlang::hash(key), ".rds"))
}

save_cache_file <- function(value, path) {
  tmp <- tempfile(tmpdir = dirname(path))
  on.exit(unlink(tmp))
  failed <- function(e) {
    warning("Could not save to the disk cache, so the next session downloads ",
            "this again: ", conditionMessage(e), call. = FALSE)
  }
  tryCatch({
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    saveRDS(value, tmp)
    file.rename(tmp, path)
  }, warning = failed, error = failed)
  invisible(NULL)
}

# A file that cannot be read counts as missing, and is removed so that a fresh
# copy can take its place.
read_cache_file <- function(path) {
  tryCatch(readRDS(path), error = function(e) {
    message("A file in the disk cache could not be read, so it is ",
            "downloaded again.")
    unlink(path)
    NULL
  })
}

# Reads are served from the session cache. An entry missing there is restored
# from its file for a cache_on_disk client, and fetched otherwise.
cached_read <- function(pin, key, fetch) {
  path <- cache_file(pin, key)
  if (is.null(.peekbankr_env[[key]]) && !is.null(path) && file.exists(path)) {
    .peekbankr_env[[key]] <- read_cache_file(path)
  }
  if (is.null(.peekbankr_env[[key]])) {
    .peekbankr_env[[key]] <- fetch()
  }
  value <- .peekbankr_env[[key]]
  if (!is.null(path) && !is.null(value) && !file.exists(path)) {
    save_cache_file(value, path)
  }
  value
}
