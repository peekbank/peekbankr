# offline unit tests for the RLE decode used by get_aoi_timepoints(rle = TRUE)

library(tibble)

test_that("decode_rle_timepoints expands runs at the 40 Hz timestep", {
  rle_tbl <- tibble(
    administration_id = 1L, trial_id = 1L,
    t_norm = c(-50L, 0L, 50L),
    aoi = c("distractor", "target", "missing"),
    length = c(2L, 2L, 1L)
  )
  out <- peekbankr:::decode_rle_timepoints(rle_tbl)
  expect_equal(nrow(out), 5)
  expect_equal(out$t_norm, as.integer(c(-50, -25, 0, 25, 50)))
  expect_equal(out$aoi, c("distractor", "distractor", "target", "target",
                          "missing"))
  expect_equal(unique(out$administration_id), 1L)
})

test_that("decode_rle_timepoints keeps administrations and trials separate", {
  rle_tbl <- tibble(
    administration_id = c(1L, 1L, 2L),
    trial_id = c(1L, 2L, 1L),
    t_norm = c(0L, 100L, -25L),
    aoi = c("target", "distractor", "other"),
    length = c(3L, 2L, 2L)
  )
  out <- peekbankr:::decode_rle_timepoints(rle_tbl)
  expect_equal(nrow(out), 7)
  t1 <- out[out$administration_id == 1 & out$trial_id == 1, ]
  expect_equal(t1$t_norm, as.integer(c(0, 25, 50)))
  t2 <- out[out$administration_id == 1 & out$trial_id == 2, ]
  expect_equal(t2$t_norm, as.integer(c(100, 125)))
  t3 <- out[out$administration_id == 2 & out$trial_id == 1, ]
  expect_equal(t3$t_norm, as.integer(c(-25, 0)))
  expect_equal(t3$aoi, c("other", "other"))
})

test_that("decoding round-trips an rle() encoding of a sample sequence", {
  aoi_seq <- c("target", "target", "distractor", "missing", "missing",
               "missing", "target")
  runs <- rle(aoi_seq)
  starts <- as.integer(c(0, cumsum(runs$lengths[-length(runs$lengths)]) * 25))
  rle_tbl <- tibble(
    administration_id = 7L, trial_id = 42L,
    t_norm = starts, aoi = runs$values, length = runs$lengths
  )
  out <- peekbankr:::decode_rle_timepoints(rle_tbl)
  expect_equal(out$aoi, aoi_seq)
  expect_equal(out$t_norm, as.integer(seq(0, by = 25,
                                          length.out = length(aoi_seq))))
})
