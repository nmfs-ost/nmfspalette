test_that("nmfs_palette() works", {
  # Check that nmfs_palette returns a function
  coral_palette <- nmfs_palette("coral")
  coral_palette_rev <- nmfs_palette("coral", reverse = TRUE)

  expect_type(coral_palette, "closure")

  # Check that nmfs_palette(n) returns a vector
  expect_snapshot(
    coral_palette(10)
  )
  expect_snapshot(
    coral_palette_rev(10)
  )
})

test_that("nmfs_palette() fails", {
  # Check that fake palette names create an error
  expect_snapshot(
    error = TRUE,
    nmfs_palette("foo")
  )
})

test_that("display_nmfs_palette() works", {
  # Check that display_nmfs_palette returns a function
  urchin_palette <- display_nmfs_palette("urchin", 4)

  expect_type(urchin_palette, "NULL")

  # Check that display_nmfs_palette() returns a snapshot
  expect_snapshot(
    urchin_palette
  )
})
