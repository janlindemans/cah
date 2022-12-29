test_that_folders_exist <- function() {
  withr::local_options(
    cah.box_path =
      "/Users/jwl38/Library/CloudStorage/Box-Box/CAH/CAH Shared",
    cah.drive_path = paste0(
      "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com",
      "/.shortcut-targets-by-id/1mCSu5zzZGk4BAPPBaZCMQXwnKmOPQSfU/CAH/CAH Shared Drive"
    )
  )
  cah_set_options(force = TRUE)
  for (i in 1:length(cah_path())) {
    test_that(paste0("Folder ",cah_path()[i]," exist"), {
      expect_true(file.exists(cah_path()[i]))
    })
  }
}
test_that_folders_exist()
test_that("better packages works", {
  withr::local_options(
    cah.box_path =
      "/Users/jwl38/Library/CloudStorage/Box-Box/CAH/CAH Shared",
    cah.drive_path = paste0(
      "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com",
      "/.shortcut-targets-by-id/1mCSu5zzZGk4BAPPBaZCMQXwnKmOPQSfU/CAH/CAH Shared Drive"
    )
  )
  cah_set_options(force = TRUE)
  #browser()
  expect_true(is.data.frame(better::gbd_read())) # TAKES LONG
})

