# You'd then also provide a helper that skips tests where you can't
# be sure of producing exactly the same output
expect_snapshot_plot <- function(name, code) {
  # Other packages might affect results
  skip_if_not_installed("ggplot2", "2.0.0")
  # Or maybe the output is different on some operation systems
  #skip_on_os("windows")
  skip_on_ci()
  # You'll need to carefully think about and experiment with these skips

  name <- paste0(name, ".png")
  # Announce the file before touching `code`. This way, if `code`
  # unexpectedly fails or skips, testthat will not auto-delete the
  # corresponding snapshot file.
  announce_snapshot_file(name = name)

  path <- tempfile(fileext = ".png")
  if(ggplot2::is_ggplot(code)){
    ggplot2::ggsave(filename = path, plot = code, width = 7, height = 5)
  } else {
    png(path, width = width, height = height)
    on.exit(dev.off())
    path <- code
  }

  expect_snapshot_file(path, name)
}