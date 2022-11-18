library(bsicons)

# @param name a name for the snapshot
# @param ... a collection of UI elements
expect_snapshot_html <- function(name, ...) {
  # Only do browser based snapshotting on GHA mac
  skip_if_not_on_gha_mac()

  # I would love to use servr::httd() to serve up the file, but for some reason
  # that isn't working with webshot, so use python+processx instead
  skip_if(
    Sys.which("python3") == "",
    "python3 is required to do browser based testing"
  )

  withr::with_tempdir({
    htmltools::save_html(bslib::page_fluid(...), "index.html")
    processx::process$new("python3", c("-m", "http.server", "4000"))
    png <- webshot2::webshot("http://localhost:4000")
    expect_snapshot_file(png, name = name)
  })
}

test_that("bs_icon() returns a SVG string and renders as HTML", {

  globe <- bs_icon("globe")
  expect_snapshot(globe, cran = TRUE)

  rocket <- bs_icon(
    "rocket", size = "2rem",
    class = "text-success mt-1",
    title = "A rocket ship",
    a11y = "sem",
    border = "2px solid red"
  )
  expect_snapshot(bs_icon("rocket", "2rem"), cran = TRUE)


  expect_snapshot_html(
    name = "main-icon-test",
    globe, rocket
  )
})
