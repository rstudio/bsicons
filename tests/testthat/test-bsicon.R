library(bsicons)
library(bslib)
library(htmltools)

bs_deps <- bs_theme_dependencies(bs_theme())

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
    py <- processx::process$new("python3", c("-m", "http.server", "4000"))

    html <- div(id = "main_content", bs_deps, ...)
    save_html(html, "index.html")

    png <- webshot2::webshot("http://localhost:4000", selector = "#main_content")
    expect_snapshot_file(png, name = name)
  })
}

test_that("bs_icon() returns a SVG string and renders as HTML", {

  globe <- bs_icon("globe", size = "5rem")
  expect_snapshot(as.character(globe), cran = TRUE)

  rocket <- bs_icon(
    "rocket", size = "7rem",
    class = "text-success mt-1",
    title = "A rocket ship",
    border = "2px solid red"
  )
  expect_snapshot(as.character(rocket), cran = TRUE)


  expect_snapshot_html(
    name = "main-icon-test.png",
    globe, rocket,
    bs_icon("0-circle-fill", size = "5rem", class = "text-primary")
  )
})
