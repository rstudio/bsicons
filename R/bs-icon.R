#' Use Bootstrap icons (as inline SVG)
#'
#' @param name The name of the Bootstrap icon. Whitespace is replaced with `-`
#'   (that way, `"arrow up"` can be used to refer to the "actual name" of
#'   `"arrow-up"`). For a searchable list of names, see <https://icons.getbootstrap.com/>
#' @param size Any valid CSS unit defining both the height and width of the
#'   icon.
#' @param class Additional CSS classes to add to the `<svg>` element. Consider
#'   providing Bootstrap 5+ utility classes (e.g., `text-success`) here to
#'   stylize the icon (but also note that those utility classes will only work
#'   when Bootstrap 5+ is on the page).
#' @param title If provided (highly recommended), `a11y` defaults to `"sem"`,
#'   meaning the title is used for on-hover text and screen reader
#'   announcements.
#' @param a11y Cases that distinguish the role of the icon and inform which
#'   accessibility attributes to be used. Icons can either be `"deco"`
#'   (decorative, the default case), `"sem"` (semantic), `"none"` (no
#'   accessibility features). The default, `"auto"`, resolves to `"sem"` if a
#'   `title` is provided (and `"deco"` otherwise).
#' @param ... additional CSS properties (e.g., `margin`, `position`, etc.)
#'   placed on the `<svg>` tag.
#'
#' @return An [htmltools::HTML()] string containing the SVG icon.
#'
#' @examples
#'
#' up <- bs_icon("arrow-up-circle", size = "9em", class = "text-success")
#' up_fill <- bs_icon("arrow-up-circle-fill", size = "9em", class = "text-success")
#'
#' # utility class will only apply with a modern version of Bootstrap
#' if (interactive() && requireNamespace('bslib')) {
#'   bslib::page_fluid(up, up_fill)
#' }
#'
#' @import htmltools
#' @export
bs_icon <- function(
  name,
  size = "1em",
  class = NULL,
  title = NULL,
  a11y = c("auto", "deco", "sem", "none"),
  ...
) {

  if (length(name) != 1) {
    rlang::abort("The number of icons specified in `name` must be 1.")
  }

  name <- sub("\\s+", "-", tolower(name))
  idx <- match(name, tolower(icon_info$name))
  if (is.na(idx)) {
    dists <- utils::adist(name, icon_info$name)
    suggestions <- icon_info$name[order(dists)][1:5]
    cli::cli_abort(c(
      "This Bootstrap icon {.val {name}} does not exist.",
      "i" = "Did you mean one of the following: {.or {.val {suggestions}}}?",
      "i" = "Try searching for the icon: {.url https://icons.getbootstrap.com}."
    ))
  }

  svg_children <- icon_info$contents[idx]

  size <- validateCssUnit(size)
  style_attr <- paste0(
    "height:", size, ";",
    "width:", size, ";",
    "fill:currentColor;",
    # Better default vertical positioning of icons in a inline context (inspired by fontawesome::fa())
    "vertical-align:-0.125em;",
    htmltools::css(...)
  )

  # Generate accessibility attributes if either of
  # the "deco" or "sem" cases are chosen
  a11y <- rlang::arg_match(a11y)
  a11y_attrs <- ""

  if (a11y == "auto") {
    a11y <- if (is.null(title)) "deco" else "sem"
  }

  if (a11y == "deco") {
    a11y_attrs <- 'aria-hidden="true" role="img" '
  } else if (a11y == "sem") {
    title <- title %||% name
    a11y_attrs <- sprintf(
      'aria-label="%s" role="img" ',
      htmlEscape(title, attribute = TRUE)
    )
  }

  res <- sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" class="bi bi-%s %s" style="%s" %s>%s%s</svg>',
    name,
    paste(class, collapse = " "),
    style_attr,
    a11y_attrs,
    if (is.null(title)) "" else paste0("<title>", htmlEscape(title), "</title>"),
    svg_children
  )

  browsable(HTML(res))
}

"%||%" <- function(x, y) {
  if (is.null(x)) y else x
}
