#' Color scale constructor for nmfs colors
#'
#' @param palette Character name of palette in `nmfs_palettes`. Default value
#' is "oceans".
#' @param discrete Boolean indicating whether color aesthetic is discrete.
#' Default is TRUE.
#' @param reverse Boolean indicating whether the palette should be reversed.
#' Default is FALSE.
#' @param interpolate Boolean indicating whether the colors assigned to plot
#' objects should interpolated from palettes, with the alternative that only the
#' defined colors in the palette are used. Default is TRUE.
#' @param ... Additional arguments passed to: [ggplot2::scale_color_gradientn()]
#' when `discrete` is TRUE; [ggplot2::discrete_scale()] when `discrete` is FALSE
#' and `interpolate` is TRUE; and [ggplot2::scale_color_manual()] when `discrete`
#' is FALSE and `interpolate` is FALSE.
#' @examples
#' library(ggplot2)
#' ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#'   geom_point(size = 4) +
#'   scale_color_nmfs("coral")
#'
#' ggplot(mtcars, aes(mpg, disp, color = as.factor(gear))) +
#'   geom_point(size = 4) +
#'   scale_color_nmfs("regional",
#'     interpolate = FALSE,
#'     discrete = TRUE
#'   )
#' @export
scale_color_nmfs <- function(
    palette = "oceans",
    discrete = TRUE,
    reverse = FALSE,
    interpolate = TRUE,
    ...) {
  pal <- nmfs_palette(palette = palette, reverse = reverse)

  pal_length <- length(nmfs_palettes[[palette]])

  if (discrete) {
    if (interpolate) {
      ggplot2::discrete_scale(
        aesthetics = "colour",
        palette = pal,
        ...
      )
    } else {
      cli::cli_alert_info("The {palette} palette has {pal_length} colors.")
      rlang::warn(
        message = "An error will occur if there are too few palette colors for your plot.
To avoid this error, use a larger palette or `interpolate = TRUE`.",
        .frequency = "once",
        .frequency_id = "too_few_colors_warning_color"
      )
      ggplot2::scale_color_manual(
        values = nmfs_palette(palette)(pal_length),
        ...
      )
    }
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for nmfs colors
#' @inheritParams scale_color_nmfs
#' @param ... Additional arguments passed to: [ggplot2::scale_fill_gradientn()]
#' when `discrete` is TRUE; [ggplot2::discrete_scale()] when `discrete` is FALSE
#' and `interpolate` is TRUE; and [ggplot2::scale_fill_manual()] when `discrete`
#' is FALSE and `interpolate` is FALSE.
#' @examples
#' library(ggplot2)
#' ggplot(mpg, aes(x = hwy, y = cty, fill = cyl)) +
#'   geom_point(shape = 21) +
#'   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#'   scale_fill_nmfs(palette = "crustacean", discrete = FALSE)
#'
#' ggplot(mtcars, aes(mpg, disp, color = as.factor(gear))) +
#'   geom_point(size = 4) +
#'   scale_fill_nmfs("regional",
#'     interpolate = FALSE,
#'     discrete = TRUE
#'   )
#' @export
scale_fill_nmfs <- function(
    palette = "oceans",
    discrete = TRUE,
    reverse = FALSE,
    interpolate = TRUE,
    ...) {
  pal <- nmfs_palette(palette = palette, reverse = reverse)

  pal_length <- length(nmfs_palettes[[palette]])

  if (discrete) {
    if (interpolate) {
      ggplot2::discrete_scale(
        aesthetics = "fill",
        palette = pal,
        ...
      )
    } else {
      cli::cli_alert_info("The {palette} palette has {pal_length} colors.")
      rlang::warn(
        message = "An error will occur if there are too few palette colors for your plot.
To avoid this error, use a larger palette or `interpolate = TRUE`.",
        .frequency = "once",
        .frequency_id = "too_few_colors_warning_fill"
      )
      ggplot2::scale_fill_manual(
        values = nmfs_palette(palette)(pal_length),
        ...
      )
    }
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}
