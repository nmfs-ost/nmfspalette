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


#' Create theme for nmfs colors
#' @inheritParams scale_color_nmfs
#' @param base_size The base font size, as defined in the
#'   [ggplot2::theme_gray()] function
#' @param lab_size The axis text size
#' @param ink The plot foreground color, as defined in the
#'   [ggplot2::theme_gray()] function
#' @param paper The plot background color, as defined in the
#'   [ggplot2::theme_gray()] function
#' @param accent The plot's accented elements' color, as defined in the
#'   [ggplot2::theme_gray()] function
#' @param ... Additional arguments passed to: [ggplot2::scale_fill_gradientn()]
#' when `discrete` is TRUE; [ggplot2::discrete_scale()] when `discrete` is FALSE
#' and `interpolate` is TRUE; and [ggplot2::scale_fill_manual()] when `discrete`
#' is FALSE and `interpolate` is FALSE.
#' @return A theme that applies nmfs color palettes and theme-related elements (such as label sizes, borders, axis lines, and more) to a plot. For more information about the ggplot2 advances that enabled this function, check out the [ggplot2 version 4.0.0 release notes](https://tidyverse.org/blog/2025/09/ggplot2-4-0-0/).
#' @examples
#' library(ggplot2)
#' ggplot(mtcars, aes(x = mpg, y = disp, color = as.factor(cyl))) +
#'   geom_point(size = 3) +
#'   theme_nmfs(discrete = TRUE, interpolate = TRUE, palette = "crustacean")
#' ggplot(mtcars, aes(x = mpg, y = disp, color = as.factor(cyl))) +
#'   geom_point(size = 3) +
#'   theme_nmfs(discrete = TRUE, interpolate = FALSE, palette = "crustacean")
#' ggplot(mtcars, aes(x = mpg, y = disp, fill = hp)) +
#' geom_point(size = 3, shape = 24) +
#' theme_nmfs(discrete = FALSE, interpolate = TRUE, palette = "seagrass")
#' @export
theme_nmfs <- function(
    palette = "oceans", 
    discrete = TRUE,
    reverse = FALSE,
    interpolate = TRUE,
    base_size = 14,
    lab_size = 12,
    ink = "black",
    paper = "white",
    accent = "#003087",
    ...) {
  if (utils::packageVersion("ggplot2") < "4.0.0"){
    rlang::warn(
      message = paste0("Your `ggplot2` version is ", utils::packageVersion("ggplot2"), ", which is older than the version required to use `theme_nmfs()` (4.0.0). Update your `ggplot2` package to use this new function!"),
      .frequency = "once",
      .frequency_id = "ggplot2_version_warning"
    )    
  }

  
  # get palette
  pal <- nmfs_palette(palette = palette,
                      reverse = reverse)
  
  pal_length <- length(nmfs_palettes[[palette]])
  
  base_theme1 <- ggplot2::theme_bw(
    base_size = base_size,
    ink = ink,
    paper = paper,
    accent = accent,
    ...
  )
  
  base_theme2 <- ggplot2::theme(
    axis.line = element_line(color = "black",
                             linewidth = 0.75),
    axis.text.x = element_text(size = lab_size, 
                               color = "black"),
    axis.text.y = element_text(size = lab_size, 
                               color = "black"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    ...
  )
  
  if (discrete) {
    if (interpolate) {
      
      base_theme1 +
      base_theme2 +
      ggplot2::theme(
         palette.colour.discrete = pal,
         palette.fill.discrete = pal
        )

    } else {
      cli::cli_alert_info("The {palette} palette has {pal_length} colors.")
      rlang::warn(
        message = "An error will occur if there are too few palette colors for your plot.
To avoid this error, use a larger palette or `interpolate = TRUE`.",
        .frequency = "once",
        .frequency_id = "too_few_colors_warning_fill"
      )
      
      base_theme1 +
      base_theme2 +
        ggplot2::theme(
          palette.colour.discrete = nmfs_palette(palette)(pal_length),
          palette.fill.discrete = nmfs_palette(palette)(pal_length)
        )
    }
  } else {
    base_theme1 +
    base_theme2 +
    ggplot2::theme(
      palette.color.continuous = nmfs_palette(palette)(pal_length),
      palette.fill.continuous = nmfs_palette(palette)(pal_length)
    )
  }
}
