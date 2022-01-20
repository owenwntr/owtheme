ow_colors <- c(
  `conservative`        = "#0087DC",
  `con`        = "#0087DC",
  `cons`        = "#0087DC",
  `labour`      = "#E4003B",
  `lab`      = "#E4003B",
  `libdem`       = "#FAA61A",
  `ld`       = "#FAA61A",
  `LD`       = "#FAA61A",
  `greenparty`     = "#6AB023",
  `greens`     = "#6AB023",
  `green`     = "#6AB023",
  `snp`     = "#FDF38E",
  `plaid` = "#005B54",
  `reform uk` = "#12B6CF",
  `reform` = "#12B6CF",
  `light blue` = "#CBE2E1",
  `red` = "#C72127",
  `teal` = "#01a08a",
  `dark yellow` = "#FAA61A",
  `dark yellow` = "#FAA61A",
  `light yellow` = "#ebcc2a",
  `dark blue` = "#3c9ab2",
  `pink` = "#f4b5bd")

#' Function to extract useful colors as hex codes
#'
#' @param ... Character names of ow_colors 
#' @examples
#' 
#' ow_cols()
#' ow_cols("labour")
#' ow_cols("conservative", "lab", "greens")
#'

ow_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (ow_colors)
  
  unname(ow_colors[cols])
}

ow_palettes <- list(
  
  `lightblue_red`  = ow_cols("light blue", "red"),
  `labcon`  = ow_cols("lab", "con"),
  `labcon_diverging`  = c(ow_cols("lab"),"white",ow_cols("con")),
  `main` = ow_cols("red","light yellow","teal","pink","dark blue","dark yellow","light blue"),
  `gradient` = ow_cols("dark blue","light blue","light yellow","dark yellow","red")
  
)

#' Return function to interpolate a ow color palette
#'
#' @param palette Character name of palette in ow_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
#'

ow_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- ow_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}

