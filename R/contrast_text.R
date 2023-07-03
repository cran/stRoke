

#' @title Contrast Text Color
#' @description Calculates the best contrast text color for a given 
#' background color.
#' @param background A hex/named color value that represents the background.
#' @param light_text A hex/named color value that represents the light text 
#' color.
#' @param dark_text A hex/named color value that represents the dark text color.
#' @param threshold A numeric value between 0 and 1 that is used to determine 
#' the luminance threshold of the background color for text color.
#' @param method A character string that specifies the method for calculating 
#' the luminance. Three different methods are available: 
#' c("relative","perceived","perceived_2")
#' @param ... parameter overflow. Ignored.
#' @details
#' This function aids in deciding the font color to print on a given background.
#' The function is based on the example provided by teppo: 
#' https://stackoverflow.com/a/66669838/21019325.
#' The different methods provided are based on the methods outlined in the 
#' StackOverflow thread: 
#' https://stackoverflow.com/questions/596216/formula-to-determine-perceived-brightness-of-rgb-color
#' @return A character string that contains the best contrast text color.
#' @examples
#' contrast_text(c("#F2F2F2", "blue"))
#' 
#' contrast_text(c("#F2F2F2", "blue"), method="relative")
#' @export
#' 
#' @importFrom grDevices col2rgb
#'
contrast_text <- function(background,
                          light_text = 'white',
                          dark_text = 'black',
                          threshold = 0.5,
                          method = "perceived_2",
                          ...) {
  if (method == "relative") {
    luminance <-
      c(c(.2126, .7152, .0722) %*% grDevices::col2rgb(background) / 255)
  } else if (method == "perceived") {
    luminance <-
      c(c(.299, .587, .114) %*% grDevices::col2rgb(background) / 255)
  } else if (method == "perceived_2") {
    luminance <- c(sqrt(colSums((
      c(.299, .587, .114) * grDevices::col2rgb(background)
    ) ^ 2)) / 255)
  }
  
  ifelse(luminance < threshold,
         light_text,
         dark_text)
}

#' Plot color examples with contrasting text
#'
#' Plots color examples with contrasting text. Parameters are passed to 
#' contrast_text.
#' @param colors Vector of colors to plot
#' @param labels Show color names. Default is TRUE
#' @param borders Border parameter for 'rect()' function. Default is NULL
#' @param cex_label Label size. Default is 1.
#' @param ncol Desired number of columns. Default is ceiling of square root to
#' the length of 'colors' vector provided.
#' @param ... Parameters for the 
#'
#' @return base plot
#' @export
#' 
#' @importFrom graphics par rect text
#' 
#' @examples
#' par(bg=NULL)
#' colors <- sample(colors(),size = 20)
#' color_plot(colors, method="relative")
#' 
color_plot <-
  function (colors,
            labels = TRUE,
            borders = NULL,
            cex_label = 1,
            ncol = NULL,
            ...){
    n <- length(colors)
    ncol <- if (is.null(ncol)) ceiling(sqrt(length(colors))) else ncol
    nrow <- ceiling(n / ncol)
    colors <- c(colors, rep(NA, nrow * ncol - length(colors)))
    colors <- matrix(colors, ncol = ncol, byrow = TRUE)
    old <- par(pty = "s", mar = c(0, 0, 0, 0))
    on.exit(par(old))
    size <- max(dim(colors))
    plot(
      c(0, size),
      c(0, -size),
      type = "n",
      xlab = "",
      ylab = "",
      axes = FALSE
    )
    rect(
      col(colors) - 1,
      -row(colors) + 1,
      col(colors),
      -row(colors),
      col = colors,
      border = borders
    )
    if (labels) {
      label_col <- contrast_text(colors,...)
      text(col(colors) - 0.5,
           -row(colors) + 0.5,
           colors,
           cex = cex_label,
           col = label_col)
    }
  }

