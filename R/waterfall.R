#' Create waterfall charts
#'
#' @name waterfall
#' @author Based on \code{grattan_waterfall} from the 'grattanCharts' package (\url{https://github.com/HughParsonage/grattanCharts}).
#' @param .data a \code{data.frame} containing two columns, one with the values, the other with the labels
#' @param values a numeric vector making up the heights of the rectangles in the waterfall
#' @param labels the labels corresponding to each vector, marked on the x-axis
#' @param rect_text_labels (character) a character vector of the same length as values that are placed on the rectangles
#' @param rect_text_size size of the text in the rectangles
#' @param rect_text_labels_anchor (character) How should \code{rect_text_labels} be positioned? In future releases, we might have support for north or south anchors, or for directed positioning (negative down, positive up) etc. For now, only centre is supported.
#' @param put_rect_text_outside_when_value_below (numeric) the text labels accompanying a rectangle of this height will be placed outside the box: below if it's negative; above if it's positive.
#' @param calc_total (logical, default: \code{FALSE}) should the final pool of the waterfall be calculated (and placed on the chart)
#' @param total_axis_text (character) the text appearing on the axis underneath the total rectangle
#' @param total_rect_text (character) the text in the middle of the rectangle of the total rectangle
#' @param total_rect_color the color of the final rectangle
#' @param total_rect_border_color the border color of the total rectangle
#' @param total_rect_text_color the color of the final rectangle's label text
#' @param fill_colours Colours to be used to fill the rectangles, in order. Disregarded if \code{fill_by_sign} is \code{TRUE} (the default).
#' @param fill_by_sign (logical, default: \code{TRUE}) should positive and negative values each have the same colour?
#' @param rect_width (numeric) the width of the rectangle, relative to the space between each label factor
#' @param rect_border the border colour around the rectangles. Provide either a single color, that will be used for each rectangle, or one color for each rectangle. Choose \code{NA} if no border is desired.
#' @param draw_lines (logical, default: \code{TRUE}) should lines be drawn between successive rectangles
#' @param linetype the linetype for the draw_lines
#' @param lines_anchors a character vector of length two specifying the horizontal placement of the drawn lines relative to the preceding and successive rectangles, respectively
#' @param draw_axis.x (character) one of "none", "behind", "front" whether to draw an x.axis line and whether to draw it behind or in front of the rectangles, default is behind
#' @param theme_text_family (character) Passed to the \code{text} argument in \code{ggplot2::theme}.
#' @param scale_y_to_waterfall (logical, default: \code{TRUE}) Should the default range of the y-axis be from the bottom of the lowest pool to the top of the highest? If \code{FALSE}, which was the only option before version 0.1.2, the range of the plot is more balanced around the y-axis.
#' @param print_plot (logical) Whether or not the plot should be printed. By default, \code{TRUE}, which means it cannot be assigned.
#' @param ggplot_object_name (character) A quoted valid object name to which ggplot layers may be added after the function has run. Ignored if \code{print} is \code{FALSE}.
#' @examples
#' waterfall(values = round(rnorm(5), 1), labels = letters[1:5], calc_total = TRUE)
#' waterfall(.data = data.frame(category = letters[1:5],
#'                              value = c(100, -20, 10, 20, 110)), 
#'           fill_colours = colorRampPalette(c("#1b7cd6", "#d5e6f2"))(5),
#'           fill_by_sign = FALSE)
#' @export


waterfall <- function(.data = NULL,
                      values, labels,
                      rect_text_labels = values,
                      rect_text_size = 1,
                      rect_text_labels_anchor = "centre",
                      put_rect_text_outside_when_value_below = 0.05*(max(cumsum(values)) - min(cumsum(values))),
                      calc_total = FALSE,
                      total_axis_text = "Total",
                      total_rect_text = sum(values),
                      total_rect_color = "black",
                      total_rect_border_color = "black",
                      total_rect_text_color = "white",
                      fill_colours = NULL,
                      fill_by_sign = TRUE,
                      rect_width = 0.7,
                      rect_border = "black",
                      draw_lines = TRUE,
                      lines_anchors = c("right", "left"),
                      linetype = "dashed",
                      draw_axis.x = "behind",
                      theme_text_family = "",
                      scale_y_to_waterfall = TRUE,
                      print_plot = FALSE,
                      ggplot_object_name = "mywaterfall") {
  if (!is.null(.data)) {
    
    if (!is.data.frame(.data)) {
      stop("`.data` was a ", class(.data)[1], ", but must be a data.frame.")
    }
    
    if (ncol(.data) < 2L) {
      stop("`.data` had fewer than two columns, yet two are required: labels and values.")
    }
    
    dat <- as.data.frame(.data)
    char_cols <- vapply(dat, is.character, FALSE)
    factor_cols <- vapply(dat, is.factor, FALSE)
    num_cols <- vapply(dat, is.numeric, FALSE)
    
    if (!xor(num_cols[1], num_cols[2]) ||
        sum(char_cols[1:2], factor_cols[1:2], num_cols[1:2]) != 2L) {
      const_width_name <- function(noms) {
        if (is.data.frame(noms)) {
          noms <- names(noms)
        }
        max_width <- max(nchar(noms))
        formatC(noms, width = max_width)
      }
      
      stop("`.data` did not contain exactly one numeric column and exactly one character or factor ",
           "column in its first two columns.\n\t", 
           "1st column: '", const_width_name(dat)[1], "'\t", sapply(dat, class)[1], "\n\t",
           "2nd column: '", const_width_name(dat)[2], "'\t", sapply(dat, class)[2])
    }
    
    if (num_cols[1L]) {
      .data_values <- .subset2(dat, 1L)
      .data_labels <- .subset2(dat, 2L)
    } else {
      .data_values <- .subset2(dat, 2L)
      .data_labels <- .subset2(dat, 1L)
    }
    
    if (!missing(values) && !missing(labels)) {
      warning(".data and values and labels supplied, .data ignored")
    } else {
      values <- .data_values
      labels <- as.character(.data_labels)
    }
  }
  
  if (!(length(values) == length(labels) &&
        length(values) == length(rect_text_labels))) {
    stop("values, labels, fill_colours, and rect_text_labels must all have same length")
  }
  
  if (rect_width > 1)
    warning("rect_Width > 1, your chart may look terrible")
  
  number_of_rectangles <- length(values)
  north_edge <- cumsum(values)
  south_edge <- c(0, cumsum(values)[-length(values)])
  
  # fill by sign means rectangles' fill colour is given by whether they are going up or down
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    grDevices::hcl(h = hues, l = 65, c = 100)[seq_len(n)]
  }
  if(fill_by_sign){
    if (!is.null(fill_colours)){
      warning("fill_colours is given but fill_by_sign is TRUE so fill_colours will be ignored.")
    }
    fill_colours <- ifelse(values >= 0,
                           gg_color_hue(2)[2],
                           gg_color_hue(2)[1])
  } else {
    if (is.null(fill_colours)){
      fill_colours <- gg_color_hue(number_of_rectangles)
    }
  }
  
  # Check if length of rectangle border colors matches the number of rectangles
  rect_border_matching <- length(rect_border) == number_of_rectangles
  if (!(rect_border_matching || length(rect_border) == 1)) {
    stop("rect_border must be a single colour or one colour for each rectangle")
  }
  
  if(!(grepl("^[lrc]", lines_anchors[1]) && grepl("^[lrc]", lines_anchors[2])))  # left right center
    stop("lines_anchors must be a pair of any of the following: left, right, centre")
  
  if (grepl("^l", lines_anchors[1]))
    anchor_left <- rect_width / 2
  if (grepl("^c", lines_anchors[1]))
    anchor_left <- 0
  if (grepl("^r", lines_anchors[1]))
    anchor_left <- -1 * rect_width / 2
  
  if (grepl("^l", lines_anchors[2]))
    anchor_right <- -1 * rect_width / 2
  if (grepl("^c", lines_anchors[2]))
    anchor_right <- 0
  if (grepl("^r", lines_anchors[2]))
    anchor_right <- rect_width / 2
  
  if (!calc_total) {
    p <- 
      if (scale_y_to_waterfall) {
        ggplot2::ggplot(data.frame(x = c(factor(1:length(labels)), factor(1:length(labels))),
                                   y = c(south_edge, north_edge)),
                        ggplot2::aes_string(x = "x", y = "y")) 
      } else {
        ggplot2::ggplot(data.frame(x = factor(1:length(labels)), y = values),
                        ggplot2::aes_string(x = "x", y = "y"))
      }
    p <- p +
      ggplot2::geom_blank() +
      ggplot2::theme(axis.title = ggplot2::element_blank())
  } else {
    p <-
      if (scale_y_to_waterfall) {
        ggplot2::ggplot(data.frame(x = c(factor(1:length(labels)), total_axis_text,
                                         factor(1:length(labels)), total_axis_text),
                                   y = c(south_edge, north_edge,
                                         south_edge[number_of_rectangles],
                                         north_edge[number_of_rectangles])),
                        ggplot2::aes_string(x = "x", y = "y"))
      } else {
        ggplot2::ggplot(data.frame(x = c(factor(1:length(labels)), total_axis_text),
                                   y = c(values, north_edge[number_of_rectangles])),
                        ggplot2::aes_string(x = "x", y = "y"))
      } 
    p <- p +
      ggplot2::geom_blank() +
      ggplot2::theme(axis.title = ggplot2::element_blank())
  }
  
  if (grepl("behind", draw_axis.x)){
    p <- p + ggplot2::geom_hline(yintercept = 0)
  }
  
  for (i in seq_along(values)){
    p <- p + ggplot2::annotate("rect",
                               xmin = i - rect_width/2,
                               xmax = i + rect_width/2,
                               ymin = south_edge[i],
                               ymax = north_edge[i],
                               colour = rect_border[[if (rect_border_matching) i else 1]],
                               fill = fill_colours[i])
    if (i > 1 && draw_lines){
      p <- p + ggplot2::annotate("segment",
                                 x = i - 1 - anchor_left,
                                 xend = i + anchor_right,
                                 linetype = linetype,
                                 y = south_edge[i],
                                 yend = south_edge[i])
    }
  }
  
  # rect_text_labels
  
  for (i in seq_along(values)){
    if(abs(values[i]) > put_rect_text_outside_when_value_below){
      p <- p + ggplot2::annotate("text",
                                 x = i,
                                 y = 0.5 * (north_edge[i] + south_edge[i]),
                                 family = theme_text_family,
                                 label = ifelse(rect_text_labels[i] == values[i],
                                                ifelse(values[i] < 0,
                                                       paste0("\U2212", -1 * values[i]),
                                                       values[i]),
                                                rect_text_labels[i]),
                                 size = rect_text_size/(5/14))
    } else {
      p <- p + ggplot2::annotate("text",
                                 x = i,
                                 y = north_edge[i],
                                 family = theme_text_family,
                                 label = ifelse(rect_text_labels[i] == values[i],
                                                ifelse(values[i] < 0,
                                                       paste0("\U2212", -1 * values[i]),
                                                       values[i]),
                                                rect_text_labels[i]),
                                 vjust = ifelse(values[i] >= 0, -0.2, 1.2),
                                 size = rect_text_size/(5/14))
    }
  }
  
  
  if (calc_total){
    p <- p + ggplot2::annotate("rect",
                               xmin = number_of_rectangles + 1 - rect_width/2,
                               xmax = number_of_rectangles + 1 + rect_width/2,
                               ymin = 0,
                               ymax = north_edge[number_of_rectangles],
                               colour = total_rect_border_color,
                               fill = total_rect_color)  +
      ggplot2::annotate("text",
                        x = number_of_rectangles + 1,
                        y = 0.5 * north_edge[number_of_rectangles],
                        family = theme_text_family,
                        label = ifelse(total_rect_text == sum(values),
                                       ifelse(north_edge[number_of_rectangles] < 0,
                                              paste0("\U2212", -1 * north_edge[number_of_rectangles]),
                                              north_edge[number_of_rectangles]),
                                       total_rect_text),
                        color = total_rect_text_color,
                        size = rect_text_size/(5/14)) +
      ggplot2::scale_x_discrete(labels = c(labels, total_axis_text))
    if (draw_lines){
      p <- p + ggplot2::annotate("segment",
                                 x = number_of_rectangles - anchor_left,
                                 xend = number_of_rectangles + 1 + anchor_right,
                                 y = north_edge[number_of_rectangles],
                                 yend = north_edge[number_of_rectangles],
                                 linetype = linetype)
    }
  } else {
    p <- p + ggplot2::scale_x_discrete(labels = labels)
  }
  
  if (grepl("front", draw_axis.x)){
    p <- p + ggplot2::geom_hline(yintercept = 0)
  }
  if (print_plot){
    # Allow modifications beyond the function call
    if (ggplot_object_name %in% ls(.GlobalEnv))
      warning("Overwriting ", ggplot_object_name, " in global environment.")
    assign(ggplot_object_name, p, inherits = TRUE)
    print(p)
  } else {
    return(p)
  }
}
