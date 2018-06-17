############################# Double Y axis #####################
hinvert_title_grob <- function(grob){
  # Swap the widths
  widths <- grob$widths
  grob$widths[1] <- widths[3]
  grob$widths[3] <- widths[1]
  grob$vp[[1]]$layout$widths[1] <- widths[3]
  grob$vp[[1]]$layout$widths[3] <- widths[1]

  # Fix the justification
  grob$children[[1]]$hjust <- 1 - grob$children[[1]]$hjust
  grob$children[[1]]$vjust <- 1 - grob$children[[1]]$vjust
  grob$children[[1]]$x <- unit(1, "npc") - grob$children[[1]]$x
  grob
}

plot_theme <- function(p) {
  plyr::defaults(p$theme, theme_get())
}

double_y_axis <- function(p1,p2){
  # Get the ggplot grobs
  g1 <- ggplotGrob(p2)
  g2 <- ggplotGrob(p1)
  # Get the locations of the plot panels in g1.
  pp <- c(subset(g1$layout, grepl("panel", g1$layout$name), se = t:r))
  # Overlap panels for second plot on those of the first plot
  g <- gtable_add_grob(g1, g2$grobs[grepl("panel", g1$layout$name)],
                       pp$t, pp$l, pp$b, pp$l)
  # ggplot contains many labels that are themselves complex grob;
  # usually a text grob surrounded by margins.
  # When moving the grobs from, say, the left to the right of a plot,
  # Make sure the margins and the justifications are swapped around.
  # The function below does the swapping.
  # Taken from the cowplot package:
  # https://github.com/wilkelab/cowplot/blob/master/R/switch_axis.R

  # Get the y axis title from g2
  index <- which(g2$layout$name == "ylab-l") # Which grob contains the y axis title?   EDIT HERE
  ylab <- g2$grobs[[index]]                # Extract that grob
  ylab <- hinvert_title_grob(ylab)         # Swap margins and fix justifications
  # Put the transformed label on the right side of g1
  g <- gtable_add_cols(g, g2$widths[g2$layout[index, ]$l], max(pp$r))
  g <- gtable_add_grob(g, ylab, max(pp$t), max(pp$r) + 1, max(pp$b), max(pp$r) + 1, clip = "off", name = "ylab-r")
  # Get the y axis from g2 (axis line, tick marks, and tick mark labels)
  index <- which(g2$layout$name == "axis-l-1")  # Which grob.    EDIT HERE
  yaxis <- g2$grobs[[index]]                    # Extract the grob

  # yaxis is a complex of grobs containing the axis line, the tick marks, and the tick mark labels.
  # The relevant grobs are contained in axis$children:
  #   axis$children[[1]] contains the axis line;
  #   axis$children[[2]] contains the tick marks and tick mark labels.
  # First, move the axis line to the left
  # But not needed here
  # yaxis$children[[1]]$x <- unit.c(unit(0, "npc"), unit(0, "npc"))

  # Second, swap tick marks and tick mark labels
  ticks <- yaxis$children[[2]]
  ticks$widths <- rev(ticks$widths)
  ticks$grobs <- rev(ticks$grobs)

  # Third, move the tick marks
  # Tick mark lengths can change.
  # A function to get the original tick mark length
  # Taken from the cowplot package:
  # https://github.com/wilkelab/cowplot/blob/master/R/switch_axis.R

  tml <- plot_theme(p2)$axis.ticks.length   # Tick mark length
  ticks$grobs[[1]]$x <- ticks$grobs[[1]]$x - unit(1, "npc") + tml
  # Fourth, swap margins and fix justifications for the tick mark labels
  ticks$grobs[[2]] <- hinvert_title_grob(ticks$grobs[[2]])

  # Fifth, put ticks back into yaxis
  yaxis$children[[2]] <- ticks
  # Put the transformed yaxis on the right side of g1
  g <- gtable_add_cols(g, g2$widths[g2$layout[index, ]$l], max(pp$r))
  g <- gtable_add_grob(g, yaxis, max(pp$t), max(pp$r) + 1, max(pp$b), max(pp$r) + 1,
                       clip = "off", name = "axis-r")
  # Get the legends
  # leg1 <- g1$grobs[[which(g1$layout$name == "guide-box")]]
  # leg2 <- g2$grobs[[which(g2$layout$name == "guide-box")]]

  # Combine the legends
  # g$grobs[[which(g$layout$name == "guide-box")]] <-
  #   gtable:::cbind_gtable(leg1, leg2, "first")
  grid.draw(g)
}

double_y_axis_1 <- function(p1, p2){
  g1 <- ggplot_gtable(ggplot_build(p2))
  g2 <- ggplot_gtable(ggplot_build(p1))

  # overlap the panel of 2nd plot on that of 1st plot
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, pp$l, pp$b, pp$l)

  # axis tweaks
  ia <- which(g2$layout$name == "axis-l")
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
  g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

  # draw it
  grid.draw(g)
  return(g)
}
