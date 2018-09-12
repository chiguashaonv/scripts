# Calculate correlation between the two variables
# cor <- cor.test(adra$gene, adra$beta,method="spearman")

plotScatterBoxplot <- function(data,ylab){
  cor.res=cor.test(data[,1],data[,2],method="pearson")
  coefficient <- paste0("cor = ",signif(cor.res$estimate,digits = 2))
  Q <- paste0("P = ",signif(cor.res$p.value,digits=2))
  p1 <- ggplot(na.omit(data), aes(x=xvalues, y=gene)) +
    geom_jitter(aes(color=factor(subtype)),pch = 20, size = 4,position = position_jitter(width = 0.1, height = 0.1)) +
    geom_smooth(aes(color="grey"),method="lm")+
    scale_x_continuous(expand = c(0, 2)) +
    scale_y_continuous(expand = c(0, 2)) +
    geom_text(y = max(na.omit(data)$gene), x = max(na.omit(data)$xvalues)-5,label = coefficient, size = 4) +
    geom_text(y = max(na.omit(data)$gene)-2, x= max(na.omit(data)$xvalues)-5,label = Q, size = 4) +
    # geom_text(y = max(gene), x = max(xvalues)-0.1,label = coefficient, size = 4) +
    # geom_text(y = max(gene)-1, x= max(xvalues)-0.1,label = Q, size = 4) +
    # geom_rect(ymin = 11, ymax = 12, xmin = 0.6, xmax = 0.9, colour = "black", fill = NA) +
    # expand_limits(x = c(-0.75, 10.75), y = c(-0.075, 1.075)) +
    expand_limits(y = c(min(na.omit(data)$gene) - 0.1 * diff(range(na.omit(data)$gene)), max(na.omit(data)$gene) + 0.1 * diff(range(na.omit(data)$gene)))) +
    expand_limits(x = c(min(na.omit(data)$xvalues) - 0.1 * diff(range(na.omit(data)$xvalues)), max(na.omit(data)$xvalues) + 0.1 * diff(range(na.omit(data)$xvalues)))) +
    scale_color_manual(values=c("#7bbda2","#ee875c","#C2C2C2","#97a4d0","#1678b5"))+
    labs(x="Latency 1st Entrance to Target",y=paste(ylab,"gene expression"))+
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
      legend.position="none",
      panel.grid.major = element_blank(),
      panel.border = element_rect(colour = "black"),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold", angle = 90),
      plot.margin= unit(c(.01, .01, 0.5, 0.5), "lines"))

  theme_remove_all <- theme(panel.border = element_rect(colour = "black"),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.title = element_blank(),
  axis.ticks =  element_blank(),
  legend.position="none",
  axis.text=element_blank(),
  # axis.ticks.margin = unit(0, "lines"),
  # axis.text = element_text(margin=unit(0, "lines")),
  axis.ticks.length = unit(0, "cm"))

  p2 <- ggplot(na.omit(data), aes(x = factor(subtype), y = xvalues)) +
     geom_boxplot(aes(x=factor(subtype),y=xvalues,fill=factor(subtype)),width = .8, outlier.colour = NA) +
     geom_beeswarm(size=1)+
     stat_compare_means(comparisons=list(c("DS_PBS","DS_VPA")))+
     # geom_jitter(pch = 20, size = 1, colour = "red", alpha = .3,position = position_jitter(width = 0.15, height = 0.15)) +
     scale_y_continuous(expand = c(0, 2)) +
     # expand_limits(y = c(-0.75, 10.75))  +
     expand_limits(y = c(min(na.omit(data)$xvalues) - 0.1 * diff(range(na.omit(data)$xvalues)), max(na.omit(data)$xvalues) + 0.1* diff(range(na.omit(data)$xvalues)))) +
     scale_fill_manual(values=c("#7bbda2","#ee875c","#97a4d0","#1678b5"))+
     theme_bw() +
     theme_remove_all +
     coord_flip() +
     theme(plot.margin= unit(c(0, 0.5, 0.5, 0), "lines"))

  p3 <- ggplot(na.omit(data), aes(x = factor(subtype), y = gene)) +
     geom_boxplot(aes(x=factor(subtype),y=gene,fill=factor(subtype)),width = .8, outlier.colour = NA) +
     geom_beeswarm(size=1)+
     stat_compare_means(comparisons=list(c("DS_PBS","DS_VPA")))+
     # geom_jitter(pch = 20, size = 1, colour = "red", alpha = .3,position = position_jitter(width = 0.15, height = 0.15)) +
     scale_y_continuous(expand = c(0, 2)) +
     # expand_limits(y = c(-0.75, 10.75))  +
     expand_limits(y = c(min(na.omit(data)$gene) - 0.1 * diff(range(na.omit(data)$gene)), max(na.omit(data)$gene) + 0.1 * diff(range(na.omit(data)$gene)))) +
     scale_fill_manual(values=c("#7bbda2","#ee875c","#97a4d0","#1678b5"))+
     theme_bw() +
     theme_remove_all +
     theme(plot.margin= unit(c(0, 0.5, 0.5, 0), "lines"))


  gt1 <- ggplot_gtable(ggplot_build(p1))
  gt2 <- ggplot_gtable(ggplot_build(p2))
  gt3 <- ggplot_gtable(ggplot_build(p3))
  # Get maximum widths and heights for x-axis and y-axis title and text
  maxWidth = unit.pmax(gt1$widths[2:3], gt2$widths[2:3])
  maxHeight = unit.pmax(gt1$heights[4:5], gt3$heights[4:5])
  # Set the maximums in the gtables for gt1, gt2 and gt3
  gt1$widths[2:3] <- as.list(maxWidth)
  gt2$widths[2:3] <- as.list(maxWidth)
  gt1$heights[4:5] <- as.list(maxHeight)
  gt3$heights[4:5] <- as.list(maxHeight)
  # Combine the scatterplot with the two marginal boxplots
  # Create a new gtable
  gt <- gtable(widths = unit(c(7, 1), "null"), height = unit(c(1, 7), "null"))
  # Instert gt1, gt2 and gt3 into the new gtable
  gt <- gtable_add_grob(gt, gt1, 2, 1)
  gt <- gtable_add_grob(gt, gt2, 1, 1)
  gt <- gtable_add_grob(gt, gt3, 2, 2)

  # grid.newpage()
  grid.draw(gt)
}



# Main scatterplot (with correlation insert)
# p1 <- ggplot(adra, aes(beta, gene)) +
#    geom_jitter(aes(color=factor(subtype)),pch = 20, size = 4,position = position_jitter(width = 0.1, height = 0.1)) +
#    geom_smooth(aes(color="grey"),method="lm")+
#    scale_x_continuous(expand = c(0, 0)) +
#    scale_y_continuous(expand = c(0, 0)) +
#    geom_text(y = (11.5 + 12)/2, x = (0.65 + 0.8)/2,label = paste("rho = ", round(cor$estimate, 2), sep = ""), size = 4) +
#    geom_text(y = (11 + 11.5)/2, x= (0.65 + 0.8)/2,label = paste("P = ", cor$p.value, sep = ""), size = 4) +
#    geom_rect(ymin = 11, ymax = 12, xmin = 0.6, xmax = 0.9, colour = "black", fill = NA) +
#    # expand_limits(x = c(-0.75, 10.75), y = c(-0.075, 1.075)) +
#    expand_limits(y = c(min(adra$gene) - 0.1 * diff(range(adra$gene)), max(adra$gene) + 0.1 * diff(range(adra$gene)))) +
#    expand_limits(x = c(min(adra$beta) - 0.1 * diff(range(adra$beta)), max(adra$beta) + 0.1 * diff(range(adra$beta)))) +
#    scale_color_manual(values=c("#C2C2C2","#EB6363","#1EB5B8"))+
#    labs(x="Beta-values",y="log2(RPKM+1)")+
#    theme_bw() +
#    theme(panel.grid.minor = element_blank(),
#     legend.position="none",
#     panel.grid.major = element_blank(),
#     panel.border = element_rect(colour = "black"),
#     axis.title.x = element_text(face = "bold"),
#     axis.title.y = element_text(face = "bold", angle = 90),
#     plot.margin= unit(c(.01, .01, 0.5, 0.5), "lines"))



# To remove all axis labelling and marks from the two marginal plots
# theme_remove_all <- theme(panel.border = element_rect(colour = "black"),
#   panel.grid.minor = element_blank(),
#   panel.grid.major = element_blank(),
#   axis.title = element_blank(),
#   axis.ticks =  element_blank(),
#   legend.position="none",
#   axis.text=element_blank(),
#   # axis.ticks.margin = unit(0, "lines"),
#   # axis.text = element_text(margin=unit(0, "lines")),
#   axis.ticks.length = unit(0, "cm"))

# Horizontal marginal boxplot - to appear at the top of the chart
# p2 <- ggplot(adra, aes(aes(x = factor(subtype)), y = beta)) +
#     geom_boxplot(aes(x=factor(subtype),y=beta,fill=factor(subtype)),width = .8, outlier.colour = NA) +
#     # geom_jitter(pch = 20, size = 1, colour = "red", alpha = .3,position = position_jitter(width = 0.15, height = 0.15)) +
#     scale_y_continuous(expand = c(0, 0)) +
#     # expand_limits(x = c(-0.75, 10.75))  +
#     expand_limits(x = c(min(adra$beta) - 0.1 * diff(range(adra$beta)), max(adra$beta) + 0.1 * diff(range(adra$beta)))) +
#     scale_fill_manual(values=c("#EB6363","#1EB5B8"))+
#     coord_flip() +
#     theme_bw() +
#     theme_remove_all +
#     theme(plot.margin= unit(c(0.5, 0, 0, 0.5), "lines"))


# Vertical marginal boxplot - to appear at the right of the chart
# p3 <- ggplot(adra, aes(aes(x = factor(subtype)), y = gene)) +
#      geom_boxplot(aes(x=factor(subtype),y=gene,fill=factor(subtype)),width = .8, outlier.colour = NA) +
#      # geom_jitter(pch = 20, size = 1, colour = "red", alpha = .3,position = position_jitter(width = 0.15, height = 0.15)) +
#      scale_y_continuous(expand = c(0, 0)) +
#      # expand_limits(y = c(-0.75, 10.75))  +
#      expand_limits(y = c(min(adra$gene) - 0.1 * diff(range(adra$gene)), max(adra$gene) + 0.1 * diff(range(adra$gene)))) +
#      scale_fill_manual(values=c("#EB6363","#1EB5B8"))+
#      theme_bw() +
#      theme_remove_all +
#      theme(plot.margin= unit(c(0, 0.5, 0.5, 0), "lines"))

# # Get the gtables
# gt1 <- ggplot_gtable(ggplot_build(p1))
# gt2 <- ggplot_gtable(ggplot_build(p2))
# gt3 <- ggplot_gtable(ggplot_build(p3))

# # Get maximum widths and heights for x-axis and y-axis title and text
# maxWidth = unit.pmax(gt1$widths[2:3], gt2$widths[2:3])
# maxHeight = unit.pmax(gt1$heights[4:5], gt3$heights[4:5])

# # Set the maximums in the gtables for gt1, gt2 and gt3
# gt1$widths[2:3] <- as.list(maxWidth)
# gt2$widths[2:3] <- as.list(maxWidth)

# gt1$heights[4:5] <- as.list(maxHeight)
# gt3$heights[4:5] <- as.list(maxHeight)

# # Combine the scatterplot with the two marginal boxplots
# # Create a new gtable
# gt <- gtable(widths = unit(c(7, 1), "null"), height = unit(c(1, 7), "null"))

# # Instert gt1, gt2 and gt3 into the new gtable
# gt <- gtable_add_grob(gt, gt1, 2, 1)
# gt <- gtable_add_grob(gt, gt2, 1, 1)
# gt <- gtable_add_grob(gt, gt3, 2, 2)

# And render the plot
# grid.newpage()
# grid.draw(gt)

# grid.rect(x = 0.5, y = 0.5, height = 0.995, width = 0.995, default.units = "npc",
#     gp = gpar(col = "black", fill = NA, lwd = 1))
# # dev.off()
