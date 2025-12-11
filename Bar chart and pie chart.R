
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(cowplot)) install.packages("cowplot")
if (!require(scales)) install.packages("scales")
if (!require(reshape2)) install.packages("reshape2")

library(ggplot2)
library(cowplot)
library(scales)
library(reshape2)


data <- data.frame(
  Component = c("LDOC", "SLDOC", "RDOC"),
  Period1_Concentration = c(2.22,0.65,0),  
  Period2_Concentration = c(3.65,0.58,1.21), 
  Period3_Concentration = c(0.30,1.17,1.26),  
  Period1_Percentage = c(0.769,0.231,0),     
  Period2_Percentage = c(0.647,0.123,0.230),   
  Period3_Percentage = c(0.112,0.426,0.462)    
)


colors <- c("#FF6B6B", "#4ECDC4", "#45B7D1")

conc_data <- melt(data[, 1:4], id.vars = "Component")
conc_data$Period <- gsub("_Concentration", "", conc_data$variable)
conc_data$Period <- factor(conc_data$Period, 
                           levels = c("Period1", "Period2", "Period3"))


bar_plot <- ggplot(conc_data, aes(x = Period, y = value, fill = Component)) +
  geom_col(position = "dodge", alpha = 0.8, color = "white", linewidth = 0.5) +
  scale_fill_manual(values = colors) +
  labs(title = "Comparison of DOC component concentrations in the three periods", y = " (mg/L)", x = "Peiord", fill = "Component") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "top"
  ) +
  geom_text(aes(label = round(value, 1)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, size = 3.5, fontface = "bold")


create_pie_chart <- function(period) {

  pie_data <- data.frame(
    Component = data$Component,
    Percentage = data[[paste0(period, "_Percentage")]]
  )
  

  pie_data$ypos <- cumsum(pie_data$Percentage) - 0.5 * pie_data$Percentage
  

  pie <- ggplot(pie_data, aes(x = "", y = Percentage, fill = Component)) +
    geom_col(color = "white") +
    coord_polar("y", start = 0) +
    scale_fill_manual(values = colors) +
    geom_text(aes(y = ypos, label = percent(Percentage)), 
              color = "white", size = 4, fontface = "bold") +
    labs(title = period, fill = "Component") +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold", margin = margin(b = 10)),
      legend.position = "none"
    )
  
  return(pie)
}


pie1 <- create_pie_chart("Period1")
pie2 <- create_pie_chart("Period2")
pie3 <- create_pie_chart("Period3")

pies_row <- plot_grid(
  pie1, pie2, pie3,
  nrow = 1,
  labels = c("A", "B", "C")
)


combined_plot <- plot_grid(
  pies_row, bar_plot,
  ncol = 1,
  rel_heights = c(1, 1.5),
  labels = c("", "D")
)


title <- ggdraw() + 
  draw_label("Analysis of DOC Component Concentration and Proportion in Water Bodies during Three Periods", 
             fontface = 'bold', size = 18)

final_plot <- plot_grid(
  title, combined_plot,
  ncol = 1,
  rel_heights = c(0.1, 1)
)


print(final_plot)


# ggsave("doc_concentration_pie_bar.png", final_plot, width = 12, height = 10, dpi = 300)