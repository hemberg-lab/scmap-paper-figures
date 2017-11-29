library(scmap)
library(irr)
library(cowplot)

cols <- c("#d73027", "#984ea3", "#fdae61", "#4575b4")

accuracy <- read.csv("data/fi2b.csv")
accuracy$dataset <- factor(accuracy$dataset, levels = c("trapnell", "jang", "treutlein"))

p_2a <- ggplot(accuracy, aes(dataset, accuracy)) +
    geom_boxplot(size = 0.3, width = 0.5, outlier.size = 0.2) +
    ylim(60, 100) +
    theme_classic(base_size = 9) +
    labs(x = "", y = "scmap-cell accuracy, %")


d <- read.table("data/fig2a.txt", header = T)
d$Time <- d$Time/3600
d$Method <- factor(d$Method, levels = c("scmap-cluster", "scmap-cell", "SVM", "RF"))

p_2b <- ggplot(d, aes(n_cells_ref, Time, colour = Method, group = Method)) +
    geom_smooth(method = "loess", span = 1, se = FALSE, size = 0.5) +
    geom_point(size = 1) +
    scale_x_log10(breaks = c(100, 1000, 10000, 20000, 30000, 40000, 100000),
                  labels = c("1e+02", "1e+03", "1e+04", "2e+04", "3e+04", "4e+04", "1e+05")) +
    scale_y_log10() +
    coord_cartesian(xlim=c(99, 100000), ylim=c(0.00001,1.2)) +
    geom_hline(yintercept = 0.017, linetype = 2) +
    annotate("text", x = 1000, y = 0.03, label = "1 minute") +
    geom_hline(yintercept = 0.5, linetype = 2) +
    annotate("text", x = 1000, y = 0.95, label = "1/2 hour") +
    geom_hline(yintercept = 24, linetype = 2) +
    annotate("text", x = 1000, y = 65, label = "1 day") +
    geom_hline(yintercept = 720, linetype = 2) +
    annotate("text", x = 1000, y = 1600, label = "1 month") +
    geom_hline(yintercept = 8760, linetype = 2) +
    annotate("text", x = 1000, y = 25000, label = "1 year") +
    scale_colour_manual(values = cols) +
    theme_classic(base_size = 9) +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x=element_text(angle = -30, hjust = 0)) +
    labs(x = "# of cells", y = "Time")

first_row <- plot_grid(p_2a, p_2b, nrow = 1, labels = c("a", "b"), label_size = 12, rel_widths = c(1, 2))
second_row <- plot_grid(NULL, NULL, NULL, nrow = 1, labels = c("c", "d", "e"), label_size = 12)
plot_grid(first_row, second_row, ncol = 1, label_size = 12)
ggsave("pdf/2abcd.pdf", w = 9, h = 6)
ggsave("jpeg/2abcd.jpeg", w = 9, h = 6)


labels <- read.csv("data/fig2cd.csv")
plot(getSankey(labels$original_labs, labels$scmap_cluster, plot_width = 300, plot_height = 300))
# save to 2c.pdf

plot(getSankey(labels$original_labs, labels$scmap_cell, plot_width = 300, plot_height = 300))
# save to 2d.pdf
