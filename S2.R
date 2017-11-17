library(cowplot)

cols <- c("#d73027", "#984ea3", "#fdae61", "#4575b4")

pos_ctrls <- read.csv("data/fig1bc.csv")
pos_ctrls$n_features <- factor(pos_ctrls$n_features, levels = c("0-100", "100-200", "200-500", "500-1000", "1000-2000", "2000-5000", "ALL"))
pos_ctrls$Method <- factor(pos_ctrls$Method, levels = c("scmap-cluster", "scmap-cell", "SVM", "RF"))

neg_ctrls <- read.csv("data/fig1d.csv")
neg_ctrls$n_features <- factor(neg_ctrls$n_features, levels = c("0-100", "100-200", "200-500", "500-1000", "1000-2000", "2000-5000", "ALL"))
neg_ctrls$Method <- factor(neg_ctrls$Method, levels = c("scmap-cluster", "scmap-cell", "SVM", "RF"))

p_s2a <- ggplot(pos_ctrls, aes(n_features, kappa, fill = Method)) +
    geom_boxplot(size = 0.1, width = 0.5, position = position_dodge(width = 0.7), outlier.size = 0.2) +
    facet_grid(threshold ~ .) +
    scale_fill_manual(values = cols, guide = guide_legend()) +
    theme_classic(base_size = 6) +
    theme(axis.line=element_blank(), 
          legend.position = "top",
          strip.background = element_rect(colour = "white"),
          axis.text.x=element_text(angle = -30, hjust = 0)) +
    annotate("segment", x=0, xend=Inf, y=0, yend=0, color = "black") +
    annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, color = "black") +
    labs(x = "Number of features", y = "Cohen's Kappa")

p_s2b <- ggplot(pos_ctrls, aes(n_features, uns_rate, fill = Method)) +
    geom_boxplot(size = 0.1, width = 0.5, position = position_dodge(width = 0.7), outlier.size = 0.2) +
    scale_fill_manual(values = cols, guide = guide_legend()) +
    facet_grid(threshold ~ .) +
    theme_classic(base_size = 6) +
    theme(axis.line=element_blank(), 
          legend.position = "top",
          strip.background = element_rect(colour = "white"),
          axis.text.x=element_text(angle = -30, hjust = 0)) +
    annotate("segment", x=0, xend=Inf, y=0, yend=0, color = "black") +
    annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, color = "black") +
    labs(x = "Number of features", y = "% of unassigned cells")

p_s2c <- ggplot(neg_ctrls, aes(n_features, uns_rate, fill = Method)) +
    geom_boxplot(size = 0.1, width = 0.5, position = position_dodge(width = 0.7), outlier.size = 0.2) +
    facet_grid(threshold ~ .) +
    scale_fill_manual(values = cols, guide = guide_legend()) +
    theme_classic(base_size = 6) +
    theme(axis.line=element_blank(), 
          legend.position = "top",
          strip.background = element_rect(colour = "white"),
          axis.text.x=element_text(angle = -30, hjust = 0)) +
    annotate("segment", x=0, xend=Inf, y=0, yend=0, color = "black") +
    annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, color = "black") +
    labs(x = "Number of features", y = "% of unassigned cells")

plot_grid(p_s2a, p_s2b, p_s2c, nrow = 1, labels = c("a", "b", "c"))
ggsave("pdf/S2.pdf", w = 9, h = 6)
ggsave("jpeg/S2.jpeg", w = 9, h = 6)
