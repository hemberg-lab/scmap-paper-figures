library(cowplot)

cols <- c("#d73027", "#984ea3", "#fdae61", "#4575b4")

pos_ctrls <- read.csv("data/fig1bc.csv")
pos_ctrls$n_features <- factor(pos_ctrls$n_features, levels = c("0-100", "100-200", "200-500", "500-1000", "1000-2000", "2000-5000", "ALL"))
pos_ctrls$Method <- factor(pos_ctrls$Method, levels = c("scmap-cluster", "scmap-cell", "SVM", "RF"))

neg_ctrls <- read.csv("data/fig1d.csv")
neg_ctrls$n_features <- factor(neg_ctrls$n_features, levels = c("0-100", "100-200", "200-500", "500-1000", "1000-2000", "2000-5000", "ALL"))
neg_ctrls$Method <- factor(neg_ctrls$Method, levels = c("scmap-cluster", "scmap-cell", "SVM", "RF"))

p_1b <- ggplot(pos_ctrls[pos_ctrls$Method != "scmap-cell" & pos_ctrls$threshold == 0.7 | pos_ctrls$Method == "scmap-cell" & pos_ctrls$threshold == 0.5, ], aes(n_features, kappa, fill = Method)) +
    geom_boxplot(size = 0.1, width = 0.5, position = position_dodge(width = 0.7), outlier.size = 0.2) +
    scale_fill_manual(values = cols, guide = guide_legend()) +
    theme_classic(base_size = 6) +
    theme(axis.line=element_blank(), 
          strip.background = element_rect(colour = "white"),
          legend.position = "top",
          legend.key.size = unit(0.22, "cm"),
          axis.text.x=element_text(angle = -30, hjust = 0)) +
    annotate("segment", x=0, xend=Inf, y=0, yend=0, color = "black") +
    annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, color = "black") +
    labs(x = "Number of features", y = "Cohen's Kappa")

p_1c <- ggplot(pos_ctrls[pos_ctrls$Method != "scmap-cell" & pos_ctrls$threshold == 0.7 | pos_ctrls$Method == "scmap-cell" & pos_ctrls$threshold == 0.5, ], aes(n_features, uns_rate, fill = Method)) +
    geom_boxplot(size = 0.1, width = 0.5, position = position_dodge(width = 0.7), outlier.size = 0.2) +
    scale_fill_manual(values = cols, guide = guide_legend()) +
    theme_classic(base_size = 6) +
    theme(axis.line=element_blank(),
          legend.position="top",
          legend.key.size = unit(0.22, "cm"),
          strip.background = element_rect(colour = "white"),
          axis.text.x=element_text(angle = -30, hjust = 0)) +
    annotate("segment", x=0, xend=Inf, y=0, yend=0, color = "black") +
    annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, color = "black") +
    labs(x = "Number of features", y = "% of unassigned cells")

p_1d <- ggplot(neg_ctrls[neg_ctrls$Method != "scmap-cell" & neg_ctrls$threshold == 0.7 | neg_ctrls$Method == "scmap-cell" & neg_ctrls$threshold == 0.5, ], aes(n_features, uns_rate, fill = Method)) +
    geom_boxplot(size = 0.1, width = 0.5, position = position_dodge(width = 0.7), outlier.size = 0.2) +
    scale_fill_manual(values = cols, guide = guide_legend()) +
    theme_classic(base_size = 6) +
    theme(axis.line=element_blank(), 
          strip.background = element_rect(colour = "white"),
          legend.position = "top",
          legend.key.size = unit(0.22, "cm"),
          axis.text.x=element_text(angle = -30, hjust = 0)) +
    annotate("segment", x=0, xend=Inf, y=0, yend=0, color = "black") +
    annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, color = "black") +
    labs(x = "Number of features", y = "% of unassigned cells")

acc <- read.csv("data/fig1e.csv")
acc$n_features <- factor(acc$n_features, levels = c("100", "200", "500", "1000", "2000", "5000"))

p_1e <- ggplot(acc,
             aes(n_features,
                 accuracy,
                 fill = factor(n_nn)
             )) +
    geom_boxplot(size = 0.1, width = 0.3, position = position_dodge(width = 0.7), outlier.size = 0.2) +
    # scale_fill_manual(values = cols) +
    theme_classic(base_size = 6) +
    theme(axis.line=element_blank(), 
          strip.background = element_rect(colour = "white"),
          legend.position = "top",
          legend.key.size = unit(0.22, "cm")) +
    annotate("segment", x=0, xend=Inf, y=0, yend=0, color = "black") +
    annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, color = "black") +
    labs(x = "Number of features", y = "scmap-cell accuracy, %") +
    guides(fill=guide_legend(title="# of NNs"))

second_col <- plot_grid(p_1b, p_1c, p_1d, p_1e, ncol = 2, labels = c("b", "c", "d", "e"), label_size = 12)
plot_grid(plot_grid(NULL, nrow = 1, labels = c("a")), second_col, ncol = 2, label_size = 12, rel_widths = c(1, 1.1))
ggsave("pdf/1bcde.pdf", w = 9, h = 6)
ggsave("jpeg/1bcde.jpeg", w = 9, h = 6)
