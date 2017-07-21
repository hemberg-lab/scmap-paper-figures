library(cowplot)

cols1 <- c("#d73027", "#4575b4")

f_select <- read.csv("data/fig1a.csv")
f_select$Dataset <- factor(f_select$Dataset, levels = c("Pollen", "Baron (human)", "Macosko"))
f_select$Features <- factor(f_select$Features, levels = c("Selected", "Other"))
lm_fit <- unique(f_select[,5:7])
p_s1a <- ggplot(f_select, aes(x = expression, y = dropouts, colour = Features)) + 
    facet_grid(. ~ Dataset, scales = "free_x") +
    geom_point(size = 0.7) + scale_colour_manual(values = cols1) + 
    labs(x = "log2(Expression)", y = "log2(% of dropouts)") + 
    geom_abline(data = lm_fit, aes(intercept = lm_intercept, slope = lm_slope)) + 
    theme_classic() +
    theme(axis.line=element_blank(), strip.background = element_rect(colour = "white")) +
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, color = "black")+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, color = "black")

cols <- c("#d73027", "#fdae61", "#4575b4")

self_proj <- read.csv("data/fig1b.csv")
self_proj$n_features <- factor(self_proj$n_features, levels = c("0-100", "100-200", "200-500", "500-1000", "1000-2000", "2000-5000", "ALL"))
self_proj$Features <- factor(self_proj$Features, levels = c("dropout", "HVG", "random"))
self_proj$Method <- factor(self_proj$Method, levels = c("scmap", "SVM", "RF"))

p_s1b <- ggplot(self_proj, aes(n_features, kappa, fill = Method)) +
    geom_boxplot(size = 0.1, width = 0.5, position = position_dodge(width = 0.7), outlier.size = 0.2) +
    facet_grid(. ~ Features) +
    scale_fill_manual(values = cols) +
    ylim(-0.4, 1) +
    theme_classic() +
    theme(axis.line=element_blank(), 
          strip.background = element_rect(colour = "white"),
          axis.text.x=element_text(angle = -30, hjust = 0)) +
    annotate("segment", x=0, xend=Inf, y=0, yend=0, color = "black") +
    annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, color = "black") +
    labs(x = "Number of features", y = "Cohen's Kappa")

plot_grid(p_s1a, p_s1b, ncol = 1, labels = c("a", "b"))

ggsave("figS1.png", w = 9, h = 6)

pos_ctrls <- read.csv("data/fig1cd.csv")
pos_ctrls$n_features <- factor(pos_ctrls$n_features, levels = c("0-100", "100-200", "200-500", "500-1000", "1000-2000", "2000-5000", "ALL"))
pos_ctrls$Method <- factor(pos_ctrls$Method, levels = c("scmap", "SVM", "RF"))

p_s2a <- ggplot(pos_ctrls, aes(n_features, kappa, fill = Method)) +
    geom_boxplot(size = 0.1, width = 0.5, position = position_dodge(width = 0.7), outlier.size = 0.2) +
    facet_grid(threshold ~ .) +
    scale_fill_manual(values = cols, guide = guide_legend()) +
    theme_classic() +
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
    theme_classic() +
    theme(axis.line=element_blank(), 
          legend.position = "top",
          strip.background = element_rect(colour = "white"),
          axis.text.x=element_text(angle = -30, hjust = 0)) +
    annotate("segment", x=0, xend=Inf, y=0, yend=0, color = "black") +
    annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, color = "black") +
    labs(x = "Number of features", y = "% of unassigned cells")

p_1c <- ggplot(pos_ctrls[pos_ctrls$threshold == 0.7, ], aes(n_features, kappa, fill = Method)) +
    geom_boxplot(size = 0.1, width = 0.5, position = position_dodge(width = 0.7), outlier.size = 0.2) +
    scale_fill_manual(values = cols, guide = guide_legend()) +
    theme_classic(base_size = 7) +
    theme(axis.line=element_blank(), 
          strip.background = element_rect(colour = "white"),
          legend.position = "top",
          axis.text.x=element_text(angle = -30, hjust = 0)) +
    annotate("segment", x=0, xend=Inf, y=0, yend=0, color = "black") +
    annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, color = "black") +
    labs(x = "Number of features", y = "Cohen's Kappa")

p_1d <- ggplot(pos_ctrls[pos_ctrls$threshold == 0.7, ], aes(n_features, uns_rate, fill = Method)) +
    geom_boxplot(size = 0.1, width = 0.5, position = position_dodge(width = 0.7), outlier.size = 0.2) +
    scale_fill_manual(values = cols, guide = guide_legend()) +
    theme_classic(base_size = 7) +
    theme(axis.line=element_blank(),
          legend.position="top",
          strip.background = element_rect(colour = "white"),
          axis.text.x=element_text(angle = -30, hjust = 0)) +
    annotate("segment", x=0, xend=Inf, y=0, yend=0, color = "black") +
    annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, color = "black") +
    labs(x = "Number of features", y = "% of unassigned cells")

neg_ctrls <- read.csv("data/fig1e.csv")
neg_ctrls$n_features <- factor(neg_ctrls$n_features, levels = c("0-100", "100-200", "200-500", "500-1000", "1000-2000", "2000-5000", "ALL"))
neg_ctrls$Method <- factor(neg_ctrls$Method, levels = c("scmap", "SVM", "RF"))

p_1e <- ggplot(neg_ctrls[neg_ctrls$threshold == 0.7, ], aes(n_features, uns_rate, fill = Method)) +
    geom_boxplot(size = 0.1, width = 0.5, position = position_dodge(width = 0.7), outlier.size = 0.2) +
    scale_fill_manual(values = cols, guide = guide_legend()) +
    theme_classic(base_size = 7) +
    theme(axis.line=element_blank(), 
          strip.background = element_rect(colour = "white"),
          legend.position = "top",
          axis.text.x=element_text(angle = -30, hjust = 0)) +
    annotate("segment", x=0, xend=Inf, y=0, yend=0, color = "black") +
    annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, color = "black") +
    labs(x = "Number of features", y = "% of unassigned cells")

p_s2c <- ggplot(neg_ctrls, aes(n_features, uns_rate, fill = Method)) +
    geom_boxplot(size = 0.1, width = 0.5, position = position_dodge(width = 0.7), outlier.size = 0.2) +
    facet_grid(threshold ~ .) +
    scale_fill_manual(values = cols, guide = guide_legend()) +
    theme_classic() +
    theme(axis.line=element_blank(), 
          legend.position = "top",
          strip.background = element_rect(colour = "white"),
          axis.text.x=element_text(angle = -30, hjust = 0)) +
    annotate("segment", x=0, xend=Inf, y=0, yend=0, color = "black") +
    annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, color = "black") +
    labs(x = "Number of features", y = "% of unassigned cells")

plot_grid(p_s2a, p_s2b, p_s2c, nrow = 1, labels = c("a", "b", "c"))
ggsave("figS2.png", w = 9, h = 6)

second_row <- plot_grid(p_1c, p_1d, p_1e, nrow = 1, labels = c("c", "d", "e"))
plot_grid(plot_grid(NULL, NULL, nrow = 1, labels = c("a", "b")), second_row, ncol = 1, rel_heights = c(2.3, 2))
ggsave("fig1.pdf", w = 9, h = 6)

downsampling <- read.csv("data/figS3ab.csv")
downsampling$n_features <- factor(downsampling$n_features, levels = c("0-100", "100-200", "200-500", "500-1000", "1000-2000", "2000-5000", "ALL"))

p_s3a <- ggplot(downsampling, aes(n_features, kappa, fill = Method)) +
    geom_boxplot(size = 0.1, width = 0.5, position = position_dodge(width = 0.7), outlier.size = 0.2) +
    facet_grid(downsample_frac ~ .) +
    scale_fill_manual(values = cols, guide = guide_legend()) +
    theme_classic() +
    theme(axis.line=element_blank(), 
          legend.position = "top",
          strip.background = element_rect(colour = "white"),
          axis.text.x=element_text(angle = -30, hjust = 0)) +
    annotate("segment", x=0, xend=Inf, y=0, yend=0, color = "black") +
    annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, color = "black") +
    labs(x = "Number of features", y = "Cohen's Kappa")

p_s3b <- ggplot(downsampling, aes(n_features, uns_rate, fill = Method)) +
    geom_boxplot(size = 0.1, width = 0.5, position = position_dodge(width = 0.7), outlier.size = 0.2) +
    scale_fill_manual(values = cols, guide = guide_legend()) +
    facet_grid(downsample_frac ~ .) +
    theme_classic() +
    theme(axis.line=element_blank(), 
          legend.position = "top",
          strip.background = element_rect(colour = "white"),
          axis.text.x=element_text(angle = -30, hjust = 0)) +
    annotate("segment", x=0, xend=Inf, y=0, yend=0, color = "black") +
    annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, color = "black") +
    labs(x = "Number of features", y = "% of unassigned cells")

featurs <- read.csv("data/figS3c.csv")
featurs$group <- paste(featurs$n_features, featurs$downsample_rate)
featurs$n_features <- as.factor(featurs$n_features)

p_s3c <- ggplot(featurs, aes(n_features, jaccard_index, fill = Method, group = group)) +
    geom_boxplot(size = 0.1, width = 0.5, position = position_dodge(width = 0.7), outlier.size = 0.2) +
    ylim(0,1) +
    scale_fill_manual(values = cols, guide = guide_legend()) +
    facet_grid(downsample_rate ~ .) +
    theme_classic() +
    theme(axis.line=element_blank(), 
          legend.position = "top",
          strip.background = element_rect(colour = "white"),
          axis.text.x=element_text(angle = -30, hjust = 0)) +
    annotate("segment", x=0, xend=Inf, y=0, yend=0, color = "black") +
    annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, color = "black") +
    labs(x = "Number of features", y = "Jaccard Index")




plot_grid(p_s3a, p_s3b, p_s3c, nrow = 1, labels = c("a", "b", "c"))
ggsave("figS3.png", w = 9, h = 6)

