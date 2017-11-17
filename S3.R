library(cowplot)

cols <- c("#d73027", "#984ea3")

downsampling <- read.csv("data/figS3ab.csv")
downsampling$n_features <- factor(downsampling$n_features, levels = c("0-100", "100-200", "200-500", "500-1000", "1000-2000", "2000-5000", "ALL"))
downsampling$Method <- factor(downsampling$Method, levels = c("scmap-cluster", "scmap-cell"))

p_s3a <- ggplot(downsampling, aes(n_features, kappa, fill = Method)) +
    geom_boxplot(size = 0.1, width = 0.5, position = position_dodge(width = 0.7), outlier.size = 0.2) +
    facet_grid(downsample_frac ~ .) +
    scale_fill_manual(values = cols, guide = guide_legend()) +
    theme_classic(base_size = 8) +
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
    theme_classic(base_size = 8) +
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
    geom_boxplot(size = 0.1, width = 0.5, position = position_dodge(width = 0.7), outlier.size = 0.2, fill = "white") +
    ylim(0,1) +
    # scale_fill_manual(values = cols, guide = guide_legend()) +
    facet_grid(downsample_rate ~ .) +
    theme_classic(base_size = 8) +
    theme(axis.line=element_blank(), 
          legend.position = "top",
          strip.background = element_rect(colour = "white")) +
    annotate("segment", x=0, xend=Inf, y=0, yend=0, color = "black") +
    annotate("segment", x=0, xend=0, y=-Inf, yend=Inf, color = "black") +
    labs(x = "Number of features", y = "Jaccard Index")

plot_grid(p_s3a, p_s3b, p_s3c, nrow = 1, labels = c("a", "b", "c"))
ggsave("pdf/S3.pdf", w = 9, h = 6)
ggsave("jpeg/S3.jpeg", w = 9, h = 6)

