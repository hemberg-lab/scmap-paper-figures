library(cowplot)

cols <- c("#d73027", "#984ea3")

downsampling <- read.csv("data/figS4.csv")
downsampling$n_features <- factor(downsampling$n_features, levels = c("0-100", "100-200", "200-500", "500-1000", "1000-2000", "2000-5000", "ALL"))
downsampling$Method <- factor(downsampling$Method, levels = c("scmap-cluster", "scmap-cell"))

p_s4a <- ggplot(downsampling, aes(n_features, kappa, fill = Method)) +
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

p_s4b <- ggplot(downsampling, aes(n_features, uns_rate, fill = Method)) +
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

dropout_rates <- as.data.frame(
    rbind(
        c("segerstolpe", 82),
        c("muraro", 73),
        c("xin", 86),
        c("baron-human", 91),
        c("shekhar", 93),
        c("macosko", 97)
    ),
    stringsAsFactors = F
)
colnames(dropout_rates) <- c("reference", "pct_dropout")
dropout_rates$pct_dropout <- as.numeric(dropout_rates$pct_dropout)

d <- read.csv("data/fig1bc.csv")
d <- d[d$Method == "scmap-cluster" & d$threshold == 0.7 | d$Method == "scmap-cell" & d$threshold == 0.5, ]
d <- d[d$n_features == "200-500", ]

d <- merge(d, dropout_rates)

d$Method <- factor(d$Method, levels = c("scmap-cluster", "scmap-cell"))

p_s4c <- ggplot(d, aes(pct_dropout, kappa, color = Method)) +
    geom_point() +
    geom_smooth(method='lm', se=FALSE) +
    scale_color_manual(values = cols, guide = guide_legend()) +
    labs(x = "% of dropouts", y = "Cohen's Kappa") +
    theme_classic(base_size = 8) +
    theme(legend.position = "top")

cat(cor(d[d$Method == "scmap-cluster", ]$pct_dropout, d[d$Method == "scmap-cluster", ]$kappa))
cat(cor(d[d$Method == "scmap-cell", ]$pct_dropout, d[d$Method == "scmap-cell", ]$kappa))

plot_grid(p_s4a, p_s4b, p_s4c, nrow = 1, labels = c("a", "b", "c"))
ggsave("pdf/S4.pdf", w = 9, h = 6)
ggsave("jpeg/S4.jpeg", w = 9, h = 6)
