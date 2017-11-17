library(cowplot)

cols1 <- c("#d73027", "#4575b4")

f_select <- read.csv("data/figS1a.csv")
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

cols <- c("#d73027", "#984ea3", "#fdae61", "#4575b4")

self_proj <- read.csv("data/figS1b.csv")
self_proj$n_features <- factor(self_proj$n_features, levels = c("0-100", "100-200", "200-500", "500-1000", "1000-2000", "2000-5000", "ALL"))
self_proj$Features <- factor(self_proj$Features, levels = c("dropout", "HVG", "random"))
self_proj$Method <- factor(self_proj$Method, levels = c("scmap-cluster", "scmap-cell", "SVM", "RF"))

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

ggsave("pdf/S1.pdf", w = 9, h = 6)
ggsave("jpeg/S1.jpeg", w = 9, h = 6)
