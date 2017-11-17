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


similarities <- read.csv("data/fig2cd-sims.csv")
labels <- read.csv("data/fig2cd-labs.csv")
original_labs <- as.character(read.csv("data/fig2cd-orig-labs.csv")[,1])
rownames(similarities) <- similarities[,1]
rownames(labels) <- labels[,1]

logicals <- lapply(as.list(as.data.frame(t(similarities[,-1:-2]))), function(x){
    # !is.na(x) & x > 0.7
    tmp <- which.max(x)
    if (length(tmp) != 0) {
        if (x[tmp] >= 0.7) {
            return(tmp)
        } else {
            return(NA)
        }
    } else {
        return(NA)
    }
})

consensus_labs1 <- mapply(function(x, y) {
        if (!is.na(y)) {
            tmp <- as.character(x)[y]
            names(tmp) <- names(x)[y]
            return(tmp)
        } else {
            return(NA)
        }
    },
    as.list(as.data.frame(t(labels[,-1:-2]))),
    logicals
)

plot(scmap::getSankey(original_labs, labels$xin, plot_width = 300, plot_height = 300))
# save to 2b.pdf

tmp <- unlist(consensus_labs1)
kappa2(cbind(original_labs[which(!is.na(tmp))], tmp[which(!is.na(tmp))]))$value

tmp[is.na(tmp)] <- "unassigned"
plot(scmap::getSankey(original_labs, tmp, plot_width = 300, plot_height = 300))
# save to 2c.pdf
