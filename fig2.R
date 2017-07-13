library(scmap)
library(irr)
library(cowplot)

cols <- c("#d73027", "#fdae61", "#4575b4")

d <- read.table("data/fig2a.txt", header = T)

d$Time <- d$Time/3600

d$Method <- factor(d$Method, levels = c("scmap", "SVM", "RF"))

p_d <- ggplot(d, aes(n_cells_ref, Time, colour = Method, group = Method)) +
    geom_smooth(method = "loess", span = 1, se = FALSE) +
    geom_point(size = 2) +
    scale_x_log10(breaks = c(100, 1000, 10000, 20000, 30000, 40000, 100000, 1000000),
                  labels = c("1e+02", "1e+03", "1e+04", "2e+04", "3e+04", "4e+04", "1e+05", "1e+06")) +
    scale_y_log10() +
    coord_cartesian(xlim=c(99, 1000000), ylim=c(0.001,1600)) +
    geom_hline(yintercept = 0.017, linetype = 2) +
    annotate("text", x = 1000, y = 0.04, label = "1 minute") +
    geom_hline(yintercept = 0.5, linetype = 2) +
    annotate("text", x = 1000, y = 1.1, label = "1/2 hour") +
    geom_hline(yintercept = 24, linetype = 2) +
    annotate("text", x = 1000, y = 65, label = "1 day") +
    geom_hline(yintercept = 720, linetype = 2) +
    annotate("text", x = 1000, y = 1600, label = "1 month") +
    geom_hline(yintercept = 8760, linetype = 2) +
    annotate("text", x = 1000, y = 25000, label = "1 year") +
    scale_colour_manual(values = cols) +
    theme_classic(base_size = 10) +
    theme(axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x=element_text(angle = -30, hjust = 0)) +
    labs(x = "# of cells", y = "Time")

first_row <- plot_grid(p_d, nrow = 1, labels = c("a"))
second_row <- plot_grid(NULL, NULL, NULL, nrow = 1, labels = c("b", "c", "d"))
plot_grid(first_row, second_row, ncol = 1)
ggsave("fig2.pdf", w = 9, h = 6)
ggsave("fig2.png", w = 9, h = 6)

similarities <- read.csv("data/fig2b-sims.csv")
labels <- read.csv("data/fig2b-labs.csv")
original_labs <- as.character(read.csv("data/fig2b-orig-labs.csv")[,1])
rownames(similarities) <- similarities[,1]
rownames(labels) <- labels[,1]

logicals <- lapply(as.list(as.data.frame(t(similarities[,-1:-2]))), function(x){
    !is.na(x) & x > 0.7
})

consensus_labs1 <- mapply(function(x, y) {
        tmp <- as.character(x)[y]
        names(tmp) <- names(x)[y]
        return(tmp)
    },
    as.list(as.data.frame(t(labels[,-1:-2]))),
    logicals
)

consensus_labs2 <- lapply(consensus_labs1, function(x) {
        if (length(unique(x)) == 1) {
            return(x[1])
        } else {
            return(NA)
        }
    }
)

plot(scmap::getSankey(original_labs, labels$xin, plot_width = 300, plot_height = 300))
# save to fig2a.pdf

tmp <- unlist(consensus_labs2)
kappa2(cbind(original_labs[which(!is.na(tmp))], tmp[which(!is.na(tmp))]))$value

tmp[is.na(tmp)] <- "unassigned"
plot(scmap::getSankey(original_labs, tmp, plot_width = 300, plot_height = 300))
# save to fig2b.pdf
