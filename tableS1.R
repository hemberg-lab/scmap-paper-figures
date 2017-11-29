library(pryr)

datasets <- c("yan", "goolam", "deng-rpkms", "pollen", "li", "usoskin", 
              "kolodziejczyk", "xin", "tasic-rpkms", "baron-mouse", "muraro",
              "segerstolpe", "klein", "zeisel", "baron-human", "shekhar", "macosko")

scmap_cluster_compression <- c()
scmap_cell_compression <- c()

for(dat in datasets) {
  d <- readRDS(paste0("~/data/", dat, ".rds"))
  d <- selectFeatures(d)
  d <- indexCluster(d)
  d <- indexCell(d)
  
  scmap_cluster_compression <- c(scmap_cluster_compression, 
    object_size(logcounts(d)) / object_size(metadata(d)$scmap_cluster_index))
  scmap_cell_compression <- c(scmap_cell_compression,
    object_size(logcounts(d)) / object_size(metadata(d)$scmap_cell_index))
}

res <- data.frame(dataset = datasets, scmap_cluster = scmap_cluster_compression,
                  scmap_cell = scmap_cell_compression)
write.csv(res, "data/tableS1.csv", quote = F, row.names = F)
