
library(tidyverse)
library(openxlsx)
library(factoextra)

df_rapportcijfers <- read.xlsx("04 output tabellen/tabel_rapportcijfers24_lng.xlsx") |>
  filter(
    winkelgebied_naam_kort != 'Amsterdam totaal',
    !is.na(winkelgebied_code),
    #aantal > 29,
    !grepl("Overig", winkelgebied_naam_kort),
    !grepl("overig", winkelgebied_naam_kort)) |>
  select(item, winkelgebied_naam_kort, gemiddelde)|>
  pivot_wider(names_from = item, values_from = gemiddelde) |>
  column_to_rownames(var="winkelgebied_naam_kort") |>
  scale()
  
  


# Correlation-based distance method
res.dist <-  df_rapportcijfers |>
  get_dist(method = "euclidean")

fviz_dist(res.dist, lab_size = 8)


# Compute dissimilarity matrix
res.dist <- dist(df_rapportcijfers, method = "euclidean")

# Compute hierarchical clustering
res.hc <- hclust(res.dist, method = "ward.D2")

# Visualize
plot(res.hc, cex = 0.5)

library("factoextra")

# Enhanced k-means clustering
res.km <- eclust(df_rapportcijfers, "kmeans", nstart = 25 , k = 4 )

fviz_dend(res.km, rect = TRUE) # dendrogam

# Gap statistic plot
fviz_gap_stat(res.km$gap_stat)

fviz_silhouette(res.km)

res.km$nbclust

res.km
