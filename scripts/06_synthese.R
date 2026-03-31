# =============================================================================
# 06_synthese.R - Synthese : projection des clusters sur le plan ACP
# =============================================================================

library(FactoMineR)
library(factoextra)
library(cluster)

load(file.path("output", "cereales_clean.RData"))
load(file.path("output", "res_acp.RData"))
load(file.path("output", "res_cah.RData"))
load(file.path("output", "res_kmeans.RData"))
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

cat("====================================\n")
cat("SYNTHESE - Projections et profils\n")
cat("====================================\n\n")

# =============================================================================
# 1. Projection des clusters CAH sur le plan ACP
# =============================================================================

png("output/figures/18_acp_clusters_cah.png", width = 900, height = 700)
print(fviz_pca_ind(res_acp, axes = c(1, 2),
               habillage = cereales$cluster_cah,
               addEllipses = TRUE, ellipse.type = "confidence",
               palette = c("steelblue", "coral", "forestgreen"),
               repel = TRUE, labelsize = 2,
               title = "Projection des clusters CAH sur le plan ACP (axes 1-2)"))
dev.off()
cat("Figure sauvegardee : output/figures/18_acp_clusters_cah.png\n")

# =============================================================================
# 2. Projection des clusters K-means sur le plan ACP
# =============================================================================

png("output/figures/19_acp_clusters_kmeans.png", width = 900, height = 700)
print(fviz_pca_ind(res_acp, axes = c(1, 2),
               habillage = cereales$cluster_km,
               addEllipses = TRUE, ellipse.type = "confidence",
               palette = c("steelblue", "coral", "forestgreen"),
               repel = TRUE, labelsize = 2,
               title = "Projection des clusters K-means sur le plan ACP (axes 1-2)"))
dev.off()
cat("Figure sauvegardee : output/figures/19_acp_clusters_kmeans.png\n")

# =============================================================================
# 3. Visualisation fviz_cluster (K-means)
# =============================================================================

png("output/figures/20_fviz_cluster_km.png", width = 900, height = 700)
print(fviz_cluster(km, data = data_scaled,
               palette = c("steelblue", "coral", "forestgreen"),
               ellipse.type = "euclid", star.plot = TRUE,
               repel = TRUE, labelsize = 8,
               ggtheme = theme_minimal(),
               main = "Clusters K-means (plan des 2 premieres composantes)"))
dev.off()
cat("Figure sauvegardee : output/figures/20_fviz_cluster_km.png\n")

# =============================================================================
# 4. Silhouette plot (K-means)
# =============================================================================

sil_km <- silhouette(km$cluster, dist_mat)

png("output/figures/21_silhouette_kmeans.png", width = 900, height = 600)
fviz_silhouette(sil_km, palette = c("steelblue", "coral", "forestgreen"),
                main = paste0("Silhouette K-means (k=", k_optimal,
                              ", moyenne=", round(mean(sil_km[, 3]), 3), ")"))
dev.off()
cat("Figure sauvegardee : output/figures/21_silhouette_kmeans.png\n")

cat("\nSilhouette moyenne K-means :", round(mean(sil_km[, 3]), 3), "\n")

# =============================================================================
# 5. Analyse croisee RATING x Clusters
# =============================================================================

cat("\n=== RATING moyen par cluster (CAH) ===\n")
rating_cah <- tapply(cereales$RATING, cereales$cluster_cah, mean)
print(round(rating_cah, 2))

cat("\n=== RATING moyen par cluster (K-means) ===\n")
rating_km <- tapply(cereales$RATING, cereales$cluster_km, mean)
print(round(rating_km, 2))

# Boxplot RATING par cluster
png("output/figures/22_rating_par_cluster.png", width = 900, height = 500)
par(mfrow = c(1, 2))
boxplot(RATING ~ cluster_cah, data = cereales,
        col = c("steelblue", "coral", "forestgreen"),
        main = "RATING par cluster (CAH)",
        xlab = "Cluster", ylab = "RATING")
boxplot(RATING ~ cluster_km, data = cereales,
        col = c("steelblue", "coral", "forestgreen"),
        main = "RATING par cluster (K-means)",
        xlab = "Cluster", ylab = "RATING")
dev.off()
cat("Figure sauvegardee : output/figures/22_rating_par_cluster.png\n")

# =============================================================================
# 6. Interpretation des clusters
# =============================================================================

cat("\n====================================\n")
cat("INTERPRETATION FINALE DES CLUSTERS\n")
cat("====================================\n\n")

cat("=== Moyennes des variables cles par cluster (K-means) ===\n")
vars_interpret <- c("CALORIES", "SUGARS", "FIBER", "PROTEIN", "POTASS",
                    "FAT", "SODIUM", "RATING")
moy_interp <- aggregate(cereales[, vars_interpret],
                        by = list(Cluster = cereales$cluster_km),
                        FUN = mean)
moy_interp[, -1] <- round(moy_interp[, -1], 1)
print(moy_interp)

cat("\n=== Cereales par cluster (K-means) ===\n")
for (cl in sort(unique(cereales$cluster_km))) {
  noms <- rownames(cereales[cereales$cluster_km == cl, ])
  cat("\nCluster", cl, "(", length(noms), "cereales) :\n")
  cat(paste(noms, collapse = ", "), "\n")
}

# =============================================================================
# 7. Resume global
# =============================================================================

cat("\n====================================\n")
cat("RESUME DU PROJET\n")
cat("====================================\n")
cat("\n1. Dataset : 77 cereales, 9 variables actives nutritionnelles")
cat("\n2. Pretraitement : valeurs -1 imputees par la mediane")
cat("\n3. ACP normee : identification des axes principaux de variabilite")
cat("\n4. CAH (Ward) : classification hierarchique en", k_optimal, "clusters")
cat("\n5. K-means : partition en", k_optimal, "clusters (nstart=25)")
cat("\n6. Indice de Rand (CAH vs K-means) : accord entre les deux methodes")
cat("\n7. Projection des clusters sur le plan ACP : visualisation finale")
cat("\n\n=== Projet TDN termine ===\n")
