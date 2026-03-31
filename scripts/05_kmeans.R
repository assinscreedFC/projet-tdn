# =============================================================================
# 05_kmeans.R - K-means et comparaison avec CAH
# =============================================================================

library(factoextra)
library(cluster)

load(file.path("output", "cereales_clean.RData"))
load(file.path("output", "res_cah.RData"))
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

cat("====================================\n")
cat("K-MEANS - k =", k_optimal, "(meme que CAH)\n")
cat("====================================\n\n")

# =============================================================================
# 1. Choix de k - Methode du coude
# =============================================================================

png("output/figures/15_kmeans_elbow.png", width = 800, height = 500)
print(fviz_nbclust(data_scaled, kmeans, method = "wss", k.max = 10, nstart = 25) +
  ggtitle("Methode du coude (K-means)") +
  geom_vline(xintercept = k_optimal, linetype = 2, color = "red"))
dev.off()
cat("Figure sauvegardee : output/figures/15_kmeans_elbow.png\n")

# Silhouette
png("output/figures/16_kmeans_silhouette.png", width = 800, height = 500)
print(fviz_nbclust(data_scaled, kmeans, method = "silhouette", k.max = 10, nstart = 25) +
  ggtitle("Silhouette moyenne (K-means)"))
dev.off()
cat("Figure sauvegardee : output/figures/16_kmeans_silhouette.png\n")

# =============================================================================
# 2. K-means avec k optimal
# =============================================================================

set.seed(42)  # Reproductibilite
km <- kmeans(data_scaled, centers = k_optimal, nstart = 25)

cereales$cluster_km <- as.factor(km$cluster)

cat("=== Effectifs par cluster (K-means) ===\n")
print(table(cereales$cluster_km))

cat("\n=== Centres des clusters (K-means, centrees-reduites) ===\n")
print(round(km$centers, 2))

# Inertie
cat("\nInertie intra-classe totale :", round(km$tot.withinss, 1), "\n")
cat("Inertie inter-classe       :", round(km$betweenss, 1), "\n")
cat("Ratio inter/total          :", round(km$betweenss / km$totss * 100, 1), "%\n")

# =============================================================================
# 3. Moyennes par cluster (echelle originale)
# =============================================================================

cat("\n=== Moyennes par cluster (K-means, echelle originale) ===\n")
moyennes_km <- aggregate(cereales[, vars_actives],
                         by = list(Cluster = cereales$cluster_km),
                         FUN = mean)
moyennes_km[, -1] <- round(moyennes_km[, -1], 1)
print(moyennes_km)

# Profils
png("output/figures/17_profils_kmeans.png", width = 1000, height = 600)
mat_km <- as.matrix(aggregate(data_scaled,
                              by = list(Cluster = cereales$cluster_km),
                              FUN = mean)[, -1])
rownames(mat_km) <- paste("Cluster", 1:k_optimal)
barplot(mat_km, beside = TRUE, col = c("steelblue", "coral", "forestgreen"),
        las = 2, main = "Profils moyens des clusters (K-means, centrees-reduites)",
        ylab = "Valeur moyenne centree-reduite", legend.text = rownames(mat_km),
        args.legend = list(x = "topright", cex = 0.8))
abline(h = 0, lty = 2)
dev.off()
cat("Figure sauvegardee : output/figures/17_profils_kmeans.png\n")

# =============================================================================
# 4. Comparaison CAH vs K-means
# =============================================================================

cat("\n====================================\n")
cat("COMPARAISON CAH vs K-MEANS\n")
cat("====================================\n\n")

# Table de contingence
cat("=== Table de contingence des deux partitions ===\n")
tab_comp <- table(CAH = cereales$cluster_cah, Kmeans = cereales$cluster_km)
print(tab_comp)

# Indice de Rand (mesure d'accord entre partitions)
# Calcul manuel de l'indice de Rand
rand_index <- function(c1, c2) {
  n <- length(c1)
  a <- 0  # paires dans le meme cluster dans les deux partitions
  b <- 0  # paires dans des clusters differents dans les deux partitions
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      same_c1 <- (c1[i] == c1[j])
      same_c2 <- (c2[i] == c2[j])
      if (same_c1 && same_c2) a <- a + 1
      if (!same_c1 && !same_c2) b <- b + 1
    }
  }
  return((a + b) / choose(n, 2))
}

ri <- rand_index(as.integer(cereales$cluster_cah),
                 as.integer(cereales$cluster_km))
cat("\nIndice de Rand :", round(ri, 3), "\n")
cat("(1 = accord parfait, 0 = accord aleatoire)\n")

# Purete
purete <- function(clusters, classes) {
  n <- length(clusters)
  total <- 0
  for (cl in unique(clusters)) {
    idx <- which(clusters == cl)
    tab <- table(classes[idx])
    total <- total + max(tab)
  }
  return(total / n)
}

pur_cah_vs_km <- purete(as.integer(cereales$cluster_cah),
                        as.integer(cereales$cluster_km))
cat("Purete CAH vs K-means :", round(pur_cah_vs_km, 3), "\n")

# --- Sauvegarde ---
save(km, cereales, vars_actives, vars_suppl_q, vars_suppl_cat,
     file = file.path("output", "cereales_clean.RData"))
save(km, file = file.path("output", "res_kmeans.RData"))

cat("\n=== K-means et comparaison termines ===\n")
