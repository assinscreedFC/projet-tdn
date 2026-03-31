# =============================================================================
# 04_cah.R - Classification Ascendante Hierarchique (CAH)
# =============================================================================

library(factoextra)
library(cluster)

load(file.path("output", "cereales_clean.RData"))
load(file.path("output", "res_acp.RData"))
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

cat("====================================\n")
cat("CAH - Methode de Ward\n")
cat("====================================\n\n")

# --- Donnees centrees-reduites ---
data_scaled <- scale(cereales[, vars_actives])

# --- Distance euclidienne ---
dist_mat <- dist(data_scaled, method = "euclidean")

# --- CAH avec methode de Ward ---
hc <- hclust(dist_mat, method = "ward.D2")

# =============================================================================
# 1. Dendrogramme
# =============================================================================

png("output/figures/12_dendrogramme.png", width = 1200, height = 600)
fviz_dend(hc, k = 3, cex = 0.5,
          k_colors = c("steelblue", "coral", "forestgreen"),
          rect = TRUE, rect_fill = TRUE, rect_border = "gray",
          main = "Dendrogramme CAH (Ward, distance euclidienne)",
          xlab = "Cereales", ylab = "Hauteur")
dev.off()
cat("Figure sauvegardee : output/figures/12_dendrogramme.png\n")

# =============================================================================
# 2. Choix du nombre de clusters
# =============================================================================

# Methode du coude (inertie intra-classe)
png("output/figures/13_cah_elbow.png", width = 800, height = 500)
inertie <- rev(hc$height)
plot(1:10, inertie[1:10], type = "b", pch = 19, col = "steelblue",
     xlab = "Nombre de clusters", ylab = "Inertie intra-classe",
     main = "Methode du coude (CAH Ward)")
abline(v = 3, lty = 2, col = "red")
dev.off()
cat("Figure sauvegardee : output/figures/13_cah_elbow.png\n")

# Silhouette pour k = 2 a 6
cat("\n=== Silhouette moyenne par k ===\n")
for (k in 2:6) {
  grp <- cutree(hc, k = k)
  sil <- silhouette(grp, dist_mat)
  cat("k =", k, ": silhouette moyenne =", round(mean(sil[, 3]), 3), "\n")
}

# Choix optimal : on prend k=3 (a ajuster apres observation)
k_optimal <- 3
cat("\n=> k optimal retenu :", k_optimal, "\n")

# =============================================================================
# 3. Attribution des clusters
# =============================================================================

cereales$cluster_cah <- as.factor(cutree(hc, k = k_optimal))

cat("\n=== Effectifs par cluster (CAH) ===\n")
print(table(cereales$cluster_cah))

# =============================================================================
# 4. Caracterisation des clusters
# =============================================================================

cat("\n=== Moyennes par cluster (variables actives) ===\n")
moyennes_cah <- aggregate(cereales[, vars_actives],
                          by = list(Cluster = cereales$cluster_cah),
                          FUN = mean)
moyennes_cah[, -1] <- round(moyennes_cah[, -1], 1)
print(moyennes_cah)

# Barplot des profils
png("output/figures/14_profils_cah.png", width = 1000, height = 600)
moyennes_scaled <- aggregate(data_scaled,
                             by = list(Cluster = cereales$cluster_cah),
                             FUN = mean)
mat_profils <- as.matrix(moyennes_scaled[, -1])
rownames(mat_profils) <- paste("Cluster", moyennes_scaled$Cluster)
barplot(mat_profils, beside = TRUE, col = c("steelblue", "coral", "forestgreen"),
        las = 2, main = "Profils moyens des clusters (CAH, centrees-reduites)",
        ylab = "Valeur moyenne centree-reduite", legend.text = rownames(mat_profils),
        args.legend = list(x = "topright", cex = 0.8))
abline(h = 0, lty = 2)
dev.off()
cat("Figure sauvegardee : output/figures/14_profils_cah.png\n")

# --- Table croisee clusters x MANUF ---
cat("\n=== Clusters x Fabricant (MANUF) ===\n")
print(table(cereales$cluster_cah, cereales$MANUF))

# --- Table croisee clusters x TYPE ---
cat("\n=== Clusters x Type (C/H) ===\n")
print(table(cereales$cluster_cah, cereales$TYPE))

# --- Sauvegarde ---
save(hc, k_optimal, dist_mat, data_scaled,
     file = file.path("output", "res_cah.RData"))
save(cereales, vars_actives, vars_suppl_q, vars_suppl_cat,
     file = file.path("output", "cereales_clean.RData"))

cat("\n=== CAH terminee ===\n")
