# =============================================================================
# 03_acp.R - Analyse en Composantes Principales (ACP normee)
# =============================================================================

library(FactoMineR)
library(factoextra)

load(file.path("output", "cereales_clean.RData"))
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

cat("====================================\n")
cat("ACP NORMEE - 9 variables actives\n")
cat("====================================\n\n")

# --- Preparer le dataframe pour FactoMineR (exclure NAME) ---
cereales_acp <- cereales[, c(vars_actives, vars_suppl_q, vars_suppl_cat)]

idx_suppl_quanti <- which(names(cereales_acp) %in% vars_suppl_q)
idx_suppl_quali  <- which(names(cereales_acp) %in% vars_suppl_cat)

# --- ACP normee (scale.unit = TRUE) ---
res_acp <- PCA(cereales_acp,
               quanti.sup = idx_suppl_quanti,
               quali.sup  = idx_suppl_quali,
               scale.unit = TRUE,
               graph = FALSE)

# =============================================================================
# 1. Valeurs propres et choix du nombre d'axes
# =============================================================================

cat("=== Valeurs propres ===\n")
eig <- res_acp$eig
print(round(eig, 2))

# Critere de Kaiser : valeurs propres > 1
n_kaiser <- sum(eig[, 1] > 1)
cat("\nCritere de Kaiser : retenir", n_kaiser, "axes (valeurs propres > 1)\n")

# Inertie cumulee >= 80%
n_80 <- which(eig[, 3] >= 80)[1]
cat("Seuil 80% d'inertie :", n_80, "axes (", round(eig[n_80, 3], 1), "%)\n")

# Scree plot (eboulis des valeurs propres)
png("output/figures/06_scree_plot.png", width = 800, height = 500)
fviz_eig(res_acp, addlabels = TRUE, ylim = c(0, 40),
         main = "Eboulis des valeurs propres (ACP normee)",
         barfill = "steelblue", barcolor = "steelblue")
dev.off()
cat("\nFigure sauvegardee : output/figures/06_scree_plot.png\n")

# =============================================================================
# 2. Cercle des correlations (axes 1-2)
# =============================================================================

# Variables actives
png("output/figures/07_cercle_correlations_12.png", width = 800, height = 700)
print(fviz_pca_var(res_acp, axes = c(1, 2),
               col.var = "contrib", gradient.cols = c("blue", "orange", "red"),
               repel = TRUE,
               title = "Cercle des correlations (Axes 1-2)"))
dev.off()
cat("Figure sauvegardee : output/figures/07_cercle_correlations_12.png\n")

# Si 3 axes retenus : cercle axes 1-3
if (n_kaiser >= 3) {
  png("output/figures/07b_cercle_correlations_13.png", width = 800, height = 700)
  print(fviz_pca_var(res_acp, axes = c(1, 3),
                 col.var = "contrib", gradient.cols = c("blue", "orange", "red"),
                 repel = TRUE,
                 title = "Cercle des correlations (Axes 1-3)"))
  dev.off()
  cat("Figure sauvegardee : output/figures/07b_cercle_correlations_13.png\n")
}

# =============================================================================
# 3. Carte des individus
# =============================================================================

# Coloration par TYPE (Chaud/Froid)
png("output/figures/08_individus_type.png", width = 900, height = 700)
print(fviz_pca_ind(res_acp, axes = c(1, 2),
               habillage = which(names(cereales_acp) == "TYPE"),
               addEllipses = TRUE, ellipse.type = "confidence",
               repel = TRUE, labelsize = 2,
               palette = c("steelblue", "coral"),
               title = "Carte des individus - coloration par TYPE"))
dev.off()
cat("Figure sauvegardee : output/figures/08_individus_type.png\n")

# Coloration par MANUF
png("output/figures/09_individus_manuf.png", width = 1000, height = 700)
print(fviz_pca_ind(res_acp, axes = c(1, 2),
               habillage = which(names(cereales_acp) == "MANUF"),
               addEllipses = TRUE, ellipse.type = "confidence",
               repel = TRUE, labelsize = 2,
               title = "Carte des individus - coloration par MANUF"))
dev.off()
cat("Figure sauvegardee : output/figures/09_individus_manuf.png\n")

# =============================================================================
# 4. Biplot (individus + variables)
# =============================================================================

png("output/figures/10_biplot.png", width = 1000, height = 800)
print(fviz_pca_biplot(res_acp, axes = c(1, 2),
                  repel = TRUE, labelsize = 2,
                  col.var = "red", col.ind = "steelblue",
                  title = "Biplot ACP - Individus et Variables"))
dev.off()
cat("Figure sauvegardee : output/figures/10_biplot.png\n")

# =============================================================================
# 5. Qualite de representation et contributions
# =============================================================================

cat("\n=== Contributions des variables aux axes ===\n")
cat("-- Axe 1 --\n")
contrib1 <- sort(res_acp$var$contrib[, 1], decreasing = TRUE)
print(round(contrib1, 2))

cat("\n-- Axe 2 --\n")
contrib2 <- sort(res_acp$var$contrib[, 2], decreasing = TRUE)
print(round(contrib2, 2))

cat("\n=== Cos2 des variables (qualite de representation) ===\n")
print(round(res_acp$var$cos2[, 1:min(3, ncol(res_acp$var$cos2))], 3))

# Graphique des contributions
png("output/figures/11_contributions.png", width = 1000, height = 500)
par(mfrow = c(1, 2))
p1 <- fviz_contrib(res_acp, choice = "var", axes = 1,
                    title = "Contributions a l'axe 1")
p2 <- fviz_contrib(res_acp, choice = "var", axes = 2,
                    title = "Contributions a l'axe 2")
print(gridExtra::grid.arrange(p1, p2, ncol = 2))
dev.off()
cat("Figure sauvegardee : output/figures/11_contributions.png\n")

# =============================================================================
# 6. Variables supplementaires
# =============================================================================

cat("\n=== Correlation des variables supplementaires quantitatives ===\n")
print(round(res_acp$quanti.sup$coord[, 1:min(3, ncol(res_acp$quanti.sup$coord))], 3))

cat("\n=== Coordonnees des modalites supplementaires qualitatives ===\n")
print(round(res_acp$quali.sup$coord[, 1:min(3, ncol(res_acp$quali.sup$coord))], 3))

# --- Individus les plus extremes ---
cat("\n=== Individus les plus extremes sur l'axe 1 ===\n")
coord1 <- res_acp$ind$coord[, 1]
cat("Extreme positif :", names(sort(coord1, decreasing = TRUE))[1:3], "\n")
cat("Extreme negatif :", names(sort(coord1))[1:3], "\n")

# --- Sauvegarde de l'objet ACP ---
save(res_acp, file = file.path("output", "res_acp.RData"))
cat("\nObjet ACP sauvegarde dans output/res_acp.RData\n")
cat("\n=== ACP terminee ===\n")
