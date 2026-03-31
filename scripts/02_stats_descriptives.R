# =============================================================================
# 02_stats_descriptives.R - Statistiques descriptives univariees et bivariees
# =============================================================================

library(corrplot)
library(moments)

load(file.path("output", "cereales_clean.RData"))

# Dossier pour les graphiques
dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# PARTIE 1 : Statistiques univariees
# =============================================================================

cat("====================================\n")
cat("STATISTIQUES UNIVARIEES\n")
cat("====================================\n\n")

# --- Resume statistique ---
cat("=== Resume des variables actives ===\n")
print(summary(cereales[, vars_actives]))

# --- Mesures de forme ---
cat("\n=== Asymetrie (Skewness) et Aplatissement (Kurtosis) ===\n")
forme <- data.frame(
  Variable  = vars_actives,
  Moyenne   = sapply(cereales[, vars_actives], mean),
  Mediane   = sapply(cereales[, vars_actives], median),
  Ecart_type = sapply(cereales[, vars_actives], sd),
  Skewness  = sapply(cereales[, vars_actives], skewness),
  Kurtosis  = sapply(cereales[, vars_actives], kurtosis)
)
print(forme, row.names = FALSE)

# --- Boxplots des variables actives (normalisees pour comparaison) ---
png("output/figures/01_boxplots_actives.png", width = 1000, height = 600)
data_scaled <- scale(cereales[, vars_actives])
boxplot(data_scaled, las = 2, col = "steelblue",
        main = "Boxplots des variables actives (centrees-reduites)",
        ylab = "Valeur centree-reduite")
abline(h = 0, lty = 2, col = "red")
dev.off()
cat("\nFigure sauvegardee : output/figures/01_boxplots_actives.png\n")

# --- Histogrammes des variables cles ---
png("output/figures/02_histogrammes.png", width = 1000, height = 800)
par(mfrow = c(3, 3))
for (v in vars_actives) {
  hist(cereales[[v]], main = v, xlab = v, col = "steelblue",
       border = "white", breaks = 15)
}
dev.off()
cat("Figure sauvegardee : output/figures/02_histogrammes.png\n")

# --- Detection des outliers (methode IQR) ---
cat("\n=== Detection des outliers (IQR x 1.5) ===\n")
for (v in vars_actives) {
  Q1 <- quantile(cereales[[v]], 0.25)
  Q3 <- quantile(cereales[[v]], 0.75)
  IQR_val <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  outliers <- cereales[cereales[[v]] < lower | cereales[[v]] > upper, ]
  if (nrow(outliers) > 0) {
    cat(v, ":", nrow(outliers), "outlier(s) -",
        rownames(outliers), "\n")
  }
}

# =============================================================================
# PARTIE 2 : Statistiques bivariees
# =============================================================================

cat("\n====================================\n")
cat("STATISTIQUES BIVARIEES\n")
cat("====================================\n\n")

# --- Matrice de correlation ---
mat_cor <- cor(cereales[, vars_actives])
cat("=== Matrice de correlation ===\n")
print(round(mat_cor, 2))

# --- Heatmap de correlation ---
png("output/figures/03_heatmap_correlation.png", width = 800, height = 700)
corrplot(mat_cor, method = "color", type = "upper", order = "hclust",
         addCoef.col = "black", number.cex = 0.7,
         tl.col = "black", tl.srt = 45,
         title = "Matrice de correlation des variables actives",
         mar = c(0, 0, 2, 0))
dev.off()
cat("\nFigure sauvegardee : output/figures/03_heatmap_correlation.png\n")

# --- Correlations fortes (|r| > 0.5) ---
cat("\n=== Correlations fortes (|r| > 0.5) ===\n")
for (i in 1:(length(vars_actives) - 1)) {
  for (j in (i + 1):length(vars_actives)) {
    r <- mat_cor[i, j]
    if (abs(r) > 0.5) {
      cat(vars_actives[i], "-", vars_actives[j], ": r =",
          round(r, 3), "\n")
    }
  }
}

# --- Scatter plots des paires les plus correlees ---
png("output/figures/04_scatter_paires.png", width = 1000, height = 500)
par(mfrow = c(1, 3))

# Paire 1 : FIBER - POTASS (attendu positif)
plot(cereales$FIBER, cereales$POTASS, pch = 19, col = "steelblue",
     xlab = "FIBER (g)", ylab = "POTASS (mg)",
     main = paste0("FIBER vs POTASS\nr = ",
                   round(cor(cereales$FIBER, cereales$POTASS), 3)))
abline(lm(POTASS ~ FIBER, data = cereales), col = "red", lwd = 2)

# Paire 2 : CALORIES - FAT (attendu positif)
plot(cereales$CALORIES, cereales$FAT, pch = 19, col = "steelblue",
     xlab = "CALORIES (Kcal)", ylab = "FAT (g)",
     main = paste0("CALORIES vs FAT\nr = ",
                   round(cor(cereales$CALORIES, cereales$FAT), 3)))
abline(lm(FAT ~ CALORIES, data = cereales), col = "red", lwd = 2)

# Paire 3 : SUGARS - lien avec une autre variable
plot(cereales$SUGARS, cereales$VITAMINS, pch = 19, col = "steelblue",
     xlab = "SUGARS (g)", ylab = "VITAMINS",
     main = paste0("SUGARS vs VITAMINS\nr = ",
                   round(cor(cereales$SUGARS, cereales$VITAMINS), 3)))
abline(lm(VITAMINS ~ SUGARS, data = cereales), col = "red", lwd = 2)
dev.off()
cat("Figure sauvegardee : output/figures/04_scatter_paires.png\n")

# --- Repartition par variables qualitatives ---
png("output/figures/05_barplots_quali.png", width = 1000, height = 500)
par(mfrow = c(1, 2))
barplot(table(cereales$MANUF), col = "steelblue",
        main = "Repartition par fabricant (MANUF)",
        las = 2, ylab = "Nombre de cereales")
barplot(table(cereales$TYPE), col = c("steelblue", "coral"),
        main = "Repartition par type (C=Froid, H=Chaud)",
        ylab = "Nombre de cereales")
dev.off()
cat("Figure sauvegardee : output/figures/05_barplots_quali.png\n")

cat("\n=== Statistiques descriptives terminees ===\n")
