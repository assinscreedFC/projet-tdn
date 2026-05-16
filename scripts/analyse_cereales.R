# Projet TND - Analyse des cereales (Binome 5)
# ACP normee + CAH + K-means sur les 77 cereales de 276-cereals.txt
library(FactoMineR); library(factoextra); library(corrplot)
library(cluster); library(moments)
set.seed(42)

# --- Pretraitement ---------------------------------------------------------
cereales <- read.table("Binome5/276-cereals.txt", header = TRUE, sep = "\t",
                        strip.white = TRUE, quote = "", comment.char = "")
quanti <- c("CALORIES","PROTEIN","FAT","SODIUM","FIBER","CARBO","SUGARS",
            "POTASS","VITAMINS","SHELF","WEIGHT","CUPS","RATING")
for (c in quanti) {                       # -1 = donnee manquante
  cereales[[c]][cereales[[c]] == -1] <- NA
  cereales[[c]][is.na(cereales[[c]])] <- median(cereales[[c]], na.rm = TRUE)
}
cereales$MANUF <- factor(cereales$MANUF); cereales$TYPE <- factor(cereales$TYPE)
rownames(cereales) <- make.unique(trimws(cereales$NAME))
actives <- c("CALORIES","PROTEIN","FAT","SODIUM","FIBER","CARBO",
             "SUGARS","POTASS","VITAMINS")

# --- Statistiques descriptives ---------------------------------------------
print(round(data.frame(
  Moy = sapply(cereales[actives], mean), Med = sapply(cereales[actives], median),
  Sd  = sapply(cereales[actives], sd),  Skew = sapply(cereales[actives], skewness)), 2))
X <- scale(cereales[actives])
print(round(cor(cereales[actives]), 2))
corrplot(cor(cereales[actives]), method = "color", type = "upper",
         order = "hclust", addCoef.col = "black", tl.col = "black")

# --- ACP normee (variables suppl. : conditionnement, RATING, MANUF, TYPE) ---
acp <- PCA(cereales[, c(actives,"SHELF","WEIGHT","CUPS","RATING","MANUF","TYPE")],
           quanti.sup = 10:13, quali.sup = 14:15,
           scale.unit = TRUE, graph = FALSE)
print(round(acp$eig, 2))
print(round(acp$quanti.sup$coord[, 1:2], 2))   # projection de RATING
fviz_pca_var(acp, col.var = "contrib", repel = TRUE)
fviz_pca_biplot(acp, repel = TRUE, labelsize = 2)

# --- CAH (Ward, distance euclidienne) --------------------------------------
hc <- hclust(dist(X), method = "ward.D2")
for (k in 2:6)
  cat("k =", k, ": silhouette =",
      round(mean(silhouette(cutree(hc, k), dist(X))[, 3]), 3), "\n")
cereales$cah <- factor(cutree(hc, 3))
fviz_dend(hc, k = 3, rect = TRUE)

# --- K-means (k = 3, identique a la CAH) ------------------------------------
km <- kmeans(X, centers = 3, nstart = 25)
cereales$km <- factor(km$cluster)
prof <- aggregate(X, list(cereales$km), mean); rownames(prof) <- prof[, 1]
barplot(as.matrix(prof[, -1]), beside = TRUE, las = 2,
        col = c("steelblue","coral","forestgreen"))

# --- Comparaison des deux partitions ---------------------------------------
print(table(CAH = cereales$cah, Kmeans = cereales$km))
rand <- function(a, b) {                  # indice de Rand
  n <- length(a); s <- 0
  for (i in 1:(n-1)) for (j in (i+1):n)
    s <- s + ((a[i]==a[j]) == (b[i]==b[j]))
  s / choose(n, 2)
}
cat("Rand :", round(rand(as.integer(cereales$cah), as.integer(cereales$km)), 3), "\n")

# --- Synthese : clusters dans le plan ACP et lien avec RATING --------------
fviz_pca_ind(acp, habillage = cereales$km, addEllipses = TRUE,
             repel = TRUE, labelsize = 2)
print(round(tapply(cereales$RATING, cereales$km, mean), 1))
boxplot(RATING ~ km, data = cereales, col = c("steelblue","coral","forestgreen"))
