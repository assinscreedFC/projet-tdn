# =============================================================================
# 01_pretraitement.R - Chargement et preparation des donnees
# Projet TDN : Analyse des cereales (Binome 5)
# =============================================================================

# --- Chargement des donnees ---
data_path <- file.path(dirname(getwd()), "Binome5", "276-cereals.txt")
if (!file.exists(data_path)) {
  data_path <- file.path("Binome5", "276-cereals.txt")
}
if (!file.exists(data_path)) {
  data_path <- file.path("..", "Binome5", "276-cereals.txt")
}

cereales <- read.table(data_path, header = TRUE, sep = "\t",
                       strip.white = TRUE, stringsAsFactors = FALSE,
                       quote = "", comment.char = "")

cat("Dimensions initiales :", nrow(cereales), "x", ncol(cereales), "\n")
cat("Variables :", names(cereales), "\n\n")

# --- Structure des donnees ---
str(cereales)
cat("\n")

# --- Identification des valeurs manquantes codees -1 ---
cat("=== Valeurs egales a -1 par variable ===\n")
nb_neg1 <- sapply(cereales, function(x) sum(x == -1, na.rm = TRUE))
print(nb_neg1[nb_neg1 > 0])

# Recoder -1 en NA (les -1 representent des valeurs manquantes)
cols_quanti <- c("CALORIES", "PROTEIN", "FAT", "SODIUM", "FIBER",
                 "CARBO", "SUGARS", "POTASS", "VITAMINS",
                 "SHELF", "WEIGHT", "CUPS", "RATING")

for (col in cols_quanti) {
  cereales[[col]][cereales[[col]] == -1] <- NA
}

cat("\n=== NA par variable apres recodage ===\n")
na_count <- colSums(is.na(cereales))
print(na_count[na_count > 0])

# --- Imputation par la mediane ---
# Choix justifie : la mediane est robuste aux valeurs extremes
for (col in cols_quanti) {
  if (any(is.na(cereales[[col]]))) {
    med_val <- median(cereales[[col]], na.rm = TRUE)
    cat("Imputation de", col, "par la mediane :", med_val, "\n")
    cereales[[col]][is.na(cereales[[col]])] <- med_val
  }
}

# --- Conversion des variables qualitatives en facteurs ---
cereales$MANUF <- as.factor(cereales$MANUF)
cereales$TYPE  <- as.factor(cereales$TYPE)

cat("\n=== Fabricants (MANUF) ===\n")
print(table(cereales$MANUF))

cat("\n=== Type (C=Froid, H=Chaud) ===\n")
print(table(cereales$TYPE))

# --- Noms des lignes = noms des cereales ---
rownames(cereales) <- make.unique(trimws(cereales$NAME))

# --- Definition des roles des variables ---
vars_actives   <- c("CALORIES", "PROTEIN", "FAT", "SODIUM", "FIBER",
                     "CARBO", "SUGARS", "POTASS", "VITAMINS")
vars_suppl_q   <- c("SHELF", "WEIGHT", "CUPS", "RATING")
vars_suppl_cat <- c("MANUF", "TYPE")

cat("\n=== Resume apres pretraitement ===\n")
cat("Variables actives (ACP)     :", vars_actives, "\n")
cat("Var. suppl. quantitatives   :", vars_suppl_q, "\n")
cat("Var. suppl. qualitatives    :", vars_suppl_cat, "\n")
cat("Nombre d'individus          :", nrow(cereales), "\n")
cat("Valeurs manquantes restantes:", sum(is.na(cereales)), "\n")

# --- Sauvegarde pour les scripts suivants ---
save(cereales, vars_actives, vars_suppl_q, vars_suppl_cat,
     file = file.path("output", "cereales_clean.RData"))
cat("\nDonnees sauvegardees dans output/cereales_clean.RData\n")
