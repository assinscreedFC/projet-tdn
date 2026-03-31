# =============================================================================
# 00_main.R - Script principal - Execute toutes les phases du projet TDN
# =============================================================================

cat("====================================================\n")
cat("PROJET TDN - Analyse des cereales (Binome 5)\n")
cat("Universite Paris Cite - 2025-2026\n")
cat("====================================================\n\n")

# --- Configuration ---
setwd(dirname(sys.frame(1)$ofile))  # Se placer dans le dossier scripts/
dir.create("../output/figures", showWarnings = FALSE, recursive = TRUE)

# Pour que les chemins relatifs output/ fonctionnent
if (!dir.exists("output")) {
  # Creer un lien symbolique ou changer de repertoire
  setwd("..")  # Remonter au dossier projet-tdn/
}

# --- Verification des packages ---
packages <- c("FactoMineR", "factoextra", "corrplot", "cluster",
              "moments", "gridExtra")
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cran.r-project.org", quiet = TRUE)
    library(pkg, character.only = TRUE)
  }
}
cat("Tous les packages charges.\n\n")

# --- Execution sequentielle ---
cat("=== Phase 1 : Pretraitement ===\n")
source("scripts/01_pretraitement.R")

cat("\n\n=== Phase 2 : Statistiques descriptives ===\n")
source("scripts/02_stats_descriptives.R")

cat("\n\n=== Phase 3 : ACP normee ===\n")
source("scripts/03_acp.R")

cat("\n\n=== Phase 4 : CAH ===\n")
source("scripts/04_cah.R")

cat("\n\n=== Phase 5 : K-means ===\n")
source("scripts/05_kmeans.R")

cat("\n\n=== Phase 6 : Synthese ===\n")
source("scripts/06_synthese.R")

cat("\n\n====================================================\n")
cat("TOUTES LES PHASES TERMINEES\n")
cat("Figures dans : output/figures/\n")
cat("====================================================\n")
