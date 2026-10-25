# Load required packages
library(readxl)
library(writexl)
library(dplyr)
library(VennDiagram)
library(grid)

# ------------------------------------------------------------------
# 1. FILE PATHS
# ------------------------------------------------------------------
gene_file    <- "Path to File"
common_file  <- "Path to File"
output_folder <-"Path to File"

# ------------------------------------------------------------------
# 2. Read the GeneCards file and clean the Symbol column
# ------------------------------------------------------------------
gene_df <- read_excel(gene_file, sheet = 1)
names(gene_df) <- trimws(names(gene_df))

gene_df$Symbol <- trimws(as.character(gene_df$Symbol))
gene_symbols <- unique(toupper(gene_df$Symbol[!is.na(gene_df$Symbol) & gene_df$Symbol != ""]))

cat("Unique gene symbols:", length(gene_symbols), "\n")

# ------------------------------------------------------------------
# 3. Read the Common_Name file and clean the Common_Name column
# ------------------------------------------------------------------
common_df <- read_excel(common_file, sheet = 1)
names(common_df) <- trimws(names(common_df))

common_df$Common_Name <- trimws(as.character(common_df$Common_Name))
common_names <- unique(toupper(common_df$Common_Name[!is.na(common_df$Common_Name) & common_df$Common_Name != ""]))

cat("Unique common names:", length(common_names), "\n")

# ------------------------------------------------------------------
# 4. Find the intersection -> stored in a FRESH dataframe
# ------------------------------------------------------------------
intersection_values <- intersect(gene_symbols, common_names)

intersection_df <- data.frame(Intersected_Name = sort(intersection_values), stringsAsFactors = FALSE)

cat("\nNumber of intersected genes/names:", nrow(intersection_df), "\n")

# ------------------------------------------------------------------
# 5. Save the intersection dataframe
# ------------------------------------------------------------------
out_path <- file.path(output_folder, "Intersection_Symbol_CommonName.xlsx")
write_xlsx(intersection_df, path = out_path)
cat("Intersection saved to:", out_path, "\n")

# ------------------------------------------------------------------
# 6. VENN DIAGRAM
# ------------------------------------------------------------------
venn_path <- file.path(output_folder, "Venn_Symbol_vs_CommonName.png")

venn.plot <- venn.diagram(
  x = list(
    "GeneCards Symbol" = gene_symbols,
    "STP Common Name"  = common_names
  ),
  filename = NULL,
  fill = c("#3f8fd2", "#e8813a"),
  alpha = 0.6,
  cex = 1.5,
  cat.cex = 1.3,
  cat.pos = c(-20, 20),
  main = "Intersection: GeneCards Symbol vs STP Common Name",
  main.cex = 1.4
)

png(venn_path, width = 1600, height = 1600, res = 300)
grid.newpage()
grid.draw(venn.plot)
dev.off()

cat("Venn diagram saved to:", venn_path, "\n")

# ------------------------------------------------------------------
# 7. SUMMARY
# ------------------------------------------------------------------
cat("\n========== SUMMARY ==========\n")
cat("GeneCards unique symbols     :", length(gene_symbols), "\n")
cat("STP unique common names      :", length(common_names), "\n")
cat("Intersected names            :", nrow(intersection_df), "\n")
cat("Intersection file            :", out_path, "\n")
cat("Venn diagram                 :", venn_path, "\n")

# 'intersection_df' is now available in your R environment as the
# fresh dataframe holding the intersected values.