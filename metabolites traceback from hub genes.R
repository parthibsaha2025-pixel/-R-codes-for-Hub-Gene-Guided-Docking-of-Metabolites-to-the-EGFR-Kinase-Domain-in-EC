library(openxlsx)
library(dplyr)

# ==========================
# INPUT / OUTPUT PATHS
# ==========================
input_folder <- "Path to File"

output_folder <- "Path to File"

if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

# ==========================
# TARGET GENES
# ==========================
target_genes <- c(
  "AKT1",
  "EGFR",
  "MAPK3",
  "MAPK1",
  "PIK3CA",
  "BCL2",
  "ERBB2"
)

# ==========================
# FIND ALL EXCEL FILES
# ==========================
all_files <- list.files(
  input_folder,
  pattern = "\\.xlsx$",
  recursive = TRUE,
  full.names = TRUE
)

cat("Files found:", length(all_files), "\n")

# ==========================
# HELPER FUNCTIONS
# ==========================
find_header_row <- function(f, max_scan_rows = 10) {
  
  preview <- tryCatch(
    read.xlsx(f, colNames = FALSE, rows = 1:max_scan_rows),
    error = function(e) NULL
  )
  
  if (is.null(preview)) return(NA)
  
  for (r in seq_len(nrow(preview))) {
    
    vals <- tolower(trimws(as.character(preview[r, ])))
    vals <- gsub("[^a-z]", "", vals)
    
    if (any(vals == "commonname", na.rm = TRUE)) {
      return(r)
    }
  }
  
  return(NA)
}

find_common_name_col <- function(cols) {
  
  cleaned <- gsub("[^a-z]", "", tolower(cols))
  
  idx <- which(cleaned == "commonname")
  
  if (length(idx) == 0) return(NA)
  
  cols[idx[1]]
}

# ==========================
# EXTRACT METABOLITE-GENE PAIRS
# ==========================
association_data <- bind_rows(
  
  lapply(all_files, function(f) {
    
    hdr_row <- find_header_row(f)
    
    if (is.na(hdr_row)) {
      return(NULL)
    }
    
    df <- tryCatch(
      read.xlsx(f, startRow = hdr_row),
      error = function(e) NULL
    )
    
    if (is.null(df) || nrow(df) == 0) {
      return(NULL)
    }
    
    cn_col <- find_common_name_col(names(df))
    
    if (is.na(cn_col)) {
      return(NULL)
    }
    
    metabolite_name <- sub("\\.xlsx$", "", basename(f))
    
    data.frame(
      metabolite = metabolite_name,
      common_name = trimws(as.character(df[[cn_col]])),
      stringsAsFactors = FALSE
    )
  })
  
)

# ==========================
# CLEAN DATA
# ==========================
association_data <- association_data %>%
  filter(
    !is.na(common_name),
    common_name != ""
  )

# ==========================
# FILTER TARGET GENES
# ==========================
gene_metabolites <- association_data %>%
  filter(common_name %in% target_genes) %>%
  distinct(common_name, metabolite) %>%
  arrange(common_name, metabolite)

# ==========================
# SAVE DETAILED TABLE
# ==========================
write.csv(
  gene_metabolites,
  file.path(output_folder,
            "Target_Gene_Metabolite_Associations.csv"),
  row.names = FALSE
)

# ==========================
# SAVE GENE-WISE SUMMARY
# ==========================
gene_summary <- gene_metabolites %>%
  group_by(common_name) %>%
  summarise(
    n_metabolites = n(),
    metabolites = paste(sort(metabolite),
                        collapse = "; "),
    .groups = "drop"
  )

write.csv(
  gene_summary,
  file.path(output_folder,
            "Target_Gene_Metabolite_Summary.csv"),
  row.names = FALSE
)

# ==========================
# DISPLAY RESULTS
# ==========================
print(gene_summary)

cat("\nSaved files:\n")
cat(file.path(output_folder,
              "Target_Gene_Metabolite_Associations.csv"), "\n")

cat(file.path(output_folder,
              "Target_Gene_Metabolite_Summary.csv"), "\n")

