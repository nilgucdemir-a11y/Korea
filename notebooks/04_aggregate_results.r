# Databricks notebook source
# MAGIC %md
# MAGIC # 04 — Aggregate IHACRES Results Across All Catchments
# MAGIC
# MAGIC Reads per-catchment JSON summaries produced by notebook 02, merges them
# MAGIC into a single master table, and writes a consolidated CSV + RDS.

# COMMAND ----------

dbutils.widgets.text("results_dir",
  "/Volumes/gc_prod_sandbox/mdt_sandbox/r_mdt/Projects/Other/122_2025_KR_IHACRES/R02_Output/ihacres_results/KOR/",
  "Results directory")

# COMMAND ----------

library(jsonlite)
library(dplyr)
library(readr)
library(tidyr)

results_dir <- dbutils.widgets.get("results_dir")

# COMMAND ----------

# MAGIC %md
# MAGIC ## Collect All Model Summaries

# COMMAND ----------

catch_dirs <- list.dirs(results_dir, recursive = FALSE, full.names = TRUE)
message(sprintf("Found %d catchment result directories.", length(catch_dirs)))

all_results <- list()

for (d in catch_dirs) {
  json_files <- list.files(d, pattern = "_results\\.json$", full.names = TRUE)
  for (jf in json_files) {
    tryCatch({
      res <- fromJSON(jf)
      row <- tibble(
        catchment_id = res$catchment_id,
        model        = res$model,
        NSE          = res$gof$NSE,
        KGE          = res$gof$KGE,
        RMSE         = res$gof$RMSE,
        PBIAS        = res$gof$PBIAS
      )
      coefs <- as.data.frame(t(unlist(res$coefficients)))
      row <- bind_cols(row, coefs)
      all_results[[length(all_results) + 1]] <- row
    }, error = function(e) {
      message(sprintf("Skipping %s: %s", jf, conditionMessage(e)))
    })
  }
}

master_df <- bind_rows(all_results)
message(sprintf("Aggregated %d model fits across %d catchments.",
                nrow(master_df), length(unique(master_df$catchment_id))))

# COMMAND ----------

# MAGIC %md
# MAGIC ## Display Summary Statistics

# COMMAND ----------

print(head(master_df, 20))

master_df %>%
  group_by(model) %>%
  summarise(
    n         = n(),
    mean_NSE  = round(mean(NSE, na.rm = TRUE), 3),
    mean_KGE  = round(mean(KGE, na.rm = TRUE), 3),
    mean_RMSE = round(mean(RMSE, na.rm = TRUE), 3),
    .groups   = "drop"
  ) %>%
  print()

# COMMAND ----------

# MAGIC %md
# MAGIC ## Save Master Results

# COMMAND ----------

master_csv <- file.path(results_dir, "all_catchments_master_results.csv")
master_rds <- file.path(results_dir, "all_catchments_master_results.rds")

write_csv(master_df, master_csv)
saveRDS(master_df, master_rds)

message("Master results saved to:")
message("  CSV: ", master_csv)
message("  RDS: ", master_rds)

# COMMAND ----------

# MAGIC %md
# MAGIC ## Performance Overview Plots

# COMMAND ----------

library(ggplot2)

for (mod in unique(master_df$model)) {
  sub <- master_df %>% filter(model == mod)

  p <- ggplot(sub, aes(x = reorder(catchment_id, -KGE), y = KGE)) +
    geom_bar(stat = "identity", fill = ifelse(sub$KGE >= 0.5, "steelblue", "coral")) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey40") +
    coord_flip() +
    labs(title = sprintf("KGE by Catchment — %s", mod),
         x = "Catchment", y = "KGE") +
    theme_minimal(base_size = 10) +
    theme(axis.text.y = element_text(size = 5))

  png_path <- file.path(results_dir, paste0("kge_barchart_", mod, ".png"),
                         width = 1000, height = max(400, nrow(sub) * 8))
  ggsave(png_path, p, width = 10, height = max(6, nrow(sub) * 0.12))
  message("Saved: ", png_path)
}

message("Aggregation complete.")
