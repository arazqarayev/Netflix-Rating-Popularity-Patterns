# Netflix-Rating-Popularity-Patterns
how IMDb scores, runtimes, and genres are related across Netflix content. Specifically:  Which genres have the highest IMDb ratings?  Do longer movies get higher scores?  How have average IMDb ratings changed over the years?
______________________________________________________________
# ======================================================
# 🎬 Netflix Rating & Popularity Patterns (Basic R)
# Description: Explore IMDb vs runtime, top-rated genres, yearly trends.
# ======================================================
<img width="858" height="587" alt="image" src="https://github.com/user-attachments/assets/c72f22cf-04df-4522-9711-ca9b00581cad" />

suppressPackageStartupMessages({
  library(tidyverse)   # dplyr, ggplot2, forcats, readr, etc.
  library(lubridate)
})


if (!exists("netflix")) {
  if (file.exists("netflix.csv")) {
    netflix <- read.csv("netflix.csv", stringsAsFactors = FALSE)
  } else {
    stop("Could not find netflix.csv and object `netflix` does not exist.")
  }
}


na_per_col <- colSums(is.na(netflix))
total_na_pct <- mean(is.na(netflix)) * 100

cat("NA per column:\n"); print(na_per_col)
cat(sprintf("\nTotal NA percentage: %.2f%%\n\n", total_na_pct))


a <- netflix %>%
  filter(!is.na(imdb_score), !is.na(runtime))

-----------------
p_scatter <- ggplot(a, aes(x = runtime, y = imdb_score)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "IMDb Score vs Runtime",
    x = "Runtime (minutes)",
    y = "IMDb Score"
  ) +
  theme_minimal()
print(p_scatter)

summary_genre <- a %>%
  filter(!is.na(genre)) %>%
  group_by(genre) %>%
  summarise(
    avg_imdb = mean(imdb_score, na.rm = TRUE),
    count    = n(),
    .groups  = "drop"
  ) %>%
  arrange(desc(avg_imdb))

cat("\nTop rows of genre summary:\n"); print(head(summary_genre))

p_genre <- ggplot(summary_genre, aes(x = avg_imdb, y = fct_reorder(genre, avg_imdb))) +
  geom_col() +
  labs(
    title = "Average IMDb by Genre",
    x = "Average IMDb Score",
    y = "Genre"
  ) +
  theme_minimal()
print(p_genre)

if ("year" %in% names(a)) {
  yearly <- a %>%
    filter(!is.na(year)) %>%
    group_by(year) %>%
    summarise(avg_imdb = mean(imdb_score, na.rm = TRUE), .groups = "drop") %>%
    arrange(year)

  cat("\nAverage IMDb by year (first rows):\n"); print(head(yearly))

  p_year <- ggplot(yearly, aes(x = year, y = avg_imdb)) +
    geom_line() +
    geom_point() +
    labs(
      title = "Average IMDb Score by Year",
      x = "Year",
      y = "Average IMDb"
    ) +
    theme_minimal()
  print(p_year)
} else {
  message("Column `year` not found; skipping yearly trend.")
}

corr_val <- suppressWarnings(cor(a$runtime, a$imdb_score, use = "complete.obs"))
cat(sprintf("\nCorrelation (runtime vs IMDb): %.3f\n", corr_val))
# Rule of thumb: > 0.3 = moderate positive; < 0 = negative relationship.

# ggsave("fig_scatter_runtime_imdb.png", p_scatter, width = 8, height = 4.5, dpi = 120)
# ggsave("fig_genre_avg_imdb.png", p_genre, width = 8, height = 6, dpi = 120)
# if (exists("p_year")) ggsave("fig_year_avg_imdb.png", p_year, width = 8, height = 4.5, dpi = 120)
# write.csv(summary_genre, "summary_genre.csv", row.names = FALSE)



