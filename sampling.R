library(dplyr)

submissions <- submissions %>%
  mutate(id = as.character(id))

reviews <- reviews %>%
  mutate(replyto = as.character(replyto))

human_summary <- reviews %>%
  mutate(rating_int = as.numeric(rating_int)) %>%
  group_by(replyto) %>%
  summarise(
    avg_human_score = mean(rating_int, na.rm = TRUE),
    sd_human_score = sd(rating_int, na.rm = TRUE),
    number_of_reviews = n(),
    .groups = "drop"
  )

combined_table <- submissions %>%
  left_join(human_summary, by = c("id" = "replyto"))

print(nrow(combined_table))
print(summary(combined_table$avg_human_score))
print(sum(!is.na(combined_table$avg_human_score)))

sample_frame <- combined_table %>%
  filter(conf_year <= 2021) %>%
  mutate(
    score_group = case_when(
      avg_human_score <= 3 ~ "low",
      avg_human_score > 3 & avg_human_score <= 6 ~ "medium",
      avg_human_score > 6 ~ "high",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(avg_human_score), !is.na(score_group))

print(nrow(sample_frame))

print(
  sample_frame %>%
    count(conf_year, score_group)
)

set.seed(123)

sampled_100_papers <- sample_frame %>%
  group_by(conf_year, score_group) %>%
  sample_frac(size = 100 / nrow(sample_frame)) %>%
  ungroup()

sampled_100_papers <- sampled_100_papers %>%
  slice_sample(n = min(100, nrow(.)))

write.csv(
  sampled_100_papers,
  "sampled_100_papers_score_year_2021_before.csv",
  row.names = FALSE
)

print(nrow(sampled_100_papers))

print(
  sampled_100_papers %>%
    count(conf_year, score_group)
)