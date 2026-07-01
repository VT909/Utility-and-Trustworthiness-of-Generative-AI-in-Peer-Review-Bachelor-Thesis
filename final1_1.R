library(httr2)
library(jsonlite)
library(tidyverse)
library(DBI)
library(RSQLite)
library(curl)

api_key <- Sys.getenv("OPENAI_API_KEY")
if (api_key == "") stop("OPENAI_API_KEY is not set.")

con <- dbConnect(SQLite(), "C:/Users/User/Downloads/cs_conf_release.db")

reviews <- dbReadTable(con, "reviews")
submissions <- dbReadTable(con, "submissions")

sampled_table <- read.csv(
  "C:/Users/User/Documents/sampled_100_papers_score_year_2021_before.csv"
)

download_folder <- "C:/Users/User/Downloads/1BTsampled_paper"

if (!dir.exists(download_folder)) {
  dir.create(download_folder, recursive = TRUE)
}

prefix <- "https://openreview.net"

sampled_table <- sampled_table %>%
  mutate(
    id = as.character(id),
    pdf_url = paste0(prefix, pdf),
    pdf_file_name = paste0(id, ".pdf"),
    local_pdf_path = file.path(download_folder, pdf_file_name),
    file_name = id
  )

for (i in 1:nrow(sampled_table)) {
  url <- sampled_table$pdf_url[i]
  destination <- sampled_table$local_pdf_path[i]
  
  if (!file.exists(destination)) {
    message("Downloading: ", url)
    
    tryCatch(
      download.file(
        url = url,
        destfile = destination,
        mode = "wb"
      ),
      error = function(e) {
        message("Failed to download: ", url)
      }
    )
  }
}

pdf_files <- sampled_table$local_pdf_path
pdf_files <- pdf_files[file.exists(pdf_files)]

upload_pdf <- function(path, key) {
  req <- request("https://api.openai.com/v1/files") |>
    req_headers(
      Authorization = paste("Bearer", trimws(key))
    ) |>
    req_body_multipart(
      purpose = "user_data",
      file = curl::form_file(path)
    )
  
  res <- req |> req_perform()
  body <- res |> resp_body_json()
  body$id
}

run_analysis <- function(file_id, prompt, key, model = "o4-mini") {
  request_body <- list(
    model = model,
    input = list(
      list(
        role = "user",
        content = list(
          list(type = "input_file", file_id = file_id),
          list(type = "input_text", text = prompt)
        )
      )
    )
  )
  
  req <- request("https://api.openai.com/v1/responses") |>
    req_headers(
      Authorization = paste("Bearer", trimws(key)),
      "Content-Type" = "application/json"
    ) |>
    req_body_json(request_body, auto_unbox = TRUE)
  
  res <- req |> req_perform()
  res |> resp_body_json()
}

extract_text_output <- function(api_response) {
  if (is.null(api_response$output)) return(NA_character_)
  
  collected_text <- c()
  
  for (item in api_response$output) {
    if (!is.null(item$content)) {
      for (part in item$content) {
        if (!is.null(part$type) && part$type == "output_text") {
          collected_text <- c(collected_text, part$text)
        }
      }
    }
  }
  
  paste(collected_text, collapse = "\n")
}

analysis_prompt <- paste(
  "You are acting as a careful academic peer reviewer.",
  "Read the attached scientific paper PDF and produce a structured review.",
  "Base your judgment only on the information available in the paper.",
  "If important information is missing or unclear, state that explicitly.",
  "",
  "Return ONLY valid JSON.",
  "",
  "Use exactly the following structure:",
  "{",
  '  "paper_title": "string",',
  '  "short_summary": "string",',
  '  "main_claims": ["string", "string"],',
  '  "strengths": ["string", "string"],',
  '  "weaknesses": ["string", "string"],',
  '  "methodology_summary": "string",',
  '  "suggested_review_score_1_to_100": "integer",',
  '  "confidence_in_score_1_to_5": "integer",',
  '  "score_justification": "string",',
  '  "r_code_for_followup_analysis": "string"',
  "}",
  "",
  "Guidelines:",
  "- strengths should capture the main positive contributions of the paper",
  "- weaknesses should capture problems in reasoning, evaluation, clarity, novelty, or methodology",
  "- confidence_in_score_1_to_5 means confidence in the assigned review score",
  "- suggested_review_score_1_to_100 should be an integer on a peer-review style scale",
  "- for suggested_review_score_1_to_100 take into account the entire range",
  "- for suggested_review_score_1_to_100 consider that one third of papers should have score bellow 35, other third above 63, and other third of papers should have score between 35 and 65",
  "- do not include markdown",
  "- do not include explanations outside the JSON",
  sep = "\n"
)

n_runs <- 3

all_results <- list()

summary_table <- data.frame(
  file_name = character(),
  run_id = integer(),
  paper_title = character(),
  ai_score = numeric(),
  confidence_score = numeric(),
  stringsAsFactors = FALSE
)

for (pdf_path in pdf_files) {
  
  if (!file.exists(pdf_path)) {
    message("Skipping missing file: ", pdf_path)
    next
  }
  
  file_name <- sampled_table$id[match(pdf_path, sampled_table$local_pdf_path)]
  
  message("Processing paper: ", file_name)
  
  file_id <- upload_pdf(pdf_path, api_key)
  
  for (run_id in 1:n_runs) {
    
    message("  Run ", run_id, " for ", file_name)
    
    api_result <- run_analysis(
      file_id = file_id,
      prompt = analysis_prompt,
      key = api_key,
      model = "o4-mini"
    )
    
    write_json(
      api_result,
      paste0(file_name, "_run", run_id, "_api_full_result.json"),
      pretty = TRUE,
      auto_unbox = TRUE
    )
    
    output_text <- extract_text_output(api_result)
    
    writeLines(
      output_text,
      paste0(file_name, "_run", run_id, "_extracted_output_raw.json")
    )
    
    parsed_output <- tryCatch(
      fromJSON(output_text),
      error = function(e) NULL
    )
    
    if (!is.null(parsed_output)) {
      
      write_json(
        parsed_output,
        paste0(file_name, "_run", run_id, "_extracted_output_parsed.json"),
        pretty = TRUE,
        auto_unbox = TRUE
      )
      
      all_results[[paste0(file_name, "_run", run_id)]] <- parsed_output
      
      summary_table <- rbind(
        summary_table,
        data.frame(
          file_name = file_name,
          run_id = run_id,
          paper_title = ifelse(is.null(parsed_output$paper_title), NA, parsed_output$paper_title),
          ai_score = ifelse(is.null(parsed_output$suggested_review_score_1_to_100), NA, parsed_output$suggested_review_score_1_to_100),
          confidence_score = ifelse(is.null(parsed_output$confidence_in_score_1_to_5), NA, parsed_output$confidence_in_score_1_to_5),
          stringsAsFactors = FALSE
        )
      )
      
    } else {
      message("  Could not parse JSON for ", file_name, " run ", run_id)
    }
  }
}

write.csv(summary_table, "summary_table_multiple_runs_100_sampled_papers.csv", row.names = FALSE)

ai_wide <- summary_table %>%
  mutate(
    ai_score = as.numeric(ai_score),
    confidence_score = as.numeric(confidence_score)
  ) %>%
  select(file_name, run_id, ai_score, confidence_score) %>%
  pivot_wider(
    names_from = run_id,
    values_from = c(ai_score, confidence_score),
    names_glue = "{.value}_run_{run_id}"
  ) %>%
  rowwise() %>%
  mutate(
    average_ai_score = mean(c_across(starts_with("ai_score_run_")), na.rm = TRUE),
    sd_ai_score = sd(c_across(starts_with("ai_score_run_")), na.rm = TRUE),
    average_ai_confidence = mean(c_across(starts_with("confidence_score_run_")), na.rm = TRUE),
    sd_ai_confidence = sd(c_across(starts_with("confidence_score_run_")), na.rm = TRUE)
  ) %>%
  ungroup()

human_reviews_table <- reviews %>%
  mutate(
    replyto = as.character(replyto),
    rating_int = as.numeric(sub(":.*", "", rating_int))
  ) %>%
  left_join(
    submissions %>%
      mutate(id = as.character(id)),
    by = c("replyto" = "id")
  ) %>%
  mutate(
    file_name = replyto,
    paper_title = if ("title" %in% names(.)) title else NA_character_
  ) %>%
  group_by(file_name, paper_title, abstract, pdf, conf_year) %>%
  summarise(
    average_human_score = mean(rating_int, na.rm = TRUE),
    sd_human_score = sd(rating_int, na.rm = TRUE),
    number_of_human_reviews = sum(!is.na(rating_int)),
    min_human_score = min(rating_int, na.rm = TRUE),
    max_human_score = max(rating_int, na.rm = TRUE),
    .groups = "drop"
  )

human_for_ai_papers <- human_reviews_table %>%
  filter(file_name %in% sampled_table$id)

final_human_ai_comparison <- human_for_ai_papers %>%
  left_join(ai_wide, by = "file_name") %>%
  left_join(
    sampled_table %>%
      select(id, score_group, local_pdf_path, pdf_url),
    by = c("file_name" = "id")
  ) %>%
  mutate(
    difference_ai_minus_human = (average_ai_score / 10) - average_human_score,
    absolute_difference = abs((average_ai_score / 10) - average_human_score)
  )

View(final_human_ai_comparison)

write.csv(
  final_human_ai_comparison,
  "final_human_ai_comparison_100_sampled_papers.csv",
  row.names = FALSE
)

print(final_human_ai_comparison)