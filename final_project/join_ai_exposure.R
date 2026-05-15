library(tidyverse)

# ─────────────────────────────────────────────────────────────────────────────
# STEP 1: Load the crosswalk
# ─────────────────────────────────────────────────────────────────────────────

crosswalk <- read_csv("job_soc_crosswalk.csv")

# ─────────────────────────────────────────────────────────────────────────────
# STEP 2: Load AI exposure data from Anthropic's HuggingFace repo
# (Labor market impacts release — occupation-level observed exposure scores)
# ─────────────────────────────────────────────────────────────────────────────

# Option A: Anthropic labor market impacts data (most relevant — actual observed exposure)
# Replace with the correct filename once you download from:
# https://huggingface.co/datasets/Anthropic/EconomicIndex
# → "Labor market impacts: Job exposure and task penetration data" folder

anthropic_exposure <- read_csv("occupation_exposure.csv")  # rename to match actual file

# Option B: Eloundou et al. (GPTs are GPTs) task-level scores, also in HuggingFace repo
# release_2025_02_10/ folder has onet task-level beta scores
eloundou_tasks <- read_csv("task_exposure_eloundou.csv")   # rename to match actual file

# ─────────────────────────────────────────────────────────────────────────────
# STEP 3: Your job postings data — assumes a column called `job_title`
# ─────────────────────────────────────────────────────────────────────────────

your_jobs <- read_csv("your_job_postings.csv")  # replace with your file

# ─────────────────────────────────────────────────────────────────────────────
# STEP 4: Assign SOC codes to your job titles using keyword matching
# Uses the crosswalk to find the BEST (highest-confidence) SOC match per title
# ─────────────────────────────────────────────────────────────────────────────

# Build a named vector of pattern → soc_code (use only High confidence matches
# as primary, then fall back to Medium)
crosswalk_primary <- crosswalk %>%
  filter(match_confidence %in% c("High", "Medium")) %>%
  arrange(soc_code, match_confidence) %>%
  # Keep one SOC per keyword_pattern (prefer High over Medium)
  group_by(keyword_pattern) %>%
  slice(1) %>%
  ungroup()

assign_soc <- function(title) {
  title_lower <- str_to_lower(title)
  
  # Try specific job title matches first (longer/more specific patterns win)
  specific_patterns <- crosswalk_primary %>%
    filter(str_detect(keyword_pattern, "\\s"))  # multi-word = specific
  
  for (i in seq_len(nrow(specific_patterns))) {
    if (str_detect(title_lower, specific_patterns$regex_example[i])) {
      return(specific_patterns$soc_code[i])
    }
  }
  
  # Fall back to single-keyword matches
  keyword_patterns <- crosswalk_primary %>%
    filter(!str_detect(keyword_pattern, "\\s"))
  
  for (i in seq_len(nrow(keyword_patterns))) {
    if (str_detect(title_lower, keyword_patterns$regex_example[i])) {
      return(keyword_patterns$soc_code[i])
    }
  }
  
  return(NA_character_)
}

your_jobs <- your_jobs %>%
  mutate(soc_code = map_chr(job_title, assign_soc))

# ─────────────────────────────────────────────────────────────────────────────
# STEP 5: Join to AI exposure scores
# ─────────────────────────────────────────────────────────────────────────────

jobs_with_exposure <- your_jobs %>%
  left_join(anthropic_exposure, by = "soc_code")

# Check coverage
coverage <- jobs_with_exposure %>%
  summarise(
    total_jobs       = n(),
    matched_soc      = sum(!is.na(soc_code)),
    matched_exposure = sum(!is.na(observed_exposure)),  # rename to match actual col
    pct_matched      = matched_exposure / total_jobs
  )

print(coverage)

# ─────────────────────────────────────────────────────────────────────────────
# STEP 6: Optional — get ALL matching SOC codes per job (one-to-many)
# Useful if you want to flag that a job could fall under multiple occupations
# ─────────────────────────────────────────────────────────────────────────────

jobs_multi_soc <- your_jobs %>%
  select(job_title) %>%
  distinct() %>%
  mutate(
    matched_socs = map(job_title, function(title) {
      title_lower <- str_to_lower(title)
      crosswalk %>%
        filter(map_lgl(regex_example, ~ str_detect(title_lower, .x))) %>%
        select(soc_code, soc_title, match_confidence)
    })
  ) %>%
  unnest(matched_socs)

# ─────────────────────────────────────────────────────────────────────────────
# STEP 7: Summary — mean exposure by keyword category
# ─────────────────────────────────────────────────────────────────────────────

exposure_summary <- jobs_with_exposure %>%
  group_by(soc_code) %>%
  summarise(
    n_jobs           = n(),
    mean_exposure    = mean(observed_exposure, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(crosswalk %>% select(soc_code, soc_title) %>% distinct(), by = "soc_code") %>%
  arrange(desc(mean_exposure))

print(exposure_summary)
