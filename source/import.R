# import from Surveymonkey
# export all reactions to CSV, without filters, long column names and numerical
# values
library(git2rdata)
library(fs)
library(tidyverse)
path_home() |>
  path("Downloads", "CSV") -> source_dir
path(source_dir, "CollectorList.csv") |>
  read_csv() |>
  transmute(id = .data$CollectorID, survey = factor(.data$Title)) -> collector
path(source_dir, "Trendfiguren.csv") |>
  read_csv(skip = 1) -> answer
path(source_dir, "Trendfiguren.csv") |>
  readLines(n = 1) |>
  str_split(",", simplify = TRUE) -> columns
answer |>
  select(email = "E-mailadres") |>
  filter(!is.na(.data$email)) |>
  write_vc("email", "data", sorting = "email", optimize = FALSE)
answer |>
  select(feedback = "Open-Ended Response") |>
  filter(!is.na(.data$feedback)) |>
  write_vc("feedback", "data", sorting = "feedback", optimize = FALSE)

answer |>
  select(id = 1, survey = 2, numbers = 10, statistics = 11) |>
  mutate(
    numbers = factor(
      .data$numbers, levels = 1:4,
      labels = c(
        "not familiar", "not very familiar", "familiar", "very familiar"
      )
    ),
    statistics = factor(
      .data$statistics, levels = 1:4,
      labels = c(
        "not familiar", "not very familiar", "familiar", "very familiar"
      )
    )
  ) |>
  write_vc("background", "data", sorting = "id", optimize = FALSE)

answer |>
  select(id = 1, index = 15, 16:18) |>
  pivot_longer(
    starts_with("Response"), names_to = "trend", values_to = "reference",
    values_drop_na = TRUE
  ) |>
  mutate(
    index = factor(.data$index == 1, labels = c("change", "index")),
    trend = factor(
      .data$trend == "Response...17", labels = c("strong", "moderate")
    ),
    reference = .data$reference |>
      factor(levels = c(1:3), labels = c("none", "line", "line + text"))
  ) |>
  write_vc("reference", "data", sorting = c("id", "trend"), optimize = FALSE)

answer |>
  select(id = 1, 19:36) |>
  pivot_longer(
    -"id", names_to = "trend", values_to = "uncertainty", values_drop_na = TRUE
  ) |>
  mutate(
    trend = str_remove(.data$trend, "Response...") |>
      as.integer(),
    trend = .data$trend - 19,
    trend = factor(
      .data$trend %% 3, levels = 0:2,
      labels = c(
        "strong trend with high precision",
        "moderate trend with high precision",
        "moderate trend with low precision"
      )
    ),
    uncertainty = .data$uncertainty |>
      factor(levels = 1:3, labels = c("none", "band", "gradient"))
  ) |>
  write_vc("uncertainty", "data", sorting = c("id", "trend"), optimize = FALSE)

answer |>
  select(id = 1, 37:54) |>
  pivot_longer(-"id", values_to = "classification", values_drop_na = TRUE) |>
  transmute(
    .data$id,
    classification = .data$classification |>
      factor(levels = 1:3, labels = c("none", "symbol", "colour"))
  ) |>
  write_vc("classification", "data", sorting = "id", optimize = FALSE)

answer |>
  select(id = 1, 12:14) |>
  pivot_longer(
    -"id", names_to = "truth", values_to = "score", values_drop_na = TRUE
  ) |>
  mutate(
    truth = str_remove(.data$truth, "Response...") |>
      as.integer(),
    truth = str_replace(
      columns[12:14][.data$truth - 11], ".*de toestand van (\\d{4}).*", "\\1"
    ) |>
      factor(
        levels = c("2005", "2007", "2013"),
        labels = c("potential increase", "moderate increase", "stable")
      ),
    score = factor(
      .data$score, levels = 1:5,
      labels = c("lower", "comparable", "higher", "unclear", "no idea")
    ),
    after = FALSE, participant = FALSE
  ) -> interpretation_before
answer |>
  select(id = 1, 55:213) |>
  pivot_longer(
    -"id", names_to = "style", values_to = "score", values_drop_na = TRUE
  ) |>
  mutate(
    style = str_remove(.data$style, "Response...") |>
      as.integer(),
    style = .data$style - 55,
    truth = str_replace(
      columns[55:213][.data$style + 1], ".*de toestand van (\\d{4}).*", "\\1"
    ) |>
      factor(
        levels = c("2005", "2007", "2013"),
        labels = c("potential increase", "moderate increase", "stable")
      ),
    style = .data$style %/% 3,
    score = factor(
      .data$score, levels = 1:5,
      labels = c("lower", "comparable", "higher", "unclear", "no idea")
    ),
    after = TRUE
  ) -> interpretation_after
interpretation_after |>
  distinct(.data$id, .data$style) |>
  group_by(.data$id) |>
  mutate(participant = n() == 1 | .data$style == min(.data$style)) |>
  inner_join(x = interpretation_after, by = c("id", "style")) |>
  select(-"style") |>
  bind_rows(interpretation_before) |>
  write_vc(
    "interpretation", "data",
    sorting = c("id", "truth", "after", "participant"), optimize = FALSE
  )
