## analyze survey behavior data

survey_aq <- read_csv("./Processed Data/survey_aq_data.csv") |> 
            mutate(Survey = factor(Survey, levels = c("Pre_Intervention", "Post_Intervention"), ordered = TRUE)) 


Q25_Q29_summarize <- survey_aq |>
                    filter(QuestionID %in% c("Q25","Q29")) |>
                    group_by(QuestionID, Survey, ac.type) |>
                    summarize(responses = sum(!is.na(Response)), n = n()) |>
                    mutate(percent_responses = responses/n) |>
                    arrange(ac.type, QuestionID, Survey)

## of the responses to Q25 and Q29,
# what percentage of people responded with "1" (changed behavior)?


Q25_Q29_summarize_changed <- survey_aq |>
                    filter(QuestionID %in% c("Q25","Q29")) |>
                    filter(Response == "1") |>
                    group_by(QuestionID, Survey, ac.type) |>
                    summarize(n_changed = sum(!is.na(Response))) |>
                    left_join(Q25_Q29_summarize, by = c("QuestionID", "Survey", "ac.type")) |>
                    mutate(percent_changed = n_changed/responses) |>
                    arrange(ac.type, QuestionID, Survey)


write_csv(Q25_Q29_summarize_changed, "./Processed Data/Q25_Q29_behavior_summary.csv")

## Interesting. is there really a drop in behavior changed from before and after the Survey results

## Look at the differences in the responses. Did anyone actually change their response, or is it due to different number of responders?

survey_behavior_diff <- survey_aq|>
  filter(QuestionID %in% c("Q25","Q29"))|> # look at Question 25 and 29
  select(HouseID, Construct, Construct_general, Pollutant, Survey, QuestionID, Response) |>
  mutate(Response = as.integer(Response)) |>
  pivot_wider(names_from = Survey, values_from = c(Response)) |>
  mutate(Response_Change = Post_Intervention - Pre_Intervention)

names(survey_behavior_diff)

## H14 and H31 are the only homes that changed their response to Q25
## They changed from changing behavior when air quality is poor, to not changing their behavior.

## summarize

## now analyze the other questions grouped by the answers

# ── Subsetting keys ───────────────────────────────────────────

# HouseID + Survey combos where Q25 == "2" (no behavior change)
Q25_no_change <- survey_aq |>
  filter(QuestionID == "Q25" & Response == "2") |>
  select(HouseID, Survey)

# Q25 == "1" (Yes – changed behavior)
Q25_yes <- survey_aq |>
  filter(QuestionID == "Q25" & Response == "1") |>
  select(HouseID, Survey)

# Q29 == "1" (Yes – made home changes)
Q29_yes <- survey_aq |>
  filter(QuestionID == "Q29" & Response == "1") |>
  select(HouseID, Survey)

# ── Libraries, palettes ───────────────────────────────────────

library(ggpattern)   # install.packages("ggpattern") if needed
library(openxlsx)

okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
               "#0072B2", "#D55E00", "#CC79A7", "#000000",
               "#332288", "#117733", "#44AA99", "#88CCEE",
               "#DDCC77", "#882255")

# ── Helper function definitions ───────────────────────────────

build_answer_lookup <- function(qid) {
  answer_key |>
    filter(QuestionID == qid) |>
    pivot_longer(cols = matches("^A[0-9]+"), names_to = "Answer_code", values_to = "Answer_text") |>
    filter(!is.na(Answer_text)) |>
    mutate(Response = str_remove(Answer_code, "^A"))
}

build_expanded <- function(qid, subset_keys) {
  lookup <- build_answer_lookup(qid)

  rows <- survey_aq |>
    filter(QuestionID == qid)

  if (!is.null(subset_keys)) {
    rows <- rows |> inner_join(subset_keys, by = c("HouseID", "Survey"))
  }

  rows <- rows |> filter(!is.na(Response))

  expanded <- rows |>
    separate_rows(Response, sep = ",") |>
    mutate(Response = str_trim(Response)) |>
    filter(Response != "") |>            # drop empty strings from trailing commas
    left_join(lookup |> select(Response, Answer_text), by = "Response") |>
    mutate(QuestionID = qid)

  # If a home has a Q##_Other free-text entry but no "Other" row yet in the
  # expanded data (e.g. they wrote in the Other field without selecting the
  # Other checkbox), add an "Other" row for them so it appears in plots/tables.

  # Find the "Other" option in the answer key — match any text starting with
  # "other" (case-insensitive) to handle variants like "Other (specify)".
  other_answer <- lookup |>
    filter(str_detect(tolower(str_trim(Answer_text)), "^other"))

  # If "Other" is not in the answer key at all but Q##_Other responses exist,
  # create a synthetic entry placed after the last defined answer.
  if (nrow(other_answer) == 0) {
    has_other_responses <- survey_aq |>
      filter(QuestionID == paste0(qid, "_Other"), !is.na(Response)) |>
      nrow() > 0

    if (has_other_responses) {
      next_code  <- as.character(max(as.integer(lookup$Response), na.rm = TRUE) + 1)
      other_answer <- tibble(Response = next_code, Answer_text = "Other")
    }
  }

  if (nrow(other_answer) > 0) {
    other_text_rows <- survey_aq |>
      filter(QuestionID == paste0(qid, "_Other"), !is.na(Response))

    if (!is.null(subset_keys)) {
      other_text_rows <- other_text_rows |>
        inner_join(subset_keys, by = c("HouseID", "Survey"))
    }

    already_other <- expanded |>
      filter(!is.na(Answer_text),
             str_detect(tolower(str_trim(Answer_text)), "^other")) |>
      distinct(HouseID, Survey)

    missing_other <- other_text_rows |>
      anti_join(already_other, by = c("HouseID", "Survey")) |>
      mutate(Response    = other_answer$Response[1],
             Answer_text = other_answer$Answer_text[1],
             QuestionID  = qid)

    expanded <- bind_rows(expanded, missing_other)
  }

  expanded
}

build_crosstab <- function(expanded_df, qid, subset_keys) {
  # ordered answer rows from answer key (keep both number and text)
  answer_lookup <- build_answer_lookup(qid) |>
    arrange(as.integer(Response))
  answers      <- answer_lookup$Answer_text
  answer_nums  <- answer_lookup$Response          # "1", "2", "3", ...
  ans_num_map  <- setNames(answer_nums, answers)  # Answer_text → number

  # relevant homes
  if (!is.null(subset_keys)) {
    relevant_homes <- subset_keys |> distinct(HouseID) |> arrange(HouseID) |> pull(HouseID)
  } else {
    relevant_homes <- survey_aq |>
      filter(QuestionID == qid, !is.na(Response)) |>
      distinct(HouseID) |> arrange(HouseID) |> pull(HouseID)
  }

  # which Answer_text + HouseID + Survey combos were selected?
  answered <- expanded_df |>
    filter(!is.na(Answer_text)) |>
    distinct(HouseID, Survey, Answer_text) |>
    mutate(selected = "X")

  # free-text "Other" responses from the companion Q##_Other question
  other_text <- survey_aq |>
    filter(QuestionID == paste0(qid, "_Other"), !is.na(Response)) |>
    select(HouseID, Survey, other_response = Response)

  # all combinations of answer × home × survey
  expand.grid(
    Answer_text = answers,
    HouseID     = relevant_homes,
    Survey      = c("Pre_Intervention", "Post_Intervention"),
    stringsAsFactors = FALSE
  ) |>
    left_join(answered, by = c("Answer_text", "HouseID", "Survey")) |>
    left_join(other_text, by = c("HouseID", "Survey")) |>
    mutate(
      selected = case_when(
        tolower(str_trim(Answer_text)) == "other" & !is.na(other_response) ~ other_response,
        TRUE ~ replace_na(selected, "")
      ),
      col_label = paste0(HouseID, "_", ifelse(Survey == "Pre_Intervention", "pre", "post"))
    ) |>
    select(Answer_text, col_label, selected) |>
    pivot_wider(names_from = col_label, values_from = selected) |>
    # interleave pre/post columns per home
    select(Answer_text,
           unlist(lapply(relevant_homes, \(h) c(paste0(h, "_pre"), paste0(h, "_post"))))) |>
    # combine number and answer text into one column: "1. Answer text"
    mutate(Answer_text = paste0(ans_num_map[Answer_text], ". ", Answer_text)) |>
    (\(tbl) {
      # Label each home as "H## (AC)" or "H## (EC)"
      ac_lookup <- survey_aq |> distinct(HouseID, ac.type)
      home_labels <- tibble(HouseID = relevant_homes) |>
        left_join(ac_lookup, by = "HouseID") |>
        mutate(label = paste0(HouseID, " (", ac.type, ")")) |>
        pull(label)

      # Column names (top row): Response | H02 (AC) | H02 (AC) | H05 (EC) | ...
      tbl <- as.data.frame(tbl)
      attr(tbl, "names") <- c("Response", rep(home_labels, each = 2))
      # First data row: "Response" | "pre" | "post" | "pre" | "post" | ...
      label_row <- as.data.frame(
        matrix(c("Response", rep(c("pre", "post"), length(relevant_homes))), nrow = 1),
        stringsAsFactors = FALSE
      )
      attr(label_row, "names") <- attr(tbl, "names")
      rbind(label_row, tbl)
    })()
}

plot_behavior_q <- function(expanded_df, filtered_df, parent_keys,
                             parent_label, qid, question_title, filename) {

  # annotation counts
  parent_n <- parent_keys |>
    count(Survey, name = "n_parent")

  n_respondents <- filtered_df |>
    group_by(Survey) |>
    summarise(n_respondents = n(), .groups = "drop")

  label_data <- parent_n |>
    left_join(n_respondents, by = "Survey") |>
    mutate(label = paste0(parent_label, ": ", n_parent,
                          "\nResponded to ", qid, ": ", n_respondents))

  # Numbered, ordered answer labels (matches crosstab order)
  all_answers <- build_answer_lookup(qid) |>
    arrange(as.integer(Response)) |>
    mutate(num_label = paste0(Response, ". ", Answer_text))

  y_levels <- rev(all_answers$num_label)   # answer 1 at top in ggplot

  expanded_labeled <- expanded_df |>
    filter(!is.na(Answer_text)) |>
    left_join(all_answers |> select(Answer_text, num_label), by = "Answer_text")

  # Summary includes all answers, even those with zero responses
  q_summary <- expanded_labeled |>
    group_by(Survey, num_label) |>
    summarise(n = n(), .groups = "drop") |>
    complete(Survey, num_label = all_answers$num_label, fill = list(n = 0)) |>
    mutate(num_label = factor(num_label, levels = y_levels))

  print(
    ggplot(q_summary, aes(x = n, y = num_label)) +
      geom_bar(stat = "identity") +
      geom_label(data = label_data,
                 aes(x = Inf, y = Inf, label = label),
                 hjust = 1, vjust = 1, inherit.aes = FALSE, size = 3) +
      scale_x_continuous(breaks = function(x) seq(0, floor(max(x)), by = 1)) +
      scale_y_discrete(labels = function(x) str_wrap(x, width = 25)) +
      facet_wrap(~ Survey) +
      labs(title = question_title, x = "Number of Responses", y = "Answer") +
      theme_bw()
  )

  # coloured-by-house plot
  p <- ggplot(expanded_labeled, aes(y = num_label, fill = HouseID, pattern = ac.type)) +
    geom_bar_pattern(
      pattern_colour  = "white",
      pattern_density = 0.35,
      pattern_spacing = 0.03,
      colour          = "white",
      linewidth       = 0.2
    ) +
    scale_pattern_manual(values = c(AC = "none", EC = "stripe"), name = "Unit Type") +
    scale_y_discrete(limits = y_levels, labels = scales::label_wrap(30)) +
    scale_fill_manual(values = okabe_ito, name = "House ID") +
    facet_wrap(~ Survey) +
    geom_label(data = label_data,
               aes(x = Inf, y = Inf, label = label),
               hjust = 1, vjust = 1, inherit.aes = FALSE, size = 3) +
    labs(
      title  = stringr::str_wrap(question_title, width = 60),
      x      = "Number of Responses",
      y      = "Answer"
    ) +
    scale_x_continuous(breaks = function(x) seq(0, floor(max(x)), by = 1)) +
    theme_bw() +
    theme(axis.text    = element_text(size = 10),
          axis.title   = element_text(size = 10),
          legend.text  = element_text(size = 10),
          legend.title = element_text(size = 10),
          strip.text   = element_text(size = 10),
          plot.title   = element_text(size = 12, face = "bold"))

  ggsave(
    filename = filename,
    plot     = p,
    width    = 7, height = 5, dpi = 300, units = "in", bg = "white"
  )
}

write_crosstab_xlsx <- function(crosstab_df, xlsx_path) {
  wb <- createWorkbook()
  addWorksheet(wb, "Sheet1")

  n_cols    <- ncol(crosstab_df)
  col_names <- names(crosstab_df)   # HouseID row (may have duplicates)

  # ── Styles ──────────────────────────────────────────────────
  hdr_center <- createStyle(
    fontName = "Aptos Narrow", fontSize = 11, textDecoration = "bold",
    fgFill = "#BDD7EE", halign = "CENTER", valign = "center",
    border = "TopBottomLeftRight", borderColour = "black", borderStyle = "thin"
  )
  hdr_left <- createStyle(
    fontName = "Aptos Narrow", fontSize = 11, textDecoration = "bold",
    fgFill = "#BDD7EE", halign = "LEFT", valign = "center",
    border = "TopBottomLeftRight", borderColour = "black", borderStyle = "thin"
  )
  data_left <- createStyle(
    fontName = "Aptos Narrow", fontSize = 11,
    halign = "LEFT", valign = "top", wrapText = TRUE,
    border = "TopBottomLeftRight", borderColour = "black", borderStyle = "thin"
  )
  data_center <- createStyle(
    fontName = "Aptos Narrow", fontSize = 11,
    halign = "CENTER", valign = "center", wrapText = TRUE,
    border = "TopBottomLeftRight", borderColour = "black", borderStyle = "thin"
  )
  alt_left <- createStyle(
    fontName = "Aptos Narrow", fontSize = 11,
    fgFill = "#F2F2F2", halign = "LEFT", valign = "top", wrapText = TRUE,
    border = "TopBottomLeftRight", borderColour = "black", borderStyle = "thin"
  )
  alt_center <- createStyle(
    fontName = "Aptos Narrow", fontSize = 11,
    fgFill = "#F2F2F2", halign = "CENTER", valign = "center", wrapText = TRUE,
    border = "TopBottomLeftRight", borderColour = "black", borderStyle = "thin"
  )

  # ── Row 1: HouseID header ────────────────────────────────────
  writeData(wb, 1, as.data.frame(t(col_names)),
            startRow = 1, startCol = 1, colNames = FALSE)
  addStyle(wb, 1, hdr_center, rows = 1, cols = 1:n_cols, gridExpand = TRUE)
  addStyle(wb, 1, hdr_left,   rows = 1, cols = 1)

  # Merge each HouseID pair: cols 2+3, 4+5, ...
  for (i in seq(2, n_cols - 1, by = 2)) {
    mergeCells(wb, 1, cols = i:(i + 1), rows = 1)
  }

  # ── Row 2: pre/post label row ────────────────────────────────
  writeData(wb, 1, as.data.frame(t(as.character(crosstab_df[1, ]))),
            startRow = 2, startCol = 1, colNames = FALSE)
  addStyle(wb, 1, hdr_center, rows = 2, cols = 1:n_cols, gridExpand = TRUE)
  addStyle(wb, 1, hdr_left,   rows = 2, cols = 1)

  # ── Data rows ────────────────────────────────────────────────
  n_data <- nrow(crosstab_df) - 1
  for (ri in seq_len(n_data)) {
    excel_row <- ri + 2
    row_vals  <- as.character(crosstab_df[ri + 1, ])
    row_vals[row_vals == ""] <- NA   # write blanks as truly empty cells

    writeData(wb, 1, as.data.frame(t(row_vals)),
              startRow = excel_row, startCol = 1, colNames = FALSE)

    if (ri %% 2 == 1) {
      addStyle(wb, 1, alt_left,   rows = excel_row, cols = 1)
      addStyle(wb, 1, alt_center, rows = excel_row, cols = 2:n_cols, gridExpand = TRUE)
    } else {
      addStyle(wb, 1, data_left,   rows = excel_row, cols = 1)
      addStyle(wb, 1, data_center, rows = excel_row, cols = 2:n_cols, gridExpand = TRUE)
    }
  }

  # ── Column widths & freeze ───────────────────────────────────
  setColWidths(wb, 1, cols = 1,        widths = 40)
  setColWidths(wb, 1, cols = 2:n_cols, widths = 10)
  freezePane(wb, 1, firstActiveRow = 3, firstActiveCol = 2)

  saveWorkbook(wb, xlsx_path, overwrite = TRUE)
}

# ── Build expanded tables (Q25–Q30) ──────────────────────────

Q25_expanded <- build_expanded("Q25", NULL)
Q26_expanded <- build_expanded("Q26", Q25_no_change)
Q27_expanded <- build_expanded("Q27", Q25_yes)
Q28_expanded <- build_expanded("Q28", Q25_yes)
Q29_expanded <- build_expanded("Q29", NULL)
Q30_expanded <- build_expanded("Q30", Q29_yes)

# Combined expanded table for Q26–Q30 with answer key merged
Q26_Q30_expanded <- bind_rows(Q26_expanded, Q27_expanded, Q28_expanded,
                               Q29_expanded, Q30_expanded) |>
  left_join(
    answer_key |> select(QuestionID, Question, matches("^A[0-9]+")),
    by = "QuestionID"
  )

View(Q26_Q30_expanded)

# ── Q26: annotation data ──────────────────────────────────────

## Analyze Q26 (why no behavior change) for households where Q25 Response == 2

# Q26 rows for Q25-no-change combos, non-NA responses only
Q26_filtered <- survey_aq |>
  filter(QuestionID == "Q26") |>
  inner_join(Q25_no_change, by = c("HouseID", "Survey")) |>
  filter(!is.na(Response))

# Build per-Survey label for geom_label annotation (n who said No to Q25, n who responded to Q26)
Q25_no_n <- Q25_no_change |>
  count(Survey, name = "n_Q25_no")

Q26_n_respondents <- Q26_filtered |>
  group_by(Survey) |>
  summarise(n_respondents = n(), .groups = "drop")

Q26_label_data <- Q25_no_n |>
  left_join(Q26_n_respondents, by = "Survey") |>
  mutate(label = paste0("Said No to Q25: ", n_Q25_no,
                        "\nResponded to Q26: ", n_respondents))

# Numbered, ordered answer labels for Q26 (matches crosstab order)
Q26_all_answers <- build_answer_lookup("Q26") |>
  arrange(as.integer(Response)) |>
  mutate(num_label = paste0(Response, ". ", Answer_text))

Q26_y_levels <- rev(Q26_all_answers$num_label)   # answer 1 at top in ggplot

Q26_expanded_labeled <- Q26_expanded |>
  filter(!is.na(Answer_text)) |>
  left_join(Q26_all_answers |> select(Answer_text, num_label), by = "Answer_text")

# Summary includes all answers, even those with zero responses
Q26_summary <- Q26_expanded_labeled |>
  group_by(Survey, num_label) |>
  summarise(n = n(), .groups = "drop") |>
  complete(Survey, num_label = Q26_all_answers$num_label, fill = list(n = 0)) |>
  mutate(num_label = factor(num_label, levels = Q26_y_levels))

ggplot(Q26_summary, aes(x = n, y = num_label)) +
  geom_bar(stat = "identity") +
  geom_label(data = Q26_label_data,
             aes(x = Inf, y = Inf, label = label),
             hjust = 1, vjust = 1,
             inherit.aes = FALSE,
             size = 3) +
  scale_x_continuous(breaks = function(x) seq(0, floor(max(x)), by = 1)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 25)) +
  facet_wrap(~ Survey) +
  labs(
    title = "Q26: Why don't you change your behavior when air quality is poor?",
    x = "Number of Responses",
    y = "Answer"
  ) +
  theme_bw()

###
ggplot(Q26_expanded_labeled, aes(y = num_label, fill = HouseID, pattern = ac.type)) +
  geom_bar_pattern(
    pattern_colour  = "white",
    pattern_density = 0.35,
    pattern_spacing = 0.03,
    colour          = "white",
    linewidth       = 0.2
  ) +
  scale_pattern_manual(values = c(AC = "none", EC = "stripe"), name = "Unit Type") +
  scale_y_discrete(
    limits = Q26_y_levels,
    labels = scales::label_wrap(30)
  ) +
  scale_fill_manual(values = okabe_ito, name = "House ID") +
  facet_wrap(~ Survey) +
    geom_label(data = Q26_label_data,
             aes(x = Inf, y = Inf, label = label),
             hjust = 1, vjust = 1,
             inherit.aes = FALSE,
             size = 3) +
  labs(
     title = stringr::str_wrap(
      "Q26: Why don't you change your behavior when air quality is poor?",
      width = 60
    ),
    x = "Number of Responses",
    y = "Answer"
  ) +
  scale_x_continuous(breaks = function(x) seq(0, floor(max(x)), by = 1)) +
  theme_bw()+
  theme(axis.text = element_text(size = 10),
         axis.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        strip.text = element_text(size = 10),
        plot.title = element_text(size = 12, face = "bold"))

ggsave(
  filename = "./Graphics/Q26_plot.png",
  plot = last_plot(),
  width = 7,        # full page width (inches)
  height = 5,       # adjust to content
  dpi = 300,        # journal standard (some require 600)
  units = "in",
  bg = "white"
)

# ── Q27 / Q28 / Q30 plots ────────────────────────────────────

## ── Q27: how do you know when the air is polluted? ───────────

Q27_filtered <- survey_aq |>
  filter(QuestionID == "Q27") |>
  inner_join(Q25_yes, by = c("HouseID", "Survey")) |>
  filter(!is.na(Response))

plot_behavior_q(
  expanded_df  = Q27_expanded,
  filtered_df  = Q27_filtered,
  parent_keys  = Q25_yes,
  parent_label = "Said Yes to Q25",
  qid          = "Q27",
  question_title = "Q27: How do you know when the outdoor air is polluted in Utah County?",
  filename     = "./Graphics/Q27_plot.png"
)

## ── Q28: how do you change your behavior? ────────────────────

Q28_filtered <- survey_aq |>
  filter(QuestionID == "Q28") |>
  inner_join(Q25_yes, by = c("HouseID", "Survey")) |>
  filter(!is.na(Response))

plot_behavior_q(
  expanded_df  = Q28_expanded,
  filtered_df  = Q28_filtered,
  parent_keys  = Q25_yes,
  parent_label = "Said Yes to Q25",
  qid          = "Q28",
  question_title = "Q28: How do you change your behavior when the outdoor air is polluted?",
  filename     = "./Graphics/Q28_plot.png"
)

## ── Q30: what changes have you made to your home? ────────────

Q30_filtered <- survey_aq |>
  filter(QuestionID == "Q30") |>
  inner_join(Q29_yes, by = c("HouseID", "Survey")) |>
  filter(!is.na(Response))

plot_behavior_q(
  expanded_df  = Q30_expanded,
  filtered_df  = Q30_filtered,
  parent_keys  = Q29_yes,
  parent_label = "Said Yes to Q29",
  qid          = "Q30",
  question_title = "Q30: What changes have you made to your home to improve indoor air quality?",
  filename     = "./Graphics/Q30_plot.png"
)

# ── Cross-tab tables ──────────────────────────────────────────

# Subsetting rules per question (for reference):
#   Q25 → all homes
#   Q26 → Q25 == "2" (No)
#   Q27 → Q25 == "1" (Yes)
#   Q28 → Q25 == "1" (Yes)
#   Q29 → all homes
#   Q30 → Q29 == "1" (Yes)

Q25_crosstab <- build_crosstab(Q25_expanded, "Q25", NULL)
Q26_crosstab <- build_crosstab(Q26_expanded, "Q26", Q25_no_change)
Q27_crosstab <- build_crosstab(Q27_expanded, "Q27", Q25_yes)
Q28_crosstab <- build_crosstab(Q28_expanded, "Q28", Q25_yes)
Q29_crosstab <- build_crosstab(Q29_expanded, "Q29", NULL)
Q30_crosstab <- build_crosstab(Q30_expanded, "Q30", Q29_yes)

View(Q25_crosstab)
View(Q26_crosstab)
View(Q27_crosstab)
View(Q28_crosstab)
View(Q29_crosstab)
View(Q30_crosstab)

# ── xlsx export ───────────────────────────────────────────────

write_crosstab_xlsx(Q25_crosstab, "./Processed Data/Q25_crosstab.xlsx")
write_crosstab_xlsx(Q26_crosstab, "./Processed Data/Q26_crosstab.xlsx")
write_crosstab_xlsx(Q27_crosstab, "./Processed Data/Q27_crosstab.xlsx")
write_crosstab_xlsx(Q28_crosstab, "./Processed Data/Q28_crosstab.xlsx")
write_crosstab_xlsx(Q29_crosstab, "./Processed Data/Q29_crosstab.xlsx")
write_crosstab_xlsx(Q30_crosstab, "./Processed Data/Q30_crosstab.xlsx")
