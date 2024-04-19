# Setup ----------------------------
library("tidyverse")
library("readxl")
library("lubridate")
library("plotly")

# Load data ------------------------
lad <- read_tsv("data/LengthCriteriaDelta_sheet1.txt") %>%
    mutate_all(~ ifelse(. == "*", NA, .)) %>%
    mutate(year = ifelse(row_number() <= 184, 2023, 2024)) %>%
    select(
        month = 1,
        day = 2,
        w23_min = 3,
        w23_max = 4,
        w22_min = 5,
        w22_max = 6,
        lf_min = 8,
        lf_max = 9,
        f22_min = 11,
        f22_max = 12,
        f23_min = 13,
        f23_max = 14,
        s22_min = 16,
        s22_max = 17,
        s23_min = 18,
        s23_max = 19,
        year = year
    ) %>%
    mutate(
        date = lubridate::ymd(paste(year, month, day, sep = "-")),
        w23_min = as.numeric(w23_min),
        w23_max = as.numeric(w23_max),
        w22_min = as.numeric(w22_min),
        w22_max = as.numeric(w22_max),
        lf_min = as.numeric(lf_min),
        lf_max = as.numeric(lf_max),
        f22_min = as.numeric(f22_min),
        f22_max = as.numeric(f22_max),
        f23_min = as.numeric(f23_min),
        f23_max = as.numeric(f23_max),
        s22_min = as.numeric(s22_min),
        s22_max = as.numeric(s22_max),
        s23_min = as.numeric(s23_min),
        s23_max = as.numeric(s23_max)
    )


lad_long <- gather(lad, cohort, value, starts_with("w"), starts_with("lf"), starts_with("f"), starts_with("s"), na.rm = TRUE)
lad_long <- separate(lad_long, cohort, into = c("cohort", "type"), sep = "_", remove = FALSE)
lad_long <- spread(lad_long, type, value) %>%
    mutate(
        cohort = if_else(cohort == "lf" & date < as.Date("2024-04-01"), "lf23",
            if_else(cohort == "lf" & date >= as.Date("2024-04-01"), "lf24", cohort)
        )
    )

write_tsv(lad_long, "data/lad_long_WY2024.txt")

# add code to plot graph
data <- read_tsv("data/lad_long.txt")
excel_file <- read_excel("ONCOR_assignment_summary-WY2024-20240117.1_SaMT_SHERLOCK_Results.xlsx", sheet = "Working")
names(excel_file) <- make.names(names(excel_file)) # Make column names valid
df <- excel_file %>%
    mutate(
        SHERLOCK.Group = str_replace_all(SHERLOCK.Group, "\\*", ""),
        SHERLOCK.Assignment = str_replace_all(SHERLOCK.Assignment, "\\*", "")
    ) %>%
    filter(
        SHERLOCK.Group != "Likely heterozygote",
        SHERLOCK.Group != "Pending QA/QC",
        SHERLOCK.Group != "NA",
        GTSeq.Group != "Steelhead"
    ) %>%
    rename(SH_OTS28 = SH..OTS.28)

p <- plot_ly() %>%
    add_markers(data = df, x = ~SampleDate, y = ~ForkLength, color = ~SHERLOCK.Assignment, text = ~ID, size = I(20), alpha = 0.8) %>%
    add_ribbons( # Add ribbons for min/max values overlaid on top of fork length
        data = lad_long,
        x = ~date,
        ymin = ~min,
        ymax = ~max,
        fillcolor = ~cohort,
        showlegend = TRUE
    ) %>%
    layout(
        title = "SHERLOCK Excel Data Visualization",
        xaxis = list(title = "Sample Date"),
        yaxis = list(title = "Fork Length"),
        showlegend = TRUE
    )

p

excel_file %>%
    mutate(SampleDate = force_tz(SampleDate, tzone = "America/Los_Angeles"))
tsv_files <- list.files("data", pattern = "^lad_long.*\\.t$", full.names = TRUE)
lad_long <- purrr::map_dfr(tsv_files, read_tsv)
