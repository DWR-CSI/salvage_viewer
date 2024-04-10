# Setup ----------------------------
library("tidyverse")

# Load data ------------------------
lad <- read_tsv("data\\LengthCriteriaDelta_sheet1.txt") %>%
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
    )
