
# Init ------------------------------------------------------------------------------
library(tidyverse)
library(dbplyr)
library(RPostgres)




# Download data ---------------------------------------------------------------------
if (exists("wrds")) {
  dbDisconnect(wrds) # because otherwise WRDS might time out
}

wrds <- dbConnect(Postgres(),
  host = "wrds-pgdata.wharton.upenn.edu",
  port = 9737,
  user = rstudioapi::askForSecret("WRDS user"),
  password = rstudioapi::askForSecret("WRDS pw"),
  sslmode = "require",
  dbname = "wrds"
)
wrds # checking if connection exists



comp_company <-
  tbl(wrds, in_schema("comp", "company")) %>%
  select(gvkey, sic, fic)

raw_funda <-
  tbl(wrds, in_schema("comp", "funda")) %>%
  filter(
    indfmt == "INDL",
    datafmt == "STD",
    popsrc == "D",
    consol == "C",
    fyear > 1970
  ) %>%
  select(gvkey, datadate, conm, fyear, fyr, sich, oiadp, at)

compustat_all <-
  raw_funda %>%
  inner_join(comp_company, by = "gvkey") %>%
  filter(fic == "USA") %>%
  mutate(SIC = if_else(is.null(sich), as.numeric(sic), sich)) %>%
  select(-sich, -sic, -fic) %>%
  collect()

dbDisconnect(wrds)

summary(compustat_all)



# Cleaning data ---------------------------------------------------------------------

selected_industries <-
  compustat_all %>%
  filter(
    (SIC >= 6000 & SIC <= 6799) == FALSE,
    (SIC >= 9100 & SIC <= 9999) == FALSE
  )

raw_earn_data <-
  selected_industries %>%
  filter(at > 50) %>%
  distinct() %>%
  arrange(gvkey, datadate) %>%
  group_by(gvkey) %>%
  mutate(roa = oiadp / (0.5 * at + 0.5 * lag(at))) %>%
  mutate(lead_roa = lead(roa)) %>%
  ungroup()

summary(raw_earn_data[, c("roa", "lead_roa")])

earn_data <-
  raw_earn_data %>%
  filter(is.na(roa) == FALSE & is.na(lead_roa) == FALSE)



n_fyear0 <-
  earn_data %>%
  count(gvkey, name = "n_fyears")

quantile(n_fyear0$n_fyears, probs = c(0.25, 0.5, 0.75))
# 25% 50% 75%
#   3   7  16

n_fyear <-
  n_fyear0 %>%
  count(n_fyears)

n_fyear$pct <- with(n_fyear, n / sum(n))

print(n_fyear, n = 50)

earn_data %>%
  select(gvkey, fyear, roa, lead_roa) %>%
  saveRDS("data/earn-data.rds")
