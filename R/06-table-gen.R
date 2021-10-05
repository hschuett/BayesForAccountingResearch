
# Setup -----------------------------------------------------------------------------
library(tidyverse)
library(flextable)


# Table 1 ---------------------------------------------------------------------------
panA <- read_csv("out/results/tab1-panA.csv") |>
  rename(variable = var)
panB <- read_csv("out/results/tab1-panB.csv") |>
  rename(variable = var)
panC <- read_csv("out/results/tab1-panC.csv") |>
  rename(variable = var)
panD <- read_csv("out/results/tab1-panD.csv")
panE <- read_csv("out/results/tab1-panE.csv")



panA$Firms <- NULL
raw_table <-
  bind_rows(panA, panB, panC, panD, panE) |>
  relocate(variable, N) |>
  filter(str_detect(variable, "b\\_t") == FALSE) |>
  mutate(panel = c(rep.int("Panel A: Sample descriptive statistics", 2),
                   rep.int("Panel B: Prior simulation check", 4),
                   rep.int("Panel C: ERC estimate comparison", 4),
                   rep.int("Panel D: Posterior of main model parameters", 6),
                   rep.int("Panel E: Posterior of main model parameters", 7)
                   )
                 )

raw_table

table1 <-
  as_grouped_data(raw_table, groups = c("panel")) |>
  flextable() |>
  font(part = "all", fontname = "Times New Roman") |>
  colformat_double(j = 4:10, digits = 3) |>
  colformat_double(i = 3, j = 4:10, digits = 4) |>
  merge_at(i = 1, j = 1:10) |> #, 4, 10)) |>
  merge_at(i = 4, j = 1:10) |>
  merge_at(i = 9, j = 1:10) |>
  merge_at(i = 14, j = 1:10) |>
  merge_at(i = 21, j = 1:10) |>
  bold(i = c(1, 4, 9, 14, 21)) |>
  hline(i = c(1, 4, 9, 14, 21)) |>
  set_header_labels(
    panel = "",
    variable = "",
    N = "",
    mean = "Mean",
    sd = "SD",
    q5 = "Q5",
    q25 = "Q25",
    q50 = "Median",
    q75 = "Q75",
    q95 = "Q95"
  ) |>
  autofit() |>
  width(j = 1, width = 0.5)


table1

save_as_docx("my table" = table1, path = "out/results/tab1.docx")


# Table 2 ---------------------------------------------------------------------------
tab2 <- read_csv("out/results/tab2.csv")
tab2 <- select(tab2, Case = case, N, `OOS-Rsq` = rsq, MAE = mae,  RMSE = rmse)

table2 <-
  tab2 |>
  flextable() |>
  font(part = "all", fontname = "Times New Roman") |>
  autofit()

table2

save_as_docx("my table" = table2, path = "out/results/tab2.docx")
