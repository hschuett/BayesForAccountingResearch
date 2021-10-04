library(tidyverse)
library(dbplyr)
library(RPostgres)
library(bizdays)

begin_date = "1990-01-01"
end_date = "2020-12-31"

cal <- create.calendar("mycal", weekdays = c("saturday", "sunday"))
write_parquet <- function(x, p) {
  arrow::write_parquet(x, p, compression = "gzip", compression_level = 5)
}


wrds <- dbConnect(Postgres(),
                  host = "wrds-pgdata.wharton.upenn.edu",
                  port = 9737,
                  user = keyring::key_get("wrds_user"),
                  password = keyring::key_get("wrds_pw"),
                  sslmode = "require",
                  dbname = "wrds"
)



# Create ibes crsp linking table ------------------------------------------
ibes_cusip_link <-
  tbl(wrds, in_schema("ibes", "idsum")) %>%
  # arrange(ticker, cusip, sdates) %>%
  filter(usfirm == 1 & is.na(cusip) == F) %>%
  group_by(ticker, cusip) %>%
  summarize(
    fdate = min(sdates, na.rm = TRUE),
    ldate = max(sdates, na.rm = TRUE),
    lname = max(cname, na.rm = TRUE)
  ) %>%
  ungroup()

crsp_cusip_link <-
  tbl(wrds, in_schema("crsp", "stocknames")) %>%
  # arrange(permno, ncusip, namedt, nameenddt) %>%
  filter(is.na(ncusip) == F) %>%
  group_by(permno, ncusip) %>%
  summarize(
    fdate = min(namedt, na.rm = TRUE),
    ldate = max(nameenddt, na.rm = TRUE),
    lname = max(comnam, na.rm = TRUE)
  ) %>%
  ungroup()

iclink <-
  ibes_cusip_link %>%
  inner_join(crsp_cusip_link,
             by = c("cusip" = "ncusip"),
             suffix = c("_i", "_c")
  ) %>%
  mutate(Score = if_else(!(ldate_i < fdate_c) & !(ldate_c < fdate_i), 0, 1)) %>%
  mutate(Score = if_else(lname_i != lname_c & Score == 1, Score + 1, Score)) %>%
  filter(Score <= 1) %>%
  collect()

iclink_clean1 <-
  iclink %>%
  filter(!(str_detect(.$ticker, "/\\d"))) %>%
  # arrange(ticker, permno, fdate_i, fdate_c, ldate_i, ldate_c) %>%
  group_by(ticker, permno) %>%
  summarize(
    fdate_i = min(fdate_i, na.rm = TRUE),
    ldate_i = max(ldate_i, na.rm = TRUE),
    fdate_c = min(fdate_c, na.rm = TRUE),
    ldate_c = max(ldate_c, na.rm = TRUE),
    lname_i = max(lname_i, na.rm = TRUE),
    lname_c = max(lname_c, na.rm = TRUE)
  ) %>%
  ungroup()

iclink_clean2 <-
  iclink_clean1 %>%
  arrange(ticker, permno, fdate_i, fdate_c, ldate_i, ldate_c) %>%
  group_by(ticker) %>%
  mutate(
    lag_fdate_i = lag(fdate_i),
    lag_ldate_i = lag(ldate_i)
  ) %>%
  ungroup() %>%
  filter(is.na(lag_fdate_i) == T | !(fdate_i >= lag_fdate_i & ldate_i <= lag_ldate_i)) %>%
  select(ticker, permno, fdate_i, fdate_c, ldate_i, ldate_c, lname_c, lname_i)

# The SCORE variable is 0 and determined to be ‘best’ when the linked CUSIP has
# intersecting dates and matching company names. Small differences in company
# names (CNAME in IBES and COMNAM in CRSP) can be checked for and tolerated
# using SPEDIS, which is the spelling distance function in SAS. SPEDIS
# (cname,comnam)=0 is a perfect score and SPEDIS < 30 is usually good enough to
# be considered a name match. Matches with intersecting CUSIP dates ranges but
# with substantially different companies names are assigned a score level of 1.
# In this exercise, 200 cases with non-intersecting CUSIP dates are
# identified. These matches are further explored by calculating the spelling
# distance between IBES and CRSP company names. Those cases are assigned a score
# level of 2 if company names match, and 3 otherwise. [wrds
# support](https://wrds-www.wharton.upenn.edu/pages/support/applications/linking-databases/linking-ibes-and-crsp-data/)


# !Be aware. IBES ticker matches to more than one permno depending on the time date used
# e.g.
# ticker permno fdate_i    fdate_c    ldate_i    ldate_c    lname_c             lname_i
# <chr>   <dbl> <date>     <date>     <date>     <date>     <chr>               <chr>
# 1 G       19721 1976-01-15 1968-01-02 1991-05-16 1996-08-15 GREYHOUND DIAL CORP GREYHOUND CP
# 2 G       83821 1996-09-19 1996-08-16 2000-02-17 2004-03-29 DIAL CORP NEW       DIAL CORPORATION


ccm_link <-
  tbl(wrds, in_schema("crsp", "ccmxpf_linktable")) %>%
  filter(
    usedflag %in% c(0, 1),
    linktype %in% c("LU", "LC"),
    linkprim %in% c("P", "C")
  ) %>%
  mutate(linkenddt = if_else(is.na(linkenddt) == T,
                             date("2030-06-30"), linkenddt
  )) %>%
  collect()

final_linkt_table <-
  ccm_link %>%
  inner_join(iclink_clean2, by = c("lpermno" = "permno")) %>%
  filter(is.na(linkdt) == T | (linkdt <= ldate_i & ldate_i <= linkenddt) | linkdt <= ldate_c & ldate_c <= linkenddt)
#    gvkey linkprim liid linktype lpermno lpermco usedflag     linkdt  linkenddt ticker    fdate_i
# 1 005342        P   01       LC   19721   20851        1 1962-01-31 2004-06-30      G 1976-01-15
# 2 063500        P   01       LC   83821   31706        1 1996-08-16 2004-03-29      G 1996-09-19
#      fdate_c    ldate_i    ldate_c             lname_c          lname_i
# 1 1968-01-02 1991-05-16 1996-08-15 GREYHOUND DIAL CORP     GREYHOUND CP
# 2 1996-08-16 2000-02-17 2004-03-29       DIAL CORP NEW DIAL CORPORATION

chosen_gvkey <- unique(final_linkt_table$gvkey)
chosen_permno <- unique(final_linkt_table$lpermno)
chosen_ticker <- unique(final_linkt_table$ticker)

rm(
  ccm_link, iclink, iclink_clean1, iclink_clean2,
  ibes_cusip_link, crsp_cusip_link
)



# Collect return data -----------------------------------------------------
delisting_returns <-
  tbl(wrds, in_schema("crsp", "dsedelist")) %>%
  select(permno, dlstdt, dlret) %>%
  mutate(dlret = if_else(is.na(dlret), -1, dlret))

returns <-
  tbl(wrds, in_schema("crsp", "dsf")) %>%
  filter(permno %in% chosen_permno) %>%
  left_join(delisting_returns, by = c("permno", "date" = "dlstdt")) %>%
  group_by(permno) %>%
  mutate(
    Price = abs(prc),
    MVE = Price * shrout,
    MinPrc = min(Price, na.rm = T),
    ret = if_else(is.na(ret) == T & is.na(dlret) == F, dlret, ret)
  ) %>%
  ungroup() %>%
  filter(
    between(date, begin_date, end_date),
    MinPrc > 1
  ) %>%
  select(permno, date, ret, Price, MVE, cfacshr)

indices <-
  tbl(wrds, in_schema("crsp", "dsi")) %>%
  select(date, MktRet = vwretd) %>%
  filter(between(date, begin_date, end_date))

compinfo <-
  tbl(wrds, in_schema("crsp", "dsenames")) %>%
  filter(
    shrcd %in% c(10, 11),
    exchcd %in% c(1, 2, 3),
    primexch %in% c("N", "A", "Q"),
    !(between(siccd, 4400, 5500) | between(siccd, 6000, 6500))
  ) %>%
  select(permno, namedt, nameendt)

crsp <-
  returns %>%
  inner_join(indices, by = "date") %>%
  inner_join(compinfo, by = "permno") %>%
  filter(between(date, namedt, nameendt)) %>%
  select(-namedt, -nameendt) %>%
  collect()

# cumulative share adjustment factor
cumshare_adjust_factor <-
  crsp %>%
  select(permno, date, cfacshr)

rm(delisting_returns, returns, indices, compinfo)



# Getting actuals + adjust for rdq from compustat quarterly ---------------

# We checked many instances, and it appears that ANNDATS and RDQE are the same,
# but in case you notice any difference between them, I recommend that you use
# RDQ as a more reliable "announcement date".
# [wrds support](https://wrds-www.wharton.upenn.edu/pages/support/support-articles/ibes/difference-between-ibes-earnings-announcement-date-and-compustat-announcement-dates/)
comp_dates <-
  tbl(wrds, in_schema("comp", "fundq")) %>%
  filter(
    indfmt == "INDL" &
      datafmt == "STD" &
      popsrc == "D" &
      consol == "C" &
      between(datadate, begin_date, end_date) &
      gvkey %in% chosen_gvkey
  ) %>%
  select(gvkey, conm, rdq, datadate, ceqq) %>%
  distinct() %>%
  collect()

matched_comp_dates <-
  comp_dates %>%
  left_join(final_linkt_table, by = "gvkey") %>%
  filter(
    is.na(linkdt) == T | (linkdt <= datadate & datadate <= linkenddt),
    !(linkenddt < fdate_c) & !(ldate_c < linkdt),
    # Next is because we still have multiple ticker -> permno -> gvkey matches. Take the one in the _c range
    (fdate_c <= datadate & datadate <= ldate_c),
    (fdate_i <= datadate & datadate <= ldate_i)
  ) %>%
  group_by(gvkey, datadate) %>%
  # taking only primary link if multiple permnos per gvkey
  mutate(countkey2 = n()) %>%
  ungroup() %>%
  filter(countkey2 == 1 | (countkey2 > 1 & liid == "01")) %>%
  select(-countkey2) %>%
  add_count(gvkey, datadate) %>%
  filter(n == 1) %>%
  select(ticker, pends = datadate, rdq, ceqq) %>%
  distinct()

ibes_actuals <-
  tbl(wrds, in_schema("ibes", "actu_epsus")) %>%
  filter(
    measure == "EPS",
    pdicity == "QTR",
    curr_act == "USD",
    ticker %in% chosen_ticker,
    between(pends, begin_date, end_date),
  ) %>%
  select(ticker, cname, pends, anndats, value) %>%
  collect()

actuals_fxd_dates <-
  ibes_actuals %>%
  left_join(matched_comp_dates, by = c("ticker", "pends")) %>%
  mutate(ea_date = if_else(is.na(rdq), anndats, rdq)) %>%
  select(-anndats, -rdq) %>%
  rename(
    fpend_date = pends,
    actual_eps = value
  ) %>%
  filter(is.na(actual_eps) == FALSE)

colSums(is.na(actuals_fxd_dates))

rm(comp_dates, matched_comp_dates, ibes_actuals)



# Collect lates ibes fcst data --------------------------------------------
qtrly_eps_fcast <-
  tbl(wrds, in_schema("ibes", "detu_epsus")) %>%
  rename(
    forecast_date = anndats,
    fpend_date = fpedats,
    forecast = value,
    analyst = estimator
  ) %>%
  filter(
    fpi == "6",  # one-quarter ahead forecasts
    measure == "EPS",
    usfirm == 1,
    report_curr == "USD",
    forecast_date < fpend_date,
    ticker %in% chosen_ticker,
    between(fpend_date, begin_date, end_date)
  ) %>%
  select(ticker, fpend_date, analyst, forecast_date, forecast, pdf, anntims) %>%
  group_by(ticker, fpend_date, analyst) %>%
  filter(forecast_date == max(forecast_date, na.rm = TRUE)) %>% # take the latest forecast by analyst
  ungroup() %>%
  collect()

qtrly_eps_fcast <-  # a few analysts activate more than one forecast on the same day. this takes the last
  qtrly_eps_fcast %>%
  arrange(ticker, fpend_date, analyst, forecast_date, -anntims) %>%
  group_by(ticker, fpend_date, analyst, forecast_date) %>%
  slice(1) %>%
  ungroup() %>%
  select(-anntims)

qtrly_eps_fcast <-  # Only keep those with at least 3 forecasts before kicking out stale forecasts
  qtrly_eps_fcast %>%
  add_count(ticker, fpend_date, name = "num_forecasts") %>%
  filter(num_forecasts >= 3)

dbDisconnect(wrds)


# Merge actuals and forecasts  ------------------------------------------------------
head(actuals_fxd_dates)
head(qtrly_eps_fcast)

permno_link <-
  final_linkt_table %>%
  select(ticker, permno = lpermno, fdate_i, ldate_i, fdate_c, ldate_c, linkdt, linkenddt) %>%
  distinct()


qtrly_eps_actuals <-
  actuals_fxd_dates %>%
  select(-cname, -ceqq) %>%
  inner_join(permno_link, by = c("ticker")) %>%
  filter(
    is.na(linkdt) == T | (linkdt <= fpend_date & fpend_date <= linkenddt),
    !(linkenddt < fdate_c) & !(ldate_c < linkdt),
    # Next is because we still have multiple ticker -> permno -> gvkey matches. Take the one in the _c range
    (fdate_c <= fpend_date & fpend_date <= ldate_c),
    (fdate_i <= fpend_date & fpend_date <= ldate_i)
  )  %>%
  select(-fdate_i, -ldate_i, -fdate_c, -ldate_c, -linkdt, -linkenddt)

cleaned_ibes <-
  qtrly_eps_actuals %>%
  inner_join(qtrly_eps_fcast, by = c("ticker", "fpend_date")) %>%
  mutate(
    forecast_staleness = ea_date - forecast_date,
    ea_delay = ea_date - fpend_date
  ) %>%
  filter(
    forecast_staleness < 180,
    ea_delay < 180
  ) %>%
  select(-num_forecasts) %>%
  add_count(ticker, fpend_date, name = "num_forecasts") %>%
  filter(num_forecasts >= 3)

# adjusting for intermediate stock splits ala Bissessur Veenman 2016 "Analyst Information Precision"
cleaned_ibes_adjusted <-
  cleaned_ibes %>%
  select(-num_forecasts, -forecast_staleness, -ea_delay) %>%
  left_join(
    cumshare_adjust_factor %>% rename(ea_cfacshr = cfacshr),
    by = c("permno", "ea_date" = "date")
  ) %>%
  left_join(
    cumshare_adjust_factor %>% rename(fcst_cfacshr = cfacshr),
    by = c("permno", "forecast_date" = "date")
  ) %>%
  mutate(adj_factor = ea_cfacshr / fcst_cfacshr) %>%
  mutate(forecast = if_else(is.na(adj_factor) == T, forecast, forecast * adj_factor)) %>%
  select(-fcst_cfacshr, -ea_cfacshr, -adj_factor)

earnings_surprise <-
  cleaned_ibes %>%
  group_by(ticker, permno, fpend_date, ea_date, actual_eps) %>%
  summarize(
    median_fcast_eps = median(forecast),
    num_forecasts = n(),
    .groups = "drop") %>%
  ungroup() %>%
  mutate(two_days_bef_ea = add.bizdays(ea_date, -2, cal)) %>%
  inner_join(
    crsp %>% select(permno, date, Price),
    by = c("permno", "two_days_bef_ea" = "date")
  ) %>%
  mutate(earn_surp = (actual_eps - median_fcast_eps) / Price) %>%
  filter(is.na(earn_surp) == F)

with(earnings_surprise, summary(actual_eps - median_fcast_eps))
#       Min.    1st Qu.     Median       Mean    3rd Qu.       Max.
# -18.800000  -0.010000   0.010000   0.002447   0.040000   7.070000
# Getting a -1 cents to +4 cents range, similar to Bissessur Veenman 2016


# Final return merge ------------------------------------------------------

# Create return windows
window_length <- 5
multiply_rets <- function(ret, n = window_length) {
  # to get a [-1, 0, 1, 2, 3] -> 5 day return window
  zoo::rollapplyr(ret, width = n, FUN = prod, fill = NA)
}

ev_rets <-
  crsp %>%
  arrange(permno, date) %>%
  mutate(
    ret1 = ret + 1,
    MktRet1 = MktRet + 1
  ) %>%
  group_by(permno) %>%
  mutate(
    EvRet1 = multiply_rets(ret1),
    EvMktRet1 = multiply_rets(MktRet1)
  ) %>%
  ungroup() %>%
  mutate(
    AbEvRet = EvRet1 - EvMktRet1,
    ea_match_date = add.bizdays(date, -(window_length - 2), cal)
  ) %>%
  select(permno, ea_match_date, AbEvRet)

merged_data <-
  earnings_surprise %>%
  # next business day if the given date is not a business day
  mutate(ea_match_date = adjust.next(ea_date, cal)) %>%
  inner_join(ev_rets, by = c("permno", "ea_match_date"))



# Cleaning up -----------------------------------------------------------------------

final_data_trunc <-
  merged_data %>%
  filter(is.na(AbEvRet) == FALSE, is.na(earn_surp) == FALSE) %>%
  filter(
    earn_surp <= quantile(earn_surp, 0.99),
    earn_surp >= quantile(earn_surp, 0.01),
    AbEvRet <= quantile(AbEvRet, 0.99),
    AbEvRet >= quantile(AbEvRet, 0.01)
  ) %>%
  mutate(firm_id = as.integer(as.factor(ticker)))


write_parquet(final_data_trunc, "data/ea-event-returns.pqt")
