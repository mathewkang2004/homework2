# Install and load tools --------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales, data.table)


# Load functions and set year ---------------------------------------------
source("submission1/data-code/functions-loader.R")
monthlist <- sprintf("%02d", 1:12)
y <- 2017


# Enrollment & Contract data ---------------------------------------------------------
plan.data <- map_dfr(monthlist, ~ load_month(.x, y)) %>%
  arrange(contractid, planid, state, county, month) %>%
  group_by(state, county) %>%
  fill(fips, .direction = "downup") %>%                
  ungroup() %>%
  group_by(contractid, planid) %>%
  fill(plan_type, partd, snp, eghp, plan_name, .direction = "downup") %>%
  ungroup() %>%
  group_by(contractid) %>%
  fill(org_type, org_name, org_marketing_name, parent_org, .direction = "downup") %>%
  ungroup()

plan.data.dt <- as.data.table(plan.data)
setorder(plan.data.dt, contractid, planid, fips, year, month)

plan.year <- plan.data.dt[
  , {
    nonmiss <- !is.na(enrollment)
    n <- sum(nonmiss)
    list(
      n_nonmiss = n,
      avg_enrollment = if (n>0) mean(enrollment[nonmiss]) else NA_real_,
      sd_enrollment  = if (n>1) sd(enrollment[nonmiss]) else NA_real_,
      min_enrollment = if (n>0) min(enrollment[nonmiss]) else NA_real_,
      max_enrollment = if (n>0) max(enrollment[nonmiss]) else NA_real_,
      first_enrollment = if (n>0) enrollment[which(nonmiss)[1]] else NA_real_,
      last_enrollment  = if (n>0) enrollment[tail(which(nonmiss), 1)] else NA_real_,
      state  = tail(state, 1),
      county = tail(county, 1),
      org_type = tail(org_type, 1),
      plan_type = tail(plan_type, 1),
      partd = tail(partd, 1),
      snp   = tail(snp, 1),
      eghp  = tail(eghp, 1),
      org_name = tail(org_name, 1),
      org_marketing_name = tail(org_marketing_name, 1),
      plan_name = tail(plan_name, 1),
      parent_org = tail(parent_org, 1),
      contract_date = tail(contract_date, 1)
    )
  },
  by = .(contractid, planid, fips, year)
]

plan.data.2017 <- as_tibble(plan.year)


# Service area data -------------------------------------------------------
service.year <- map_dfr(monthlist, ~ load_month_sa(.x, y))

# Ensure stable order before fills
service.year <- service.year %>%
  arrange(contractid, fips, state, county, month)

# Fill missing identifiers/labels
service.year <- service.year %>%
  group_by(state, county) %>%
  fill(fips, .direction = "downup") %>%
  ungroup() %>%
  group_by(contractid) %>%
  fill(plan_type, partial, eghp, org_type, org_name, .direction = "downup") %>%
  ungroup()

# Collapse to yearly: one row per contract × county (fips) × year --------
service.data.2017 <- service.year %>%
  group_by(contractid, fips, year) %>%
  arrange(month, .by_group = TRUE) %>%
  summarize(
    state     = last(state),
    county    = last(county),
    org_name  = last(org_name),
    org_type  = last(org_type),
    plan_type = last(plan_type),
    partial   = last(partial),
    eghp      = last(eghp),
    ssa       = last(ssa),
    notes     = last(notes),
    .groups = "drop"
  )


# Landscape data ----------------------------------------------------------
# Import data
ma.path.a <- paste0("data/input/2017LandscapeSource file MA_AtoM 10182016.csv")
ma.data.a <- read_csv(ma.path.a,
                      skip=6,
                      col_names=c("state","county","org_name","plan_name","plan_type","premium","partd_deductible",
                                  "drug_type","gap_coverage","drug_type_detail","contractid",
                                  "planid","segmentid","moop","star_rating"),
                      col_types = cols(
                        state = col_character(),
                        county = col_character(),
                        org_name = col_character(),
                        plan_name = col_character(),
                        plan_type = col_character(),
                        premium = col_number(),
                        partd_deductible = col_number(),
                        drug_type = col_character(),
                        gap_coverage = col_character(),
                        drug_type_detail = col_character(),
                        contractid = col_character(),
                        planid = col_double(),
                        segmentid = col_double(),
                        moop = col_character(),
                        star_rating = col_character()
                      ))


ma.path.b <- paste0("data/input/2017LandscapeSource file MA_NtoW 10182016.csv")
ma.data.b <- read_csv(ma.path.b,
                      skip=6,
                      col_names=c("state","county","org_name","plan_name","plan_type","premium","partd_deductible",
                                  "drug_type","gap_coverage","drug_type_detail","contractid",
                                  "planid","segmentid","moop","star_rating"),
                      col_types = cols(
                        state = col_character(),
                        county = col_character(),
                        org_name = col_character(),
                        plan_name = col_character(),
                        plan_type = col_character(),
                        premium = col_number(),
                        partd_deductible = col_number(),
                        drug_type = col_character(),
                        gap_coverage = col_character(),
                        drug_type_detail = col_character(),
                        contractid = col_character(),
                        planid = col_double(),
                        segmentid = col_double(),
                        moop = col_character(),
                        star_rating = col_character()
                      ))

ma.data <- rbind(ma.data.a,ma.data.b)


mapd.path.a <- paste0("data/input/Medicare Part D 2017 Plan Report 10182016.xls")
mapd.data.a <- read_xls(mapd.path.a,
                        range="A5:Z17657",
                        sheet="Alabama to Montana",
                        col_names=c("state","county","org_name","plan_name","contractid","planid","segmentid",
                                    "org_type","plan_type","snp","snp_type","benefit_type","below_benchmark",
                                    "national_pdp","premium_partc",
                                    "premium_partd_basic","premium_partd_supp","premium_partd_total",
                                    "partd_assist_full","partd_assist_75","partd_assist_50","partd_assist_25",
                                    "partd_deductible","deductible_exclusions","increase_coverage_limit",
                                    "gap_coverage"))



mapd.path.b <- paste0("data/input/Medicare Part D 2017 Plan Report 10182016.xls")
mapd.data.b <- read_xls(mapd.path.b,
                        range="A5:Z20217",
                        sheet="Nebraska to Wyoming",
                        col_names=c("state","county","org_name","plan_name","contractid","planid","segmentid",
                                    "org_type","plan_type","snp","snp_type","benefit_type","below_benchmark",
                                    "national_pdp","premium_partc",
                                    "premium_partd_basic","premium_partd_supp","premium_partd_total",
                                    "partd_assist_full","partd_assist_75","partd_assist_50","partd_assist_25",
                                    "partd_deductible","deductible_exclusions","increase_coverage_limit",
                                    "gap_coverage"))
mapd.data <- rbind(mapd.data.a,mapd.data.b)

landscape.2017 <- mapd.clean.merge(ma.data=ma.data, mapd.data=mapd.data, y)

# Penetration data --------------------------------------------------------
ma.penetration <- map_dfr(monthlist, ~ load_month_pen(.x, y)) %>%
  arrange(state, county, month) %>%
  group_by(state, county) %>%
  fill(fips, .direction = "downup") %>%
  ungroup()

# Collapse to yearly (safe summaries; avoid NaN/Inf) ----------------------
pen.2017 <- ma.penetration %>%
  group_by(fips, state, county, year) %>%
  arrange(month, .by_group = TRUE) %>%
  summarize(
    n_elig  = sum(!is.na(eligibles)),
    n_enrol = sum(!is.na(enrolled)),
    
    avg_eligibles   = ifelse(n_elig  > 0, mean(eligibles, na.rm = TRUE), NA_real_),
    sd_eligibles    = ifelse(n_elig  > 1,  sd(eligibles,  na.rm = TRUE), NA_real_),
    min_eligibles   = ifelse(n_elig  > 0, min(eligibles,  na.rm = TRUE), NA_real_),
    max_eligibles   = ifelse(n_elig  > 0, max(eligibles,  na.rm = TRUE), NA_real_),
    first_eligibles = ifelse(n_elig  > 0, first(na.omit(eligibles)),     NA_real_),
    last_eligibles  = ifelse(n_elig  > 0,  last(na.omit(eligibles)),     NA_real_),
    
    avg_enrolled    = ifelse(n_enrol > 0, mean(enrolled,   na.rm = TRUE), NA_real_),
    sd_enrolled     = ifelse(n_enrol > 1,  sd(enrolled,    na.rm = TRUE), NA_real_),
    min_enrolled    = ifelse(n_enrol > 0, min(enrolled,    na.rm = TRUE), NA_real_),
    max_enrolled    = ifelse(n_enrol > 0, max(enrolled,    na.rm = TRUE), NA_real_),
    first_enrolled  = ifelse(n_enrol > 0, first(na.omit(enrolled)),       NA_real_),
    last_enrolled   = ifelse(n_enrol > 0,  last(na.omit(enrolled)),       NA_real_),
    
    ssa = last(ssa),
    .groups = "drop"
  )


# Risk rebate data --------------------------------------------------------
# Import data
ma.path.a <- "data/input/2017PartCPlanLevel.xlsx"
risk.rebate.a <- read_xlsx(ma.path.a,range="A4:G2813",
                           col_names=c("contractid","planid","contract_name","plan_type",
                                       "riskscore_partc","payment_partc","rebate_partc"))
ma.path.b <- "data/input/2017PartDPlans.xlsx"
risk.rebate.b <- read_xlsx(ma.path.b,range="A4:H3606",
                           col_names=c("contractid","planid","contract_name","plan_type",
                                       "directsubsidy_partd","riskscore_partd","reinsurance_partd",
                                       "costsharing_partd"))


risk.rebate.a <- risk.rebate.a %>%
  mutate(
    across(c(riskscore_partc, payment_partc, rebate_partc),
           ~ parse_number(as.character(.))),
    planid = as.numeric(planid),
    year   = 2017
  ) %>%
  select(contractid, planid, contract_name, plan_type,
         riskscore_partc, payment_partc, rebate_partc, year)    

risk.rebate.b <- risk.rebate.b %>%
  mutate(
    across(c(directsubsidy_partd, reinsurance_partd, costsharing_partd),
           ~ parse_number(as.character(.))),
    payment_partd = directsubsidy_partd + reinsurance_partd + costsharing_partd,
    planid        = as.numeric(planid)
  ) %>%
  select(contractid, planid, payment_partd,
         directsubsidy_partd, reinsurance_partd, costsharing_partd,
         riskscore_partd)  

rebate.2017 <- risk.rebate.a %>%
  left_join(risk.rebate.b, by=c("contractid","planid"))


# FFS data ----------------------------------------------------------------
# Import data
ffs.data <- read_xlsx("data/input/FFS17.xlsx",
                      skip=2,
                      col_names=c("ssa","state","county_name","parta_enroll",
                                  "parta_reimb","parta_percap","parta_reimb_unadj",
                                  "parta_percap_unadj","parta_ime","parta_dsh",
                                  "parta_gme","partb_enroll",
                                  "partb_reimb","partb_percap"), na="*")


ffscosts.2017 <- ffs.data %>%
  select(ssa,state,county_name,parta_enroll,parta_reimb,
         partb_enroll,partb_reimb) %>%
  mutate(year=2017,
         ssa=as.numeric(ssa),
         mean_risk=NA) %>%
  mutate(across(c(parta_enroll, parta_reimb, partb_enroll, partb_reimb, mean_risk),
                ~ parse_number(as.character(.))))

# Merge data --------------------------------------------------------------
ma.2017 <- plan.data.2017 %>%
  inner_join(service.data.2017 %>% select(contractid, fips),
             by = c("contractid","fips")) %>%
  filter(!state %in% c("VI","PR","MP","GU","AS",""),
         snp == "No",
         (planid < 800 | planid >= 900),
         !is.na(planid), !is.na(fips)) %>%
  left_join(pen.2017 %>% ungroup() %>% rename(state_long = state, county_long = county) %>%
              mutate(state_long=str_to_lower(state_long)) %>% 
              group_by(fips) %>% mutate(ncount=n()) %>% filter(ncount==1),
            by = c("fips"))

state.2017 <- ma.2017 %>%
  group_by(state) %>%
  summarize(state_name = last(state_long[!is.na(state_long)]), .groups = "drop")

ma.2017 <- ma.2017 %>%
  mutate(year = coalesce(as.numeric(year.x), as.numeric(year.y), 2017)) %>%
  select(-starts_with("year."))

full.2017 <- ma.2017 %>%
  left_join(state.2017, by = "state") %>%
  left_join(landscape.2017 %>% mutate(state=str_to_lower(state)), by = c("contractid","planid","state_name" = "state","county")) %>%
  left_join(rebate.2017 %>% select(-contract_name, -plan_type), by = c("contractid","planid")) %>%
  mutate(
    basic_premium = case_when(
      rebate_partc > 0 ~ 0,
      partd == "No" & !is.na(premium) & is.na(premium_partc) ~ premium,
      TRUE ~ premium_partc
    ),
    bid = case_when(
      rebate_partc == 0 & basic_premium > 0 ~ (payment_partc + basic_premium) / riskscore_partc,
      rebate_partc > 0  | basic_premium == 0 ~  payment_partc / riskscore_partc,
      TRUE ~ NA_real_
    )
  ) %>%
  left_join(ffscosts.2017 %>% select(-state) %>% filter(!is.na(ssa)), by = c("ssa","year")) %>%
  mutate(
    avg_ffscost = case_when(
      parta_enroll == 0 & partb_enroll == 0 ~ 0,
      parta_enroll == 0 & partb_enroll >  0 ~ partb_reimb / partb_enroll,
      parta_enroll >  0 & partb_enroll == 0 ~ parta_reimb / parta_enroll,
      parta_enroll >  0 & partb_enroll >  0 ~ (parta_reimb / parta_enroll) + (partb_reimb / partb_enroll),
      TRUE ~ NA_real_
    )
  )

full.2017 <- full.2017 %>%
  mutate(year = coalesce(as.numeric(year.x), as.numeric(year.y), 2017)) %>%
  select(-starts_with("year."))

# Save data ---------------------------------------------------------------
write_csv(full.2017,"data/output/data-2017.csv")

# Clear all objects ------------------------------------
gc()
