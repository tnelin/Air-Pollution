# Analytic Dataset
analytic <- read.csv("Data/analytic_20221003.csv", stringsAsFactors = F) %>%
    mutate(hospital_discharge_date = as.Date(hospital_discharge_date),
           year_dcg = year(as.Date(hospital_discharge_date)),
           flag_date = as.Date(flag_date),
           year_after_discharge = hospital_discharge_date %m+% years(1),
           time_to_readmit = ifelse(readm_flag == 1, flag_date - hospital_discharge_date, 365),
           bpd = factor(recode(bpd, "Mild", "Moderate", "Severe")),
           trach = factor(recode(trach + 1, "No", "Yes")),
           race_NY = relevel(factor(race_NY), ref = "White"),
           BGGEOID = as.character(BGGEOID)) %>%
    dplyr::select(-insurance)

## Read in and use corrected insurance status
ed <- read.csv("Q:/Tim_SVI/Data/Raw_Data/neo-clean-air--ed_hist.csv", stringsAsFactors = F)
ip <- read.csv("Q:/Tim_SVI/Data/Raw_Data/neo-clean-air--ip_hist.csv", stringsAsFactors = F)
edip_insur <- ed %>%
    filter(visit_key %in% analytic$visit_key) %>%
    dplyr::select(pat_key, payor_group) %>%
    bind_rows(ip %>%
                filter(visit_key %in% analytic$visit_key) %>%
                dplyr::select(pat_key, payor_group)) %>%
    distinct() %>%
    mutate(insurance = factor(ifelse(payor_group == "COMMERCIAL", "Private", "Non-Private"), levels = c("Private","Non-Private"))) %>%
    dplyr::select(-payor_group)

analytic <- analytic %>%
    left_join(edip_insur, by = c("pat_key"))

## Filter to metro Philly
analytic <- analytic %>% filter(metrophl == 1)

## Define multinomial discharge variable
analytic$readm_multi <- "None"
analytic$readm_ed <- 0
analytic$readm_ip <- 0
for(i in 1:nrow(analytic)){if(analytic$ed_flag[i] == 1)
{analytic$readm_multi[i] <- "ED" analytic$readm_ed[i] <- 1}
if(analytic$ip_flag[i] == 1){analytic$readm_multi[i] <- "IP" analytic$readm_ed[i] <- 0 analytic$readm_ip[i] <- 1}}
analytic$readm_multi <- factor(analytic$readm_multi, levels = c("None","ED","IP"))
table(analytic$readm_multi)

## Read in Brokamp deprivation index at census tract and attach to analytic data
dep_index <- read.csv("\\\\chop.edu/researchprd/CPHD/5. Data Support/Geographic Data/Nancy/Cleaned Data/ct_dep_index.csv") %>%
    filter(year == 2015 & !is.na(dep_index)) %>%
    mutate(dep_index_scaled = dep_index / sd(dep_index))
analytic <- analytic %>%
    left_join(dep_index %>% dplyr::select(geoid, dep_index, dep_index_scaled), by = c("GEOID" = "geoid"))

# PM2.5/Temp Data
ap <- read.csv("Data/pm_temp_1yr_linked_20221111.csv")

## Filter to metro Philly
ap <- ap %>% filter(pat_key %in% analytic$pat_key)

## define sin and cos time
ap <- ap %>% 
    mutate(temp_delta_K = temp_max_K - temp_mean_K,
           v = 2 * (yday(date) - 1)/((365 + leap_year(year(date))) - 1)) %>%
    mutate(time.sin = sinpi(v),
           time.cos = cospi(v)) %>%
    dplyr::select(!c(nrow, X, v))

## define lagged PM and temperature, heatwave and cold snap
setDT(ap)[, c("pm_lag_0_1", "pm_lag_0_6", "pm_lag_0_30", "pm_lag_0_365", "temp_mean_lag_0_1", "temp_mean_lag_0_6", "temp_mean_lag_0_30", "temp_mean_lag_0_365") := 
                list(Reduce(`+`, shift(pm_ug.m3, 0:1))/2,
                     Reduce(`+`, shift(pm_ug.m3, 0:6))/7,
                     Reduce(`+`, shift(pm_ug.m3, 0:30))/31,
                     Reduce(`+`, shift(pm_ug.m3, 0:365))/366,
                     Reduce(`+`, shift(temp_mean_K, 0:1))/2,
                     Reduce(`+`, shift(temp_mean_K, 0:6))/7,
                     Reduce(`+`, shift(temp_mean_K, 0:30))/31,
                     Reduce(`+`, shift(temp_mean_K, 0:365))/366)
                     , by = pat_key]

ap <- ap %>%
    group_by(pat_key) %>%
    mutate(temp_max_K_lag1 = lag(temp_max_K, n=1, order_by=pat_key),
           temp_min_K_lag1 = lag(temp_min_K, n=1, order_by=pat_key)) %>%
    mutate(heatwave = ifelse(temp_max_K > 308.15 & temp_max_K_lag1 > 308.15, 1, 0),
           coldsnap = ifelse(temp_min_K < 266.483 & temp_min_K_lag1 < 266.483, 1, 0),
           pm_cummean = cummean(pm_ug.m3),
           temp_cummean = cummean(temp_mean_K))

## Create flag variables for whether a date is before, during, or after a readmission
ap <- ap %>%
    # left_join(analytic %>% dplyr::select(pat_key, flag_date, readm_multi), by = "pat_key") %>%
    left_join(analytic, by = "pat_key") %>%
    mutate(date = date(date)) %>%
    mutate(month = month(date),
           readmission = ifelse(is.na(flag_date), 0, ifelse(flag_date == date, 1, 0)),
           readmission_ip = ifelse(is.na(flag_date), 0, ifelse(flag_date == date & readm_multi == "IP", 1, 0)),
           readmission_ed = ifelse(is.na(flag_date), 0, ifelse(flag_date == date & readm_multi == "ED", 1, 0)),
           readmit_month = month(flag_date),
           readmit_dow = wday(flag_date),
           before_readmission = ifelse(is.na(flag_date), NA, ifelse(date < flag_date, 1, 0)),
           after_readmission = ifelse(is.na(flag_date), NA, ifelse(date > flag_date, 1, 0))) %>%
    mutate(week_before_readmission = ifelse(before_readmission == 1 & flag_date - date <= 7, 1, 0)) %>%
    mutate(season = case_when(month %in% c(1,2,12) ~ "winter",
                              month %in% 3:5 ~ "spring",
                              month %in% 6:8 ~ "summer",
                              month %in% 9:11 ~ "fall",
                              TRUE ~ as.character(month)) %>% 
                    factor(levels = c("summer","fall","winter","spring")))

## Summary of PM2.5/Temperature data by patient for year after discharge
pat_sum <- ap %>% 
            group_by(pat_key) %>%
            filter(hospital_discharge_date <= date & date <= year_after_discharge) %>%
            summarize(avg_mean_temp = mean(temp_mean_K),
                      avg_max_temp = mean(temp_max_K),
                      avg_min_temp = mean(temp_min_K),
                      avg_pm_ug.m3 = mean(pm_ug.m3),
                      any_heatwave = any(heatwave == 1),
                      any_coldsnap = any(coldsnap == 1),
                      any_heatwave_br = any(heatwave == 1 & before_readmission == 1),
                      any_coldsnap_br = any(coldsnap == 1 & before_readmission == 1),
                      avg_mean_temp_wbr = mean(temp_mean_K[week_before_readmission == 1])) %>%
            ungroup() %>%
            mutate(avg_pm_ug.m3_centered = avg_pm_ug.m3 - mean(avg_pm_ug.m3))

analytic <- analytic %>% left_join(pat_sum, by = "pat_key")

## Filter to the month of an exposure and only keep observations from the same day of the week
ap_ccm <- ap %>%
            mutate(day_of_week = wday(as_date(date)),
                   month = month(date)) %>%
            filter(readmission == 1 | (day_of_week == readmit_dow & month == readmit_month))

ap_ccm_ip <- ap_ccm %>% filter(readmission_ip == 1 | (day_of_week == readmit_dow & month == readmit_month & readm_multi == "IP"))
ap_ccm_ed <- ap_ccm %>% filter(readmission_ed == 1 | (day_of_week == readmit_dow & month == readmit_month & readm_multi == "ED"))

## Keep rows that occur between hospital discharge and either readmission or a year after discharge
ap_cph <- ap %>%
    filter(hospital_discharge_date <= date & (date <= min(year_after_discharge, flag_date, na.rm = TRUE))) %>%
    mutate(time1 = as.numeric(date - hospital_discharge_date) - 1,
           time2 = as.numeric(date - hospital_discharge_date),
           age_days = as.numeric(dcg_age_days + (date - hospital_discharge_date))) %>%
    filter(time1 >= 0) # removes the day of discharge row

# Readmission modeling
logit_mod2 <- glm(readm_flag ~ avg_pm_ug.m3 + bpd + avg_pm_ug.m3:bpd + sex + race_NY + dcg_age_days + insurance + dep_index_scaled + year_dcg, data = analytic, family = binomial())
summary(logit_mod2)

# IP Readmission with PM x BPD interaction
logit_mod3c <- glm(readm_ip ~ avg_pm_ug.m3 + bpd + avg_pm_ug.m3:bpd + sex + race_NY + dcg_age_days + insurance + dep_index_scaled + year_dcg, data = analytic, family = binomial())
summary(logit_mod3c)

# ER Readmission with PM x BPD interaction
logit_mod3a <- glm(readm_ed ~ avg_pm_ug.m3 + bpd + avg_pm_ug.m3:bpd + sex + race_NY + dcg_age_days + insurance + dep_index_scaled + year_dcg, data = analytic, family = binomial())
summary(logit_mod3a)


