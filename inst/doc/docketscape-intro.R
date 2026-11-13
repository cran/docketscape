## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 4,
  eval = TRUE
)

## ----setup-data---------------------------------------------------------------
library(docketscape)

set.seed(123)
n_cases <- 30
cases_raw <- data.frame(
  case_id = 1:n_cases,
  court = sample(c("Court_A", "Court_B"), n_cases, replace = TRUE),
  filing_date = sample(seq.Date(as.Date("2012-01-01"), as.Date("2013-03-01"), by = "month"), n_cases, replace = TRUE),
  duration_days = round(runif(n_cases, 60, 500)),
  disposed = rbinom(n_cases, 1, 0.70),
  stringsAsFactors = FALSE
)

# Pending cases have NA for decision_date
cases_raw$decision_date <- as.Date(NA)
cases_raw$decision_date[cases_raw$disposed == 1] <- 
  cases_raw$filing_date[cases_raw$disposed == 1] + cases_raw$duration_days[cases_raw$disposed == 1]

# Simulate legal sections
set.seed(42)
acts <- c("NI Act", "IPC", "CrPC")
secs <- list("NI Act" = c("138", "141"), "IPC" = c("420", "406"), "CrPC" = c("154", "482"))

sec_rows <- lapply(1:n_cases, function(i) {
  n_s <- sample(1:2, 1)
  act_sample <- sample(acts, n_s, replace = TRUE)
  sec_sample <- sapply(act_sample, function(a) sample(secs[[a]], 1))
  data.frame(case_id = i, act = unname(act_sample), section = unname(sec_sample), stringsAsFactors = FALSE)
})
sections_raw <- unique(do.call(rbind, sec_rows))

## ----clean-cases--------------------------------------------------------------
cleaned <- clean_cases(cases_raw, observation_end = "2014-12-31")
head(cleaned[, c("case_id", "court", "duration", "event", "is_valid")])

## ----section-graph------------------------------------------------------------
sec_graph <- build_section_graph(sections_raw)
print(sec_graph)

## ----diffuse-profiles---------------------------------------------------------
diffused <- diffuse_profiles(sections_raw, sec_graph, alpha = 0.7)
print(diffused)

## ----case-graph---------------------------------------------------------------
case_graph <- build_case_graph(diffused, k = 3, use_idf = TRUE)
print(case_graph)

## ----neighbourhood-survival---------------------------------------------------
km_focal <- neighbourhood_survival(case_graph, cleaned, case_id = 1)
print(km_focal)

## ----plot-survival, fig.width = 6, fig.height = 4-----------------------------
surv_tab <- km_focal$survival_table

plot(
  surv_tab$time, surv_tab$surv,
  type = "s",
  lwd = 2,
  col = "darkblue",
  xlab = "Time (days)",
  ylab = "Estimated Survival Probability S(t)",
  main = paste("Neighbourhood Survival Curve for Case", km_focal$target_case_id),
  ylim = c(0, 1)
)
abline(h = 0.5, lty = 2, col = "grey50")
if (!is.na(km_focal$target_duration)) {
  abline(v = km_focal$target_duration, col = "firebrick", lty = 3, lwd = 2)
}
grid()
legend(
  "topright",
  legend = c("Peer Survival S(t)", "Median Threshold (50%)", "Target Case Duration"),
  col = c("darkblue", "grey50", "firebrick"),
  lty = c(1, 2, 3),
  lwd = c(2, 1, 2),
  bty = "n"
)

## ----relative-lateness--------------------------------------------------------
lateness <- relative_lateness(km_focal)
print(lateness)

## ----court-load---------------------------------------------------------------
load_df <- court_load(cleaned, by = "month")
head(load_df)

## ----fit-model----------------------------------------------------------------
fit <- fit_delay_model(cleaned, case_graph, load_df)
print(fit)

## ----decompose----------------------------------------------------------------
decomp <- decompose_delay(fit)
if (fit$fit_success) {
  head(decomp$case_decomposition, 3)
}

## ----validate-models----------------------------------------------------------
val <- validate_models(fit, sections = sections_raw, temporal_split = 0.7)
print(val)

