library(here)
source(here("R/R6_RawData.r"))
source(here("R/R6_Schedule.r"))

data_root <- "D:/JMDPフィールド実験"

options(
  knitr.table.format = "html",
  knitr.kable.NA = "",
  modelsummary_stars_note = FALSE
)

# //NOTE Assignment schedule
schedule <- Schedule$new(here(data_root, "RCT-schedule.csv"))
schedule$flextable(
  title = "Assignment Schedule",
  notes = paste(
    "Notes: See Table XX for detail intervention of each experimental arm.",
    "Control group is experimental arm A."
  )
)

# //NOTE Add data
rawdt <- RawData$new(
  here(data_root, "shape.csv"),
  treat_vars = "treat",
  treat_levels = LETTERS[1:4]
)

# //NOTE Condition of study sample
rawdt$add_cond_study_sample("ongoing == 0")
rawdt$balance_attrition()

# //NOTE Setup RCT
intervention <- list(
  "Standard notification" = c(A = "X", B = "X", C = "X", D = "X"),
  "Probability message" = c(A = "", B = "X", C = "", D = "X"),
  "Early Coordination message" = c(A = "", B = "", C = "X", D = "X")
)

endpoint <- list(
  reply = "Reply",
  positive = "Positive intention",
  negative = "Negative intention",
  test = "CT",
  candidate = "Candidate",
  consent = "Consent",
  donate = "Donation"
)

covs <- c(
  "Male (= 1)" = "male",
  "Age" = "age",
  "Number of past coordinations" = "coordinate",
  "Number of listed hospitals" = "hospital_per_area",
  "Number of hospitals listed with PBSC collection" = "PB_per_area",
  "Number of hospitals listed with BM collection" = "BM_per_area"
)

rct <- rawdt$RCT()
rct$add_intervention(intervention)
rct$add_outcome(endpoint)
rct$add_covariate(covs)
rct$add_fixed_effect(c("month", "week"))
rct$set_default_se_type("stata")

# //NOTE Balance test
rct$
  summary_experiment()$
  flextable(
    title = "Summary of Field Experiment",
    notes = paste(
      "Notes: For balance test, we regress a covariate on treatment dummies",
      "and test a null hypothesis that all coefficients are zero.",
      "We use the robust standard errors for statistical inference."
    )
  )

# //NOTE Stock data analysis
stock <- rct$
  add_covariate(c("Squared age" = "I(age^2)"))$
  lm(1:3, sample_drop = FALSE)

# full sample analysis: linear regression
stock_all <- stock$fit_all()

stock_all$
  flextable(
    title = "Linear Probability Model of Reply and Intention",
    notes = paste(
      "Covariates are gender, squared polynomial of age,",
      "number of past coordinations,",
      "number of hospitals per 10 square kilometers,",
      "number of hospitals with PBSC collection per 10 square kilometers,",
      "number of hospitals with BM collection per 10 square kilometers,",
      "month dummies, and week dummies."
    )
  )

stock_all$
  kable(
    title = "Linear Probability Model of Reply and Intention",
    notes = paste(
      "Covariates are gender, squared polynomial of age,",
      "number of past coordinations,",
      "number of hospitals per 10 square kilometers,",
      "number of hospitals with PBSC collection per 10 square kilometers,",
      "number of hospitals with BM collection per 10 square kilometers,",
      "month dummies, and week dummies."
    ),
    font_size = 15
  )

# full sample analysis: logit regression
stock_logit <- rct$
  add_covariate(c("Squared age" = "I(age^2)"))$
  logit(1:3, sample_drop = FALSE)$
  fit_all()

stock_logit$
  flextable(
    title = "Logit Model of Reply and Intention",
    notes = paste(
      "Covariates are gender, squared polynomial of (demeaned) age, number of past coordinations,",
      "number of hospitals per 10 square kilometers,",
      "number of hospitals with PBSC collection per 10 square kilometers,",
      "number of hospitals with BM collection per 10 square kilometers,",
      "month dummies, and week dummies."
    )
  )

stock_logit$
  kable(
    title = "Logit Model of Reply and Intention",
    notes = paste(
      "Covariates are gender, squared polynomial of (demeaned) age, number of past coordinations,",
      "number of hospitals per 10 square kilometers,",
      "number of hospitals with PBSC collection per 10 square kilometers,",
      "number of hospitals with BM collection per 10 square kilometers,",
      "month dummies, and week dummies."
    ),
    font_size = 15
  )

# subsample analysis
stock_sub <- stock$fit_sub()
stock_sub$coefplot()

# cluster se
stock_g <- rct$
  add_covariate(c("Squared age" = "I(age^2)"))$
  lm(1:3, cluster = "RCTweek")

stock_g$
  fit_all()$
  kable(font_size = 15)

stock_g$
  fit_sub()$
  coefplot()

# //NOTE Coordination data analysis
coordinate <- rct$
  add_covariate(c("Squared age" = "I(age^2)"))$
  lm(4:7, sample_drop = FALSE)

# full sample analysis
coordinate_all <- coordinate$fit_all()

coordinate_all$
  flextable(
    title = "Linear Probability Model of Coordination",
    notes = paste(
      "Covariates are gender, squared polynomial of age,",
      "number of past coordinations,",
      "number of hospitals per 10 square kilometers,",
      "number of hospitals with PBSC collection per 10 square kilometers,",
      "number of hospitals with BM collection per 10 square kilometers,",
      "month dummies, and week dummies."
    )
  )

coordinate_all$
  kable(
    title = "Linear Probability Model of Coordination",
    notes = paste(
      "Covariates are gender, squared polynomial of age,",
      "number of past coordinations,",
      "number of hospitals per 10 square kilometers,",
      "number of hospitals with PBSC collection per 10 square kilometers,",
      "number of hospitals with BM collection per 10 square kilometers,",
      "month dummies, and week dummies."
    ),
    font_size = 15
  )

# subsample analysis
coordinate_sub <- coordinate$fit_sub()
coordinate_sub$coefplot()

# //NOTE Random causal forest
rcf <- rct$rcf("test", sample_drop = FALSE)

# boxplot
rcf$subset_boxplot()

# CATE
cate <- rcf$cate(age < 30, male == 0)

cate$
  flextable(
    label = list(
      c("30 < Age", "Age < 30"),
      c("Male", "Female")
    ),
    title = "Conditional Average Treatment Effect Estimated by RCF"
  )

cate$
  kable(
    label = list(
      c("30 < Age", "Age < 30"),
      c("Male", "Female")
    ),
    title = "Conditional Average Treatment Effect Estimated by RCF"
  )

# decompose effect
mechanism <- rcf$decompose_effect(
  "D",
  c("B", "C"),
  male == 0,
  age < 30
)

mechanism$kable(
  title = "Correlation of Predicted Treatment Effects",
  subset_label = list(
    c("Male", "Female"),
    c("30 < Age", "Age < 30")
  ),
  font_size = 15
)

mechanism$flextable(
  title = "Correlation of Predicted Treatment Effects",
  subset_label = list(
    c("Male", "Female"),
    c("30 < Age", "Age < 30")
  )
)

# sample characteristics
middle_male <- rcf$effect_characteristics(
  target = "B",
  effect = 0.1,
  male = male == 1,
  age = 30 < age & age <= 40
)

middle_male$
  kable(
    group_label = c("Effect < 0.1", "0.1 < Effect"),
    font_size = 15
  )

older_male <- rcf$effect_characteristics(
  target = "C",
  male = male == 1,
  age = 45 < age
)

older_male$
  kable(font_size = 15)

older_female <- rcf$effect_characteristics(
  target = "C",
  male = male == 0,
  age = 45 < age
)

older_female$
  kable(font_size = 15)

# //NOTE Flow analysis
flow <- rct$
  add_covariate(c("Squared age" = "I(age^2)"))$
  flow(outcome = "reply")

# descriptive statistics
flow$plot(
  male == 1,
  age >= 30,
  label_list = list(
    c("Female", "Male"),
    c("Age < 30", "Age > 30")
  )
)

# regression analysis
flow_fit <- flow$fit(
  1:40,
  male = male == 1,
  old = age >= 30
)

flow_fit$
  plot(male = FALSE, old = FALSE)

flow_fit$
  plot(male = TRUE, old = FALSE)

# Flow: negative intention
f_negative <- rct$
  add_covariate(c("Squared age" = "I(age^2)"))$
  flow(outcome = "negative")

f_negative_fit <- f_negative$fit(
  1:40,
  male = male == 1,
  old = age >= 30
)

f_negative_fit$
  plot(male = FALSE, old = FALSE)

# Flow: positive intention
f_positive <- rct$
  add_covariate(c("Squared age" = "I(age^2)"))$
  flow(outcome = "positive")

f_positive_fit <- f_positive$fit(
  1:40,
  male = male == 1,
  old = age >= 30
)

f_positive_fit$
  plot(male = FALSE, old = FALSE)
