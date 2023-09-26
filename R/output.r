library(here)
source("R/R6_RawData.r")
source("R/R6_Schedule.r")

data_root <- "D:/JMDPフィールド実験"

options(
  knitr.table.format = "latex",
  knitr.table.NA = "",
  modelsummary_stars_note = FALSE,
  modelsummary_factory_default = "kableExtra",
  modelsummary_factory_latex = "kableExtra",
  modelsummary_factory_word = "flextable"
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
rct$set_default_se("stata")

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
rct$add_covariate(c("Squared age" = "I(age^2)"))
stock <- rct$lm(1:3)

# full sample analysis
stock_all <- stock$fit_all()

stock_all$
  msummary(title = "Linear Probability Model of Reply and Intention")$
  flextable(
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
  msummary(title = "Linear Probability Model of Reply and Intention")$
  kable(
    notes = paste(
      "Covariates are gender, squared polynomial of age,",
      "number of past coordinations,",
      "number of hospitals per 10 square kilometers,",
      "number of hospitals with PBSC collection per 10 square kilometers,",
      "number of hospitals with BM collection per 10 square kilometers,",
      "month dummies, and week dummies."
    )
  )

# subsample analysis
stock_sub <- stock$fit_sub()
stock_sub$coefplot()

# //NOTE Coordination data analysis
coordinate <- rct$lm(4:7)

# full sample analysis
coordinate_all <- coordinate$fit_all()

coordinate_all$
  msummary(title = "Linear Probability Model of Coordination")$
  flextable(
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
  msummary(title = "Linear Probability Model of Coordination")$
  kable(
    notes = paste(
      "Covariates are gender, squared polynomial of age,",
      "number of past coordinations,",
      "number of hospitals per 10 square kilometers,",
      "number of hospitals with PBSC collection per 10 square kilometers,",
      "number of hospitals with BM collection per 10 square kilometers,",
      "month dummies, and week dummies."
    )
  )

# subsample analysis
coordinate_sub <- coordinate$fit_sub()
coordinate_sub$coefplot()
