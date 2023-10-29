library(here)
library(R6)
library(tidyverse)
source(here("R/misc.r"))

Logit <- R6::R6Class("Logit",
  public = list(
    data = NULL,
    initialize = function(data, covariate, fe) {
      ctrl <- levels(data$treat)[1]

      rhs <- list(
        unctrl = "treat",
        ctrl = c("treat", covariate)
      )

      if (!is.null(fe)) {
        rhs$ctrl <- append(
          rhs$ctrl,
          sapply(fe, function(x) paste0("factor(", x, ")"))
        )
      }

      private$model <- lapply(rhs, function(m) reformulate(m, "value"))
      private$ctrl_arm <- levels(data$treat)[1]
      self$data <- data

      cat("\n")
      cat("Options for binary regression (Logit)\n")
      cat("- Control arm:", private$ctrl_arm, "\n")
      cat("Regression models\n")
      for (i in 1:length(private$model)) print(private$model[[i]])
      cat("\n")
    },
    fit_all = function() {
      est <- self$data %>%
        group_by(outcome) %>%
        nest() %>%
        mutate(
          fit1 = map(data, ~ glm(private$model$unctrl, data = ., family = binomial())),
          fit2 = map(data, ~ glm(private$model$ctrl, data = ., family = binomial()))
        ) %>%
        ungroup() %>%
        pivot_longer(
          fit1:fit2,
          names_prefix = "fit",
          names_to = "model",
          values_to = "fit"
        ) %>%
        select(-data) %>%
        rename(covs = model) %>%
        mutate(covs = if_else(covs == "2", "X", ""))

      LogitAll$new(est)
    }
  ),
  private = list(
    ctrl_arm = "",
    model = list()
  )
)

LogitAll <- R6::R6Class("LogitAll",
  public = list(
    initialize = function(est) private$est <- est,
    get_est = function() private$est,
    print_msummary = function() private$reg_tab,
    flextable = function(title = "", notes = "", font_size = 9, ...) {
      private$msummary("flextable", title = title, ...)

      label <- private$label_structure(private$est)
      rle1 <- label$rle1
      rle2 <- label$rle2

      flex <- private$reg_tab %>%
        add_header_row(values = rle1$values, colwidths = rle1$lengths)

      if (!is.null(rle2)) {
        flex <- flex %>%
          add_header_row(values = rle2$values, colwidths = rle2$lengths)
      }

      flex %>%
        align(j = -1, align = "center", part = "all") %>%
        add_footer_lines(paste(
          "Notes: We show odds ratios and associated 95 percent confidential intervals",
          "in square brackets.",
          notes
        )) %>%
        width(j = 1, 1) %>%
        fontsize(size = font_size, part = "all") %>%
        ft_theme()
    },
    kable = function(title = "", notes = "", font_size = 9, hold = FALSE, ...) {
      private$msummary("kableExtra", title = title, ...)
      tbl <- private$reg_tab

      if (hold) {
        tbl <- tbl %>% kableExtra::kable_styling(font_size = font_size, latex_options = "HOLD_position")
      } else {
        tbl <- tbl %>% kableExtra::kable_styling(font_size = font_size)
      }

      label <- private$label_structure(private$est)
      rle1 <- label$rle1
      lab1 <- rle1$lengths
      names(lab1) <- rle1$values

      tbl <- tbl %>%
        kableExtra::add_header_above(lab1)

      if (!is.null(label$rle2)) {
        rle2 <- label$rle2
        lab2 <- rle2$lengths
        names(lab2) <- rle2$values

        tbl <- tbl %>%
          kableExtra::add_header_above(lab2)
      }

      tbl %>%
        kableExtra::footnote(
          general_title = "",
          general = paste(
            "Notes: We show odds ratios and associated 95 percent confidential intervals",
            "in square brackets.",
            notes
          ),
          threeparttable = TRUE,
          escape = FALSE
        )
    }
  ),
  private = list(
    est = NULL,
    reg_tab = NULL,
    label_structure = function(est) {
      label <- c(" ", as.character(est$outcome))
      intention_label <- str_detect(label, "intention")

      if (any(intention_label)) {
        label2 <- ifelse(intention_label, "Intention", " ")
        rle2 <- rle(label2)
      } else {
        label2 <- rle2 <- NULL
      }

      label1 <- str_remove(label, " intention")
      rle1 <- rle(label1)

      return(list(rle1 = rle1, rle2 = rle2))
    },
    msummary = function(output, ...) {
      fit <- private$est %>% pull(fit)
      estimate <- "{or}"
      statistic <- "[{lower.or}, {upper.or}]"
      coef_map <- c(
        "treatB" = "Treatment B",
        "treatC" = "Treatment C",
        "treatD" = "Treatment D"
      )
      stars <- c("***" = .01, "**" = .05, "*" = .1)
      gof_omit <- "R2|AIC|BIC|RMSE|Std|FE|se_type"
      align <- paste(c("l", rep("c", nrow(private$est))), collapse = "")
      add_tab <- data.frame(rbind(c("Covariates", private$est$covs)))
      attr(add_tab, "position") <- 7

      args <- list(
        models = fit,
        output = output,
        estimate = estimate,
        statistic = statistic,
        coef_map = coef_map,
        stars = stars,
        gof_omit = gof_omit,
        add_rows = add_tab,
        align = align
      )

      if (!missing(...)) args <- append(args, list(...))
      private$reg_tab <- do.call("modelsummary", args)

      invisible(self)
    }
  )
)