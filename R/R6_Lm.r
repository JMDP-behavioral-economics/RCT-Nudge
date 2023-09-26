library(here)
library(R6)
library(tidyverse)
library(estimatr)
library(modelsummary)
source(here("R/misc.r"))

Lm <- R6::R6Class("Lm",
  public = list(
    data = NULL,
    initialize = function(data, covariate, se, fe = NULL) {
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
      private$se_type <- se

      cat("Options for linear regression\n")
      cat("- Control arm:", private$ctrl_arm, "\n")
      cat("- Clustered standard error: FALSE\n")
      cat("  - Standard error type:", private$se_type, "\n")
      cat("Regression models\n")
      for (i in 1:length(private$model)) print(private$model[[i]])
    },
    fit_all = function() {
      est <- self$data %>%
        group_by(outcome) %>%
        nest() %>%
        mutate(
          fit1 = private$call_lm(data, private$model$unctrl, private$se_type),
          fit2 = private$call_lm(data, private$model$ctrl, private$se_type),
          avg = map_chr(
            data,
            ~ with(
              subset(., treat == private$ctrl_arm),
              sprintf("%1.4f", mean(value))
            )
          )
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

      LmAll$new(est)
    }
  ),
  private = list(
    ctrl_arm = "",
    model = list(),
    se_type = "",
    call_lm = function(data, model, se) {
      map(data, ~ lm_robust(model, data = ., se_type = se))
    }
  )
)

LmAll <- R6::R6Class("LmAll",
  public = list(
    initialize = function(est) private$est <- est,
    get_est = function() private$est,
    print_msummary = function() private$reg_tab,
    msummary = function(title = "") {
      add_tab <- data.frame(
        rbind(
          c("Control average", private$est$avg),
          c("Covariates", private$est$covs)
        )
      )

      attr(add_tab, "position") <- 7:8

      private$reg_tab <- private$est %>%
        pull(fit) %>%
        modelsummary(
          title = title,
          coef_map = c(
            "treatB" = "Treatment B",
            "treatC" = "Treatment C",
            "treatD" = "Treatment D"
          ),
          stars = c("***" = .01, "**" = .05, "*" = .1),
          fmt = 4,
          gof_omit = "R2|AIC|BIC|Log|Std|FE|se_type",
          add_rows = add_tab
        )
      
      invisible(self)
    },
    flextable = function(notes = "", font_size = 9) {
      label <- c("", as.character(private$est$outcome))
      intention_label <- str_detect(label, "intention")

      if (any(intention_label)) {
        label2 <- ifelse(intention_label, "Intention", "")
        rle2 <- rle(label2)
      } else {
        label2 <- NULL
      }

      label1 <- str_remove(label, " intention")
      rle1 <- rle(label1)

      flex <- private$reg_tab %>%
        add_header_row(values = rle1$values, colwidths = rle1$lengths)
      
      if (!is.null(label2)) {
        flex <- flex %>%
          add_header_row(values = rle2$values, colwidths = rle2$lengths)
      }

      flex %>%
        align(j = -1, align = "center", part = "all") %>%
        add_footer_lines(paste(
          "Notes: * p < 0.1, ** p < 0.05, *** p < 0.01.",
          "The robust standard errors are in parentheses.",
          notes
        )) %>%
        width(j = 1, 1) %>%
        fontsize(size = font_size, part = "all") %>%
        ft_theme()
    }
  ),
  private = list(
    est = NULL,
    reg_tab = NULL
  )
)