library(here)
library(R6)
library(tidyverse)
source(here("R/misc.r"))

MultipleHypothesis <- R6::R6Class("MultipleHypothesis",
  public = list(
    initialize = function(data, outcome, age_cut) {
      private$data <- data %>%
        mutate(
          young = if_else(age < age_cut, 1, 0),
          group = case_when(
            male == 0 & young == 1 ~ 1,
            male == 0 & young == 0 ~ 2,
            male == 1 & young == 1 ~ 3,
            male == 1 & young == 0 ~ 4
          ),
          group = factor(group, labels = c("Young female", "Older female", "Young male", "Older male"))
        )

      private$outcome <- outcome
    },
    get_result = function() private$result,
    estimate = function(scale = 1, seed = 120511, B = 3000) {
      private$result <- private$mht(
        private$data,
        private$outcome,
        "treat",
        "group",
        scale,
        seed,
        B
      )

      invisible(self)
    }
  ),
  private = list(
    data = NULL,
    outcome = NULL,
    result = NULL,
    mht = function(data, Y, D, G = NULL, scale, seed, B) {
      # setup
      y <- as.matrix(data[, Y])
      y <- y * scale
      ylab <- 1:length(Y)
      names(ylab) <- Y

      dcol <- data[, D, drop = TRUE]

      if (class(dcol) == "character") {
        dlab <- 1:length(unique(dcol))
        dlab <- dlab - 1
        names(dlab) <- unique(dcol)
        dcol <- sapply(dcol, function(x) dlab[names(dlab) == x])
        dcol <- unname(dcol)
      } else if (class(dcol) == "factor") {
        dlab <- 1:length(levels(dcol))
        dlab <- dlab - 1
        names(dlab) <- levels(dcol)
        dcol <- sapply(dcol, function(x) dlab[names(dlab) == x])
        dcol <- unname(dcol)
      } else {
        dlab <- NULL
      }

      d <- matrix(dcol, ncol = 1)

      if (is.null(G)) {
        g <- matrix(rep(1, nrow(data)), ncol = 1)
        glab <- 1
        names(glab) <- "Full sample"
      } else {
        gcol <- data[, G, drop = TRUE]

        if (class(gcol) == "character") {
          glab <- 1:length(unique(gcol))
          names(glab) <- unique(gcol)
          gcol <- sapply(gcol, function(x) glab[names(glab) == x])
          gcol <- unname(gcol)
        } else if (class(gcol) == "factor") {
          glab <- 1:length(levels(gcol))
          names(glab) <- levels(gcol)
          gcol <- sapply(gcol, function(x) glab[names(glab) == x])
          gcol <- unname(gcol)
        } else {
          glab <- NULL
        }

        g <- matrix(gcol, ncol = 1)
      }

      # combination of pair-wise comparison
      pc <- t(combn(sort(unique(d)), m = 2))
      pc <- pc[pc[, 1] == 0, , drop = FALSE]

      # check dimension
      num <- nrow(y)
      numy <- ncol(y)
      numg <- length(unique(g))
      numd1 <- length(unique(d)) - 1
      numpc <- nrow(pc)

      # Mean, variance, and sample size
      mu <- array(0, dim = c(numy, numg, numd1 + 1))
      v <- array(0, dim = c(numy, numg, numd1 + 1))
      n <- array(0, dim = c(numy, numg, numd1 + 1))

      for (j in 1:numg) {
        for (k in 0:numd1) {
          idx <- which(g == j & d == k)
          mu[, j, k + 1] <- apply(y[idx, , drop = FALSE], 2, mean)
          v[, j, k + 1] <- apply(y[idx, , drop = FALSE], 2, var)
          n[, j, k + 1] <- rep(length(idx), numy)
        }
      }

      # Test statistics
      pc_d0 <- pc[, 1] + 1
      pc_d1 <- pc[, 2] + 1
      diff <- mu[, , pc_d1, drop = FALSE] - mu[, , pc_d0, drop = FALSE]
      v1 <- v[, , pc_d1, drop = FALSE] / n[, , pc_d1, drop = FALSE]
      v0 <- v[, , pc_d0, drop = FALSE] / n[, , pc_d0, drop = FALSE]
      absdiff <- abs(diff)
      stats <- absdiff / sqrt(v1 + v0)

      # bootstrap
      set.seed(seed)
      idxboot <- matrix(
        sample(1:num, num * B, replace = TRUE),
        nrow = num,
        ncol = B
      )

      statsboot <- array(0, dim = c(B, numy, numg, numpc))

      for (i in 1:B) {
        yboot <- y[idxboot[, i], , drop = FALSE]
        gboot <- g[idxboot[, i], , drop = FALSE]
        dboot <- d[idxboot[, i], , drop = FALSE]

        muboot <- array(0, dim = c(numy, numg, numd1 + 1))
        vboot <- array(0, dim = c(numy, numg, numd1 + 1))
        nboot <- array(0, dim = c(numy, numg, numd1 + 1))

        for (k in 1:numg) {
          for (l in 0:numd1) {
            idx <- which(gboot == k & dboot == l)
            muboot[, k, l + 1] <- apply(yboot[idx, , drop = FALSE], 2, mean)
            vboot[, k, l + 1] <- apply(yboot[idx, , drop = FALSE], 2, var)
            nboot[, k, l + 1] <- rep(length(idx), numy)
          }
        }

        diffboot <- muboot[, , pc_d1, drop = FALSE] - muboot[, , pc_d0, drop = FALSE]
        v1boot <- vboot[, , pc_d1, drop = FALSE] / nboot[, , pc_d1, drop = FALSE]
        v0boot <- vboot[, , pc_d0, drop = FALSE] / nboot[, , pc_d0, drop = FALSE]
        statsboot[i, , , ] <- abs(diffboot - diff) / sqrt(v1boot + v0boot)
      }

      # Single hypothesis test
      p <- array(0, dim = c(numy, numg, numpc))
      prev <- array(0, dim = c(numy, numg, numpc))
      prevboot <- array(0, dim = c(B, numy, numg, numpc))

      for (i in 1:numy) {
        for (j in 1:numg) {
          for (k in 1:numpc) {
            statsboot_sub <- statsboot[, i, j, k]
            prev[i, j, k] <- mean(statsboot_sub < stats[i, j, k])
            prevboot[, i, j, k] <- sapply(
              statsboot_sub,
              function(x) mean(statsboot_sub < x)
            )
            p[i, j, k] <- 1 - prev[i, j, k]
          }
        }
      }

      # Multiple hypothesis
      nh <- numy * numg * numpc
      statsall <- matrix(0, nrow = nh, ncol = 9 + B)
      counter <- 1

      for (i in 1:numy) {
        for (j in 1:numg) {
          for (k in 1:numpc) {
            statsall[counter, ] <- c(
              counter,
              i,
              j,
              pc[k, ],
              diff[i, j, k],
              p[i, j, k],
              stats[i, j, k],
              prev[i, j, k],
              prevboot[, i, j, k]
            )
            counter <- counter + 1
          }
        }
      }

      statsall <- statsall[!is.nan(statsall[, 8]), ]
      use_nh <- nrow(statsall)
      statsrank <- statsall[order(statsall[, 7]), ]
      alphamul <- numeric(nh)

      for (i in 1:use_nh) {
        maxstats <- apply(statsrank[i:use_nh, 10:ncol(statsall), drop = FALSE], 2, max)
        alphamul[i] <- mean(statsrank[i, 9] < maxstats)
      }

      # Bonferroni correction and Holm correction
      bon <- pmin(statsrank[, 7] * nh, 1)
      holm <- pmin(statsrank[, 7] * (nh:1), 1)

      # Return results
      show <- statsrank[, 1:7]
      show <- cbind(show, alphamul, bon, holm)
      show <- show[order(show[, 1]), ]
      colnames(show) <- c(
        "id",
        "Outcome id",
        "Subgroup id",
        "Control id",
        "Treated id",
        "Diff",
        "Single p",
        "List p",
        "Bonf p",
        "Holm p"
      )

      show <- data.frame(show)

      show[, 2] <- sapply(show[, 2], function(x) names(which(ylab == x)))

      if (!is.null(glab)) {
        show[, 3] <- sapply(show[, 3], function(x) names(which(glab == x)))
      }

      if (!is.null(dlab)) {
        show[, 4] <- sapply(show[, 4], function(x) names(which(dlab == x)))
        show[, 5] <- sapply(show[, 5], function(x) names(which(dlab == x)))
      }

      return(show)
    }
  )
)