library(here)
library(Rcpp)
library(rlang)
sourceCpp(here("cpp", "wildBS.cpp"))

wildBS <- function(model, data, subset = NULL, cluster, H0, B) {
  # create dataset
  args <- enquos(cluster = cluster, subset = subset)
  args <- lapply(args, eval_tidy, data)
  args$data <- data
  args$na.action <- na.omit
  args$formula <- model

  estdt <- do.call("model.frame", args)
  X <- model.matrix(model, estdt)
  Y <- estdt[, 1]
  G <- as.integer(estdt[, "(cluster)"])

  # diagnostic hypothesis
  H0 <- gsub(" ", "", H0)
  split <- strsplit(H0, "=")[[1]]
  pos <- as.integer(seq_len(ncol(X))[colnames(X) == split[1]])
  rhs <- as.numeric(split[2])

  # true wald
  wald <- ClusterOLS_Wald(X, Y, G, pos, rhs)

  # crate restricted dataset
  resY <- Y - X[, pos] * rhs
  resX <- X[, -pos]

  # wild bootstrap
  boot_wald <- do_wildBS(resX, resY, X, G, pos, rhs, B = B)

  # return p-value
  mean(abs(boot_wald) > abs(wald))
}

# example
if (FALSE) {
  wildBS(
    mod,
    use,
    subset = outcome == "Positive intention" & male == 1 & age < 30,
    cluster = RCTweek,
    H0 = "treatB = 0",
    B = 2000
  )
}
