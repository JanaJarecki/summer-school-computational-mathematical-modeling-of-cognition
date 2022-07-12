library(data.table)
setwd(dir=dirname(rstudioapi::getActiveDocumentContext()$path))
set.seed(42)
N <- 40
cond <- 1:2
d <- data.table(
  id = 1:N,
  condition = rep(cond, each=N/2)
)
d[, judgment := rnorm(.N, cond, 3)]
d[, certainty := 0.2 + 0.2 * judgment + 1.2 * condition + rnorm(.N, 0, 1) ]
d[, choice := rbinom(.N, 1, prob=1/condition-.2)]

d[, model_X_pred := runif(.N,.1,.5) * 0^(choice) + runif(.N,.5,.9) * 0^(1-choice)]
d[, model_Y_pred := runif(.N,.4,.7) * 0^(choice) + runif(.N,.1,.7) * 0^(1-choice)]

d[, model_X_evidence := rbeta(N, 1, 5)]
d[, model_Y_evidence := 1 - model_X_evidence]
d[, alpha := rnorm(.N, 0.4, .1)]
d[, delta := runif(.N, 0, 5)]


d[, condition := factor(condition, labels = c("A","B"))]

fwrite(d, "ggplot-data.csv")
