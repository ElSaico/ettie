#' @export
run.benchmark <- function(data, seed = 8080, tune.budget = 200, parallel = FALSE) {
  set.seed(seed, "L'Ecuyer")
  mlr::configureMlr(show.info = FALSE, on.learner.error = "warn")

  input.wl <- build.input.wl(data)
  input.wc <- build.input.wc(data)

  pred.tasks <- list(
    mlr::makeClassifTask("women.league", input.wl, "result"),
    mlr::makeClassifTask("world.cup", input.wc, "result"),
    mlr::makeClassifTask("women.league.normalized", mlr::normalizeFeatures(input.wl), "result"),
    mlr::makeClassifTask("world.cup.normalized", mlr::normalizeFeatures(input.wc), "result")
  )
  pred.tuner <- mlr::makeTuneControlIrace(budget = tune.budget)
  pred.learners <- list(
    # support feature importance OOB
    mlr::makeLearner("classif.boosting", predict.type = "prob"),
    mlr::makeLearner("classif.cforest", predict.type = "prob"),
    mlr::makeLearner("classif.gbm", predict.type = "prob"),
    mlr::makeLearner("classif.randomForestSRC", predict.type = "prob"),
    mlr::makeLearner("classif.rpart", predict.type = "prob"),
    mlr::makeLearner("classif.RRF", predict.type = "prob"),
    mlr::makeLearner("classif.xgboost", predict.type = "prob"),
    # don't support feature importance OOB
    mlr::makeLearner("classif.adaboostm1", predict.type = "prob"),
    mlr::makeLearner("classif.extraTrees", predict.type = "prob"),
    mlr::makeLearner("classif.glmnet", predict.type = "prob"),
    mlr::makeLearner("classif.lda", predict.type = "prob"),
    mlr::makeLearner("classif.naiveBayes", predict.type = "prob"),
    mlr::makeLearner("classif.nnet", predict.type = "prob"),
    mlr::makeLearner("classif.svm", predict.type = "prob")
  )
  pred.learners.tuned <- lapply(pred.learners, function(l) {
    # getDefaultParConfig sets a few data-dependent hyperparameter boundaries
    # so we use one of input.wl's tasks since it's larger on both dimensions
    par.set <- tryCatch(
      mlrHyperopt::getParConfigParSet(mlrHyperopt::getDefaultParConfig(l), pred.tasks[[1]]),
      error = function(e) NULL
    )
    if (!is.null(par.set)) {
      mlr::makeTuneWrapper(l, mlr::hout, par.set = par.set, control = pred.tuner)
    }
  }) %>% purrr::compact()
  pred.measures <- list(
    mlr::kappa, mlr::logloss, mlr::acc, mlr::multiclass.au1p, mlr::multiclass.au1u,
    mlr::multiclass.aunp, mlr::multiclass.aunu, mlr::multiclass.brier, multiclass.mcc
  )

  if (parallel) {
    reg <- batchtools::makeExperimentRegistry(file.dir = NA, seed = seed)
    # trading multicore's smaller overhead for Windows compatibility
    reg$cluster.functions <- batchtools::makeClusterFunctionsSocket()
    jobs <- mlr::batchmark(
      learners = c(pred.learners, pred.learners.tuned), tasks = pred.tasks,
      resamplings = mlr::hout, measures = pred.measures
    )
    batchtools::submitJobs(jobs)
    batchtools::waitForJobs(jobs)
    mlr::reduceBatchmarkResults(jobs)
  } else {
    mlr::benchmark(
      learners = c(pred.learners, pred.learners.tuned), tasks = pred.tasks,
      resamplings = mlr::hout, measures = pred.measures
    )
  }
}
