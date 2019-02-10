make.tune.learner <- function(learner.name, id, predict.type, par.set, par.vals = list()) {
  mlr::makeTuneWrapper(
    mlr::makeLearner(learner.name, id, predict.type = predict.type), resampling = mlr::cv3,
    measures = mlr::multiclass.brier, par.set = par.set, control = pred.control
  )
}

mlr::configureMlr(show.info = FALSE, on.learner.error = "warn")
pred.control <- mlr::makeTuneControlMBO()
pred.learners <- list(
  mlr::makeLearner("classif.randomForestSRC", "rf", predict.type = "prob", ntree = 500),
  mlr::makeLearner("classif.cforest", "rf.cit", predict.type = "prob"),
  mlr::makeLearner("classif.RRF", "rf.regularized", predict.type = "prob"),
  make.tune.learner(
    "classif.extraTrees", "rf.extra", "prob",
    ParamHelpers::makeParamSet(
      ParamHelpers::makeIntegerParam("mtry", 4, 7, default = 4),
      ParamHelpers::makeIntegerParam("numRandomCuts", 1, 5, default = 1)
    )
  ),
  make.tune.learner(
    "classif.boosting", "adaboost.m1", "prob",
    ParamHelpers::makeParamSet(
      ParamHelpers::makeDiscreteParam("coeflearn", c("Breiman", "Freund")),
      ParamHelpers::makeIntegerParam("mfinal", 1, 100, default = 100),
      ParamHelpers::makeIntegerParam("maxdepth", 1, 30, default = 30)
    )
  ),
  make.tune.learner(
    "classif.boosting", "adaboost.samme", "prob",
    ParamHelpers::makeParamSet(
      ParamHelpers::makeIntegerParam("mfinal", 1, 100, default = 100),
      ParamHelpers::makeIntegerParam("maxdepth", 1, 30, default = 30)
    ),
    list(coeflearn = "Zhu")
  ),
  make.tune.learner(
    "classif.glmnet", "glm", "prob",
    ParamHelpers::makeParamSet(
      ParamHelpers::makeNumericParam("alpha", 0, 1, default = 1),
      ParamHelpers::makeNumericParam("s", 0, 1, default = 0.01)
    )
  ),
  make.tune.learner(
    "classif.naiveBayes", "naive.bayes", "prob",
    ParamHelpers::makeParamSet(
      ParamHelpers::makeDiscreteParam("laplace", c(0, 1))
    )
  ),
  make.tune.learner(
    "classif.svm", "svm", "prob",
    ParamHelpers::makeParamSet(
      ParamHelpers::makeDiscreteParam("kernel", c("linear", "polynomial", "radial", "sigmoid")),
      ParamHelpers::makeNumericParam("cost", 0.001, 100, default = 0.001),
      ParamHelpers::makeNumericParam("gamma", 0.001, 1, default = 0.001)
    )
  ),
  make.tune.learner(
    "classif.xgboost", "xgboost", "prob",
    ParamHelpers::makeParamSet(
      ParamHelpers::makeNumericParam("eta", 0.05, 0.3, default = 0.3),
      ParamHelpers::makeNumericParam("gamma", 0, 1, default = 0),
      ParamHelpers::makeIntegerParam("max_depth", 3, 10, default = 6),
      ParamHelpers::makeNumericParam("min_child_weight", 0, 7, default = 1),
      ParamHelpers::makeNumericParam("colsample_bytree", 0.5, 1, default = 1)
    )
  )
)
pred.measures <- list(mlr::multiclass.brier, rps, mlr::logloss, mlr::multiclass.aunu, mlr::acc, multiclass.mcc)

#' @export
run.classification <- function(input, seed = 8080, parallel = FALSE) {
  set.seed(seed, "L'Ecuyer")
  mlr::configureMlr(show.info = FALSE, on.learner.error = "warn")

  pred.task <- mlr::makeClassifTask("match.result", input, "result")

  if (parallel) {
    reg <- batchtools::makeExperimentRegistry(file.dir = NA, seed = seed)
    # trading multicore's smaller overhead for Windows compatibility
    reg$cluster.functions <- batchtools::makeClusterFunctionsSocket()
    jobs <- mlr::batchmark(learners = pred.learners, tasks = pred.task, resamplings = mlr::cv5, measures = pred.measures)
    batchtools::submitJobs(jobs)
    batchtools::waitForJobs(jobs)
    mlr::reduceBatchmarkResults(jobs)
  } else {
    mlr::benchmark(learners = pred.learners, tasks = pred.task, resamplings = mlr::cv5, measures = pred.measures)
  }
}

#' @export
run.regression <- function(input, seed = 8080, parallel = FALSE, resampling = mlr::cv5) {
  set.seed(seed, "L'Ecuyer")
  mlr::configureMlr(show.info = FALSE, on.learner.error = "warn")

  pred.task <- mlr::makeRegrTask("match.result", input, "goal.difference")
  pred.control <- mlr::makeTuneControlMBO()
  pred.learners <- list(
    # lda, boosting, naiveBayes -> bartMachine, evtree, mars
    # FIXME: cForest, RRF and randomForestSRC might be redundant
    "regr.cforest",
    "regr.randomForestSRC",
    "regr.RRF",
    make.tune.learner(
      "regr.extraTrees", pred.control, resampling,
      ParamHelpers::makeParamSet(
        ParamHelpers::makeIntegerParam("mtry", 4, 7, default = 4),
        ParamHelpers::makeIntegerParam("numRandomCuts", 1, 5, default = 1)
      )
    ),
    make.tune.learner(
      "regr.glmnet", pred.control, resampling,
      ParamHelpers::makeParamSet(
        ParamHelpers::makeNumericParam("alpha", 0, 1, default = 1),
        ParamHelpers::makeNumericParam("s", 0, 1, default = 0.01)
      )
    ),
    make.tune.learner(
      "regr.svm", pred.control, resampling,
      ParamHelpers::makeParamSet(
        ParamHelpers::makeDiscreteParam("kernel", c("linear", "polynomial", "radial", "sigmoid")),
        ParamHelpers::makeNumericParam("cost", 0.001, 100, default = 0.001),
        ParamHelpers::makeNumericParam("gamma", 0.001, 1, default = 0.001)
      )
    ),
    make.tune.learner(
      "regr.xgboost", pred.control, resampling,
      ParamHelpers::makeParamSet(
        ParamHelpers::makeNumericParam("eta", 0.01, 0.3, default = 0.3),
        ParamHelpers::makeNumericParam("gamma", 0, 1, default = 0),
        ParamHelpers::makeIntegerParam("max_depth", 6, 10, default = 6),
        ParamHelpers::makeNumericParam("min_child_weight", 0, 7, default = 0),
        ParamHelpers::makeNumericParam("colsample_bytree", 0, 1, default = 1)
      )
    )
  )
  pred.measures <- list(mlr::rmse, mlr::spearmanrho)

  if (parallel) {
    reg <- batchtools::makeExperimentRegistry(file.dir = NA, seed = seed)
    # trading multicore's smaller overhead for Windows compatibility
    reg$cluster.functions <- batchtools::makeClusterFunctionsSocket()
    jobs <- mlr::batchmark(learners = pred.learners, tasks = pred.task, resamplings = resampling, measures = pred.measures)
    batchtools::submitJobs(jobs)
    batchtools::waitForJobs(jobs)
    mlr::reduceBatchmarkResults(jobs)
  } else {
    mlr::benchmark(learners = pred.learners, tasks = pred.task, resamplings = resampling, measures = pred.measures)
  }
}
