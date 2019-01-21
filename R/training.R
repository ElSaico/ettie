make.tune.learner <- function(learner.name, control, resampling, par.set, par.vals = list()) {
  mlr::makeTuneWrapper(
    mlr::makeLearner(learner.name, predict.type = "prob"), resampling = resampling,
    par.set = par.set, measures = mlr::multiclass.aunu, control = control
  )
}

#' @export
run.benchmark <- function(data, seed = 8080, parallel = FALSE, resampling = mlr::cv5) {
  set.seed(seed, "L'Ecuyer")
  mlr::configureMlr(show.info = FALSE, on.learner.error = "warn")

  input <- build.input(data)

  pred.task <- mlr::makeClassifTask("match.result", input, "result")
  pred.control <- mlr::makeTuneControlMBO()
  pred.learners <- list(
    make.tune.learner(
      "classif.boosting", pred.control, resampling,
      ParamHelpers::makeParamSet(
        ParamHelpers::makeDiscreteParam("coeflearn", c("Breiman", "Freund", "Zhu")),
        ParamHelpers::makeIntegerParam("mfinal", 1, 100, default = 100),
        ParamHelpers::makeIntegerParam("maxdepth", 1, 30, default = 30)
      )
    ),
    mlr::makeLearner("classif.cforest", predict.type = "prob"),
    mlr::makeLearner("classif.randomForestSRC", predict.type = "prob", ntree = 500),
    mlr::makeLearner("classif.RRF", predict.type = "prob"),
    make.tune.learner(
      "classif.xgboost", pred.control, resampling,
      ParamHelpers::makeParamSet(
        ParamHelpers::makeNumericParam("eta", 0.01, 0.3, default = 0.3),
        ParamHelpers::makeNumericParam("gamma", 0, 1, default = 0),
        ParamHelpers::makeIntegerParam("max_depth", 6, 10, default = 6),
        ParamHelpers::makeNumericParam("min_child_weight", 0, 7, default = 0),
        ParamHelpers::makeNumericParam("colsample_bytree", 0, 1, default = 1)
      )
    ),
    # FIXME: cForest, RRF and randomForestSRC might be redundant
    make.tune.learner(
      "classif.extraTrees", pred.control, resampling,
      ParamHelpers::makeParamSet(
        ParamHelpers::makeIntegerParam("mtry", 4, 7, default = 4),
        ParamHelpers::makeIntegerParam("numRandomCuts", 1, 5, default = 1)
      )
    ),
    make.tune.learner(
      "classif.glmnet", pred.control, resampling,
      ParamHelpers::makeParamSet(
        ParamHelpers::makeNumericParam("alpha", 0, 1, default = 1),
        ParamHelpers::makeNumericParam("s", 0, 1, default = 0.01)
      )
    ),
    mlr::makeLearner("classif.lda", predict.type = "prob"),
    make.tune.learner(
      "classif.naiveBayes", pred.control, resampling,
      ParamHelpers::makeParamSet(
        ParamHelpers::makeDiscreteParam("laplace", c(0, 1))
      )
    ),
    make.tune.learner(
      "classif.svm", pred.control, resampling,
      ParamHelpers::makeParamSet(
        ParamHelpers::makeDiscreteParam("kernel", c("linear", "polynomial", "radial", "sigmoid")),
        ParamHelpers::makeNumericParam("cost", 0.001, 100, default = 0.001),
        ParamHelpers::makeNumericParam("gamma", 0.001, 1, default = 0.001)
      )
    )
  )
  pred.measures <- list(
    mlr::kappa, mlr::logloss, mlr::acc, mlr::multiclass.aunu,
    mlr::multiclass.brier, multiclass.mcc
  )

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
