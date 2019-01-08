#' @export
run.benchmark <- function(data, seed = 8080) {
  set.seed(seed, "L'Ecuyer")
  input.wl <- build.input.wl(data)
  input.wc <- build.input.wc(data)

  pred.tasks <- list(
    mlr::makeClassifTask("women.league", input.wl, "result"),
    mlr::makeClassifTask("world.cup", input.wc, "result"),
    mlr::makeClassifTask("women.league.normalized", mlr::normalizeFeatures(input.wl), "result"),
    mlr::makeClassifTask("world.cup.normalized", mlr::normalizeFeatures(input.wc), "result")
  )
  pred.tuner <- mlr::makeTuneControlRandom()
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
    par.set <- tryCatch(mlrHyperopt::getParConfigParSet(mlrHyperopt::getDefaultParConfig(l)),
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

  mlr::benchmark(
    learners = c(pred.learners, pred.learners.tuned), tasks = pred.tasks,
    resamplings = mlr::hout, measures = pred.measures, show.info = FALSE
  )
}
