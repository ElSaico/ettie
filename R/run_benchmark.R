library(dplyr)
library(mlr)

run.benchmark <- function(matches, metrics, seed = 8080) {
  set.seed(seed, "L'Ecuyer")
  input.wl <- build.input.wl(matches, metrics)
  input.wc <- build.input.wc(matches, metrics)

  pred.tasks <- list(
    makeClassifTask("women.league", input.wl, "result"),
    makeClassifTask("world.cup", input.wc, "result"),
    makeClassifTask("women.league.normalized", normalizeFeatures(input.wl), "result"),
    makeClassifTask("world.cup.normalized", normalizeFeatures(input.wc), "result")
  )
  pred.learners <- list(
    # support feature importance OOB
    makeLearner("classif.boosting", predict.type = "prob"),
    makeLearner("classif.cforest", predict.type = "prob"),
    makeLearner("classif.gbm", predict.type = "prob"),
    makeLearner("classif.randomForestSRC", predict.type = "prob"),
    makeLearner("classif.rpart", predict.type = "prob"),
    makeLearner("classif.RRF", predict.type = "prob"),
    makeLearner("classif.xgboost", predict.type = "prob"),
    # don't support feature importance OOB
    makeLearner("classif.adaboostm1", predict.type = "prob"),
    makeLearner("classif.extraTrees", predict.type = "prob"),
    makeLearner("classif.glmnet", predict.type = "prob"),
    makeLearner("classif.lda", predict.type = "prob"),
    makeLearner("classif.naiveBayes", predict.type = "prob"),
    makeLearner("classif.nnet", predict.type = "prob"),
    makeLearner("classif.svm", predict.type = "prob")
  )
  pred.measures <- list(kappa, logloss, acc, multiclass.au1p, multiclass.au1u, multiclass.aunp, multiclass.aunu, multiclass.brier, multiclass.mcc)

  benchmark(learners = pred.learners, tasks = pred.tasks, resamplings = hout, measures = pred.measures, show.info = FALSE)
}
