library(mlr)

run.benchmark <- function(matches, metrics, seed = 8080) {
  set.seed(seed, "L'Ecuyer")
  input.by.match <- build.input(matches, metrics)
  input.by.match.and.team <- build.input(matches, metrics, TRUE)

  pred.tasks <- list(
    makeClassifTask("match", input.by.match, "result"),
    makeClassifTask("match.and.team", input.by.match.and.team, "result"),
    makeClassifTask("match.normalized", normalizeFeatures(input.by.match), "result"),
    makeClassifTask("match.and.team.normalized", normalizeFeatures(input.by.match.and.team), "result")
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
