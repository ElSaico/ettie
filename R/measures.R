multiclass.mcc <- mlr::makeMeasure(
  id = "multiclass.mcc",
  name = "Multiclass Matthews correlation coefficient",
  minimize = FALSE,
  properties = c("classif", "classif.multi", "req.pred", "req.truth"),
  best = 1,
  worst = -1,
  fun = function(task, model, pred, feats, extra.args) {
    yardstick::mcc(mlr::getTaskData(task), mlr::getPredictionTruth(pred), mlr::getPredictionResponse(pred))$.estimate
  }
)

rps <- mlr::makeMeasure(
  id = "rps",
  name = "Ranked probability score",
  minimize = TRUE,
  properties = c("classif", "classif.multi", "req.pred", "req.truth"),
  best = 0,
  worst = 1,
  fun = function(task, model, pred, feats, extra.args) {
    verification::rps(mlr::getPredictionTruth(pred), as.matrix(mlr::getPredictionProbabilities(pred)))$rps
  }
)
