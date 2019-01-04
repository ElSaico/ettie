library(mlr)

multiclass.mcc <- makeMeasure(
  id = "multiclass.mcc",
  name = "Multiclass Matthews correlation coefficient",
  minimize = FALSE,
  properties = c("classif", "classif.multi", "req.pred", "req.truth"),
  best = 1,
  worst = -1,
  fun = function(task, model, pred, feats, extra.args) {
    yardstick::mcc(getTaskData(task), getPredictionTruth(pred), getPredictionResponse(pred))$.estimate
  }
)
