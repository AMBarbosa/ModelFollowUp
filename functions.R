trainTestEval <- function(mydata, train, test, pred, thresh) {

  auc_train <- with(mydata, AUC(obs = train, pred = pred))
  auc_test <- with(mydata, AUC(obs = test, pred = pred))

  cutoff <- thresh
  classif_meas <- c("CCR", "Sensitivity", "Specificity", "TSS", "kappa")
  classif_train <- with(mydata, threshMeasures(obs = train, pred = pred, thresh = cutoff, measures = classif_meas))
  classif_test <- with(mydata, threshMeasures(obs = test, pred = pred, thresh = cutoff, measures = classif_meas))
  classif <- data.frame(train = classif_train$ThreshMeasures, test = classif_test$ThreshMeasures)
  classif

  misclassif_meas <- c("UPR", "OPR", "PPI", "PAI")
  misclass_train <- with(mydata, threshMeasures(obs = train, pred = pred, thresh = cutoff, measures = misclassif_meas))
  misclass_test <- with(mydata, threshMeasures(obs = test, pred = pred, thresh = cutoff, measures = misclassif_meas))
  misclass <- data.frame(train = misclass_train$ThreshMeasures, test = misclass_test$ThreshMeasures)
  misclass

  miller_train <- with(mydata, MillerCalib(obs = train, pred = pred))
  miller_test <- with(mydata, MillerCalib(obs = test, pred = pred))

  dsq_train <- with(mydata, Dsquared(obs = train, pred = pred, family = "binomial"))
  dsq_test <- with(mydata, Dsquared(obs = test, pred = pred, family = "binomial"))
  rsq_train <- with(mydata, RsqGLM(obs = train, pred = pred))
  rsq_test <- with(mydata, RsqGLM(obs = test, pred = pred))
  explan <- cbind(c(dsq_train, unlist(rsq_train)), c(dsq_test, unlist(rsq_test)))
  colnames(explan) <- c("train", "test")
  rownames(explan)[1] <- "Dsquared"
  explan


  par(mfrow = c(2, 2), mar = c(5.5, 4, 3, 1))

  # ROC curves and AUC:
  with(auc_train, plot(thresholds$sensitivity ~ thresholds$false.pos.rate, type = "l", lwd = 2, col = "darkgrey", xlab = "False positive rate", ylab = "True positive rate", main = "(a) ROC curves"))
  with(auc_test, lines(thresholds$sensitivity ~ thresholds$false.pos.rate, lwd = 2, col = "black"))
  text(1, 0.3, adj = 1, substitute(paste(AUC == a), list(a = round(auc_train$AUC, 2))), col = "darkgrey")
  text(1, 0.1, adj = 1, substitute(paste(AUC == a), list(a = round(auc_test$AUC, 2))), col = "black")

  # threshold-based measures barplot:
  barplot(cbind(t(classif)), beside = T, las = 2, main = "(b) Classification metrics", col = c("darkgrey", "black"), border = rep(NA, 2), ylim = c(0, 1))
  text(x = 22.5, y = 0.97, labels = round(misclass["PPI", 1], 1), col = "grey20", cex = 0.8)
  #abline(v = 15.5, lty = 2)

  # Miller calibration lines:
  plot(c(0, 2.5), c(0, 2.5), type = "n", xlab = "", ylab = "", main = "(c) Miller calibration lines")
  abline(a = miller_train$intercept, b = miller_train$slope, col = "darkgrey")
  abline(a = miller_test$intercept, b = miller_test$slope, col = "black")
  text(2.5, 0.3, adj = 1, substitute(paste(Slope == a), list(a = round(miller_train$slope, 2))), col = "darkgrey")
  text(2.5, 0.1, adj = 1, substitute(paste(Slope == a), list(a = round(miller_test$slope, 2))), col = "black")

  # explanatory power barplot:
  barplot(t(explan), beside = T, las = 2, main = "(d) Explanatory power", col = c("darkgrey", "black"), border = rep(NA, 2), ylim = c(0, 1))

}
