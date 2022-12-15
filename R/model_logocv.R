#' Leave-one-group-out cross-validation
#'
#' @description model_logocv performs leave-one-group-out cross-validation on a linear mixed-effects model (class lmerMod or lmerTest) or hierarchical generalized additive model (class gam) model with a single random grouping factor.
#'
#' @usage
#' model_logocv(model, data, group, control = NULL)
#'
#' @param model Model object for cross-validation. Supported classes are "lmerMod" (from lme4:: lmer), "lmerTest" (from lmerTest::lmer) and "gam" (from mgcv::gam)
#' @param data Data frame or tibble containing data used for model calibration. None of the variables used in 'model' can contain NAs.
#' @param group Name of variable in data used as a random grouping factor in model.
#' @param control Optional settings to prevent convergence issues with lmer models.
#' Default = NULL.
#'
#' @details
#' Leave-one-group-out cross validation is a re-sampling procedure than can be used to evaluate a model’s predictive performance for new levels of a random grouping factor. If that grouping factor is Site, then the function evaluates the model’s ability to predict the response at new sites not in the calibration dataset.
#'
#' One group (site) is omitted (to form a separate test set), the model is fitted to the data for the remaining groups (the training set), and then the re-fitted model is then used to predict the response variable for the test set. (Note that these predictions use only the fixed terms in the model; random effects are excluded when predicting for new sites). This is process is repeated for each group so that a prediction is generated for every observation in the full dataset. The overall performance of the model is measured by the root mean square error (RMSE), which quantifies the difference between the observed and predicted values. Comparing RMSE values for competing models can be used to guide model selection.
#'
#' When refitting the model during cross-validation, all arguments to lmer() and gam() take default values, with the exception of (i) 'REML' (which inherits from the original lmer model object), and (ii) 'control' (which can be set via the 'control' argument for lmer models only).
#'
#' Not recommended for use on more complex models with multiple (crossed or nested) random grouping factors.

#' @return A list of two elements: (i) the RMSE, and (ii) a data frame or tibble containing the input dataset plus an additional column (pred_cv) containing the cross-validation predicted values.
#'
#' @export
#'
#' @examples
#' library(lme4)
#' library(mgcv)
#'
#' ## Example 1: Cross-validation on linear mixed-effects model
#' # model1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#' # out1 <- model_logocv(model = model1, data = sleepstudy, group = "Subject")
#' # out1[[1]] # RMSE
#' # out1[[2]] # predicted values from cross-validation
#'
#' # convergence issues, so try different optimizer
#' # my_control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 10000))
#' # model1b <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy, control = my_control)
#' # model_logocv(model = model1b, data = sleepstudy, group = "Subject", control = my_control)
#'
#' ## Example 2: Cross-validation on hierarchical generalised additive model
#' # model2 <- gam(Reaction ~ s(Days) + s(Subject, bs = "re") + s(Days, Subject, bs = "re"), data = sleepstudy)
#' # model_logocv(model = model2, data = sleepstudy, group = "Subject")
#'
#' # compare alternative models
#' # model2b <- gam(Reaction ~ s(Days) + s(Subject, bs = "re"), data = sleepstudy)
#' # model2c <- gam(Reaction ~ s(Subject, bs = "re"), data = sleepstudy)
#' # out2 <- model_logocv(model = model2, data = sleepstudy, group = "Subject")
#' # out2b <- model_logocv(model = model2b, data = sleepstudy, group = "Subject")
#' # out2c <- model_logocv(model = model2c, data = sleepstudy, group = "Subject")
#' # out2[[1]]; out2b[[1]] ;out2c[[1]]




model_logocv <- function(model, data, group, control=NULL){

  ## error messages:

  # check that all arguments are provided
  if(missing(model)) {stop("'model' is missing; specify a lmerMod, lmerTest or gam model object for cross-validation")}
  if(missing(data)) {stop("'data' is missing; specify dataframe or tibble containing data used for model calibration")}
  if(missing(group)) {stop("'group' is missing; specify the name of the random grouping factor in 'model'")}

  # check that model is one of supported types
  if(class(model)[1] %in% c("lmerMod", "lmerTest", "gam") == FALSE) {stop("'model' must be a lmerMod or gam object")}

  # check that data is a data frame or tibble
  if(!is.data.frame(data)) {stop("'data' must be a dataframe or tibble")}

  # check that control is either NULL or a lmerControl object
  if(missing(control) == FALSE && is.null(control) == FALSE){
    if(!class(control)[1] == "lmerControl") {stop("'control' must be a lmerControl object")}
  }

  # check that group variable is in data
  if(group %in% colnames(data) == FALSE) {stop(paste0("'",group,"' cannot be found in 'data'"))}



  ## prep data for cross-validation
  mod_class <- class(model)[1]
  dat <- as.data.frame(data)
  dat$grp <- dat[,group]
  ntot <- dim(dat)[1] # total number of observations
  gps <- unique(dat$grp)
  ngps <- length(gps) # number of groups
  dat$folds <- numeric(ntot)
  predicted_cv <- numeric(ntot)

  ## extract formula and name of response variable
  frm <- formula(model)
  response <- all.vars(frm)[1]

  ## extract REML setting and optmizer (for lmer models only)
  if(mod_class == "lmerMod" | mod_class == "lmerTest"){
      isREML <- ifelse(lme4::getME(model, "REML")==0, FALSE, TRUE)
  }

  # predict.gam lacks an equivalent to re.form=NA, so instead find terms in model that
  # include random 'group' factor, so these terms can later be excluded when making
  # predictions (for gam models only)
  if(mod_class == "gam"){
    p <- predict(model, type="terms")
    term_names <- attributes(p)$dimnames[[2]]
    excl_terms <- term_names[grep(group, term_names)]
  }

  ## perform cross-validation
  if(mod_class == "lmerMod" | mod_class == "lmerTest"){
      for(i in 1:ngps){
        testIndexes <- which(dat$grp==gps[i], arr.ind=TRUE)
        # drop group i to create training dataset
        trainData <- dat[-testIndexes, ]
        # create test dataset containing only group i
        testData <- dat[testIndexes, ]
        print(paste0("Fitting model for group ", i))
        # train model on remaining groups
        if(is.null(control)==TRUE){
          train.mod <- lme4::lmer(frm, data=trainData, REML=isREML)
        } else {
          train.mod <- lme4::lmer(frm, data=trainData, REML=isREML, control = control)
        }
        # predict for group i with random terms excluded
        predicted_cv[testIndexes] <- predict(train.mod, newdata=testData,  re.form=NA)
      }
  }

  ## predict.gam doesn't work for new levels of group, so have to change the id of the
  ## withheld group, and then predict with random effects turned off
  if(mod_class == "gam"){
    for(i in 1:ngps){
      testIndexes <- which(dat$grp==gps[i], arr.ind=TRUE)
      # drop group i to create training dataset
      trainData <- dat[-testIndexes, ]
      # create test dataset containing only group i
      testData <- dat[testIndexes, ]
      # change the id of the withheld group to that of one in the training dataset
      testData[,group] <- trainData[1,group]
      print(paste0("Fitting model for group ", i))
      # train model on remaining groups
      train.mod <- mgcv::gam(frm, data=trainData)
      # predict for group i with random terms excluded
      predicted_cv[testIndexes] <- predict(train.mod, newdata=testData, exclude = excl_terms)
    }
  }


  ## calculate residuals (predicted - observed) and RMSE
  error_cv <- predicted_cv - dat[,response]
  RMSE <- sqrt(mean(error_cv^2))


  ## create data frame or tibble to output
  final_data = dplyr::bind_cols(data, data.frame(pred_cv = predicted_cv))

  return(list(RMSE = RMSE, data = final_data))

}










