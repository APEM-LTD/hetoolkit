#' Repeated, stratified k-fold cross-validation
#'
#' @description model_cv performs repeated, stratified k-fold cross-validation on a linear mixed-effects model (class: lmerMod) or hierarchical generalized additive model (class: gam) model with a single random grouping factor.
#'
#' @usage
#' model_cv(model, data, group, k=5, r=1, control=NULL)
#'
#' @param model Model object for cross-validation. Supported classes are "lmerMod" (from lme4::lmer) and "gam" (from mgcv::gam)
#' @param data Data frame or tibble containing data used for model calibration. None of the variables used in 'model' can contain NAs.
#' @param group Name of variable in data used as a random grouping factor in model.
#' @param k Number of folds. Default = 5.
#' @param r Number of repeats. Default = 1.
#' @param control Optional settings to prevent convergence issues with lmer models.
#' Default = NULL.
#'
#' @details k-fold cross validation is a re-sampling procedure than can be used to evaluate a model’s predictive performance for groups in the calibration dataset. If the grouping factor is Site, then the function evaluates the model’s ability to predict the response at sites in the calibration dataset.
#'
#' The data is randomly split into k folds. One fold is omitted (to form a separate test set), the model is fitted to the data from the remaining folds (the training set), and the re-fitted model is then used to predict the response variable for the test set. This is repeated k times so that a prediction is generated for every observation in the full dataset. If required, the whole process can then be repeated r times to provide a more precise estimate of model performance, albeit at greater computational cost. The overall performance of the model is measured by the root mean square error (RMSE), which quantifies the difference between the observed and predicted values. When r>1, the RMSE is calculated using the predicted values across all repeats. Comparing RMSE values for competing models can be used to guide model selection.
#'
#' Because lmer and gam models include a random grouping factor (e.g. site), the cross-validation procedure is stratified, to ensure that the observations for each group are split as evenly as possible among the k folds.
#'
#' There is no hard rule for choosing k, but values of 5 or 10 have been demonstrated empirically to provide a good trade-off between bias and variance in a model’s estimated performance. r takes a default value of 1, but can be increased to yield a more precise estimate of the RMSE.
#'
#' When refitting the model during cross-validation, all arguments to lmer() and gam() take default values, with the exception of (i) 'REML' (which inherits from the original lmer model object), and (ii) 'control' (which can be set via the 'control' argument for lmer models only).
#'
#' This function is not recommended for use on more complex models with multiple (crossed or nested) random grouping factors.
#'
#' @return A list of two elements: (i) the RMSE, and (ii) a data frame or tibble containing the input dataset plus r additional columns containing the cross-validation predicted values (named pred_cv#).
#'
#' @export
#'
#' @examples
#' library(lme4)
#' library(mgcv)
#'
#' ## Example 1: Cross-validation on linear mixed-effects model
#' # model1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#' # out1 <- model_cv(model = model1, data = sleepstudy, group = "Subject", k = 5, r = 1)
#' # out1[[1]] # RMSE
#' # out1[[2]] # predicted values from cross-validation
#'
#' # more precise estimate of RMSE by increasing r
#' # model_cv(model = model1, data = sleepstudy, group = "Subject", k = 5,  r = 10)
#'
#' # convergence issues, so try different optimizer
#' # my_control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 10000))
#' # model1b <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy, control = my_control)
#' # model_cv(model = model1b, data = sleepstudy, group = "Subject", k = 5, r = 1, control = my_control)
#'
#' ## Example 2: Cross-validation on hierarchical generalised additive model
#' # model2 <- gam(Reaction ~ s(Days) + s(Subject, bs = "re") + s(Days, Subject, bs = "re"), data = sleepstudy)
#' # model_cv(model = model2, data = sleepstudy, group = "Subject", k = 10, r = 1)
#'
#' # compare alternative models
#' # model2b <- gam(Reaction ~ s(Days) + s(Subject, bs = "re"), data = sleepstudy)
#' # model2c <- gam(Reaction ~ s(Subject, bs = "re"), data = sleepstudy)
#' # out2 <- model_cv(model = model2, data = sleepstudy, group = "Subject", k = 10, r = 1)
#' # out2b <- model_cv(model = model2b, data = sleepstudy, group = "Subject", k = 10, r = 1)
#' # out2c <- model_cv(model = model2c, data = sleepstudy, group = "Subject", k = 10, r = 1)
#' # out2[[1]]; out2b[[1]] ;out2c[[1]]




model_cv <- function(model, data, group, k=5, r=1, control=NULL){

  ## error messages

  # check that all arguments are provided
  if(missing(model)) {stop("'model' is missing; specify a lmerMod or gam model object for cross-validation")}
  if(missing(data)) {stop("'data' is missing; specify dataframe or tibble containing data used for model calibration")}
  if(missing(group)) {stop("'group' is missing; specify the name of the random grouping factor in 'model'")}

  # check that model is one of supported types
  if(class(model)[1] %in% c("lmerMod", "gam") == FALSE) {stop("'model' must be a lmerMod or gam object")}

  # check that data is a data frame or tibble
  if(!is.data.frame(data)) {stop("'data' must be a dataframe or tibble")}

  # check that control is either NULL or a lmerControl object
  if(missing(control) == FALSE && is.null(control) == FALSE){
    if(!class(control)[1] == "lmerControl") {stop("'control' must be a lmerControl object")}
  }

  # check that group variable is in data
  if(group %in% colnames(data) == FALSE) {stop(paste0("'",group,"' cannot be found in 'data'"))}

  # check that k is valid
  if(!is.numeric(k)) {stop("'k' must be an integer >=1")}
  if(is.numeric(k) && k%%1!=0) {stop("'k' must be an integer >=1")}
  if(is.numeric(k) && k<1) {stop("'k' must be an integer >=1")}

  # check that r is valid
  if(!is.numeric(r)) {stop("'r' must be an integer >=1")}
  if(is.numeric(r) && r%%1!=0) {stop("'r' must be an integer >=1")}
  if(is.numeric(r) && r<1) {stop("'r' must be an integer >=1")}


  ## prep data for cross-validation
  mod_class <- class(model)[1]
  dat <- as.data.frame(data)
  dat$grp <- dat[,group]
  ntot <- dim(dat)[1] # total number of observations
  gps <- unique(dat$grp)
  ngps <- length(gps) # number of groups
  dat$folds <- numeric(ntot)
  predicted_cv <- matrix(data = NA, nrow = ntot, ncol = r, byrow = FALSE, dimnames = list(c(seq(1:ntot)), paste0(c("pred_cv"),c(seq(1:r)))))

  ## extract formula and name of response variable
  frm <- formula(model)
  response <- all.vars(frm)[1]

  ## extract REML setting (for lmer models only)
  if(mod_class == "lmerMod"){
    isREML <- ifelse(lme4::getME(model, "REML")==0, FALSE, TRUE)
  }

  ## perform cross-validation
  for(j in 1:r){

    print(paste0("Repeat #", j))

    # create folds in data, by group, ready for cross validation
    for(ii in 1:ngps){
      # flag samples for ii-th group
      dat$siteflag <- dat$grp==gps[ii]
      # number of samples for i-th group
      n <- sum(dat$siteflag)
      # find indices for samples for ii-th group
      Indexes <- which(dat$siteflag==TRUE, arr.ind=TRUE)
      # create folds
      dat$folds[Indexes] <- match(rep(1:k,length=n),sample(1:k))
    }

    # check folds (should be approx equal number in each fold)
    # tapply(dat$folds,list(dat$grp,as.factor(dat$folds)),length)
    # check folds (should be approx equal number in each fold)
    # tapply(dat$folds,list(as.factor(dat$folds)),length)

    if(mod_class == "lmerMod"){

      for(i in 1:k){
        testIndexes <- which(dat$folds==i,arr.ind=TRUE)
        # drop fold i to create training dataset
        trainData <- dat[-testIndexes, ]
        # create test dataset containing only fold i
        testData <- dat[testIndexes, ]
        print(paste0("Fitting model for fold ", i))
        # train model on remaining folds
        if(is.null(control)==TRUE){
          train.mod <- lme4::lmer(frm, data=trainData, REML=isREML)
        } else {
          train.mod <- lme4::lmer(frm, data=trainData, REML=isREML, control = control)
        }
        # predict for fold i with random terms included
        predicted_cv[testIndexes,j] <- predict(train.mod, newdata=testData, allow.new.levels = FALSE)
      }

    }

    if(mod_class == "gam"){

      for(i in 1:k){
        testIndexes <- which(dat$folds==i,arr.ind=TRUE)
        # drop fold i to create training dataset
        trainData <- dat[-testIndexes, ]
        # create test dataset containing only fold i
        testData <- dat[testIndexes, ]
        print(paste0("Fitting model for fold ", i))
        # train model on remaining folds
        train.mod <- mgcv::gam(frm, data=trainData)
        # predict for fold i with random terms included
        predicted_cv[testIndexes,j] <- predict(train.mod, newdata=testData)
      }

    }

  }


  ## calculate residuals (predicted - observed) and RMSE
  error_cv <- predicted_cv - dat[,response]
  RMSE <- sqrt(mean(error_cv^2))

  ## create data frame or tibble to output
  final_data = dplyr::bind_cols(data, data.frame(predicted_cv))

  return(list(RMSE = RMSE, data = final_data))

}

