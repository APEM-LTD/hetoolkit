# Test Error Messages

# create example inputs
sleepstudy <- lme4::sleepstudy
sleepstudy$type <- rep(c("A","B","C"),60)
mod <- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)


# make sure model object of the correct class has been input:
expect_error(diag_lmer(
  model = "hello", data = sleepstudy
), "please specify a lmer class of model")


# is data there and a dataframe?
expect_error(diag_lmer(
  model = mod, data = NULL
), "data missing, with no default")

expect_error(diag_lmer(
  model = mod, data = "hello"
), "data input must be a dataframe")


# if not null facet_by and colour_by listed in dataframe
expect_error(diag_lmer(
  model = mod, data=sleepstudy, facet_by = "hello"
), "facet_by name not within supplied dataframe, revise facet_by name or add to datafame")

expect_error(diag_lmer(
  model = mod, data=sleepstudy, colour_by = "hello"
), "colour_by name not within supplied dataframe, revise colour_by name or add to datafame")

expect_error(diag_lmer(
  model = mod, data=sleepstudy, facet_by = "Subject", order_by = "hello"
  ), "'order_by' must be either 'mean' or 'variance'")

expect_error(diag_lmer(
  model = mod, data=sleepstudy, facet_by = "Subject", order_by = "mean", order = "hello"
),"'order' must be either 'ascending' or 'descending'")

expect_error(diag_lmer(
  model = mod, data=sleepstudy, facet_by = "Subject", order_by = "mean", scales = "hello"
),"'scales' must be one of 'fixed', 'free_x', 'free_y', or 'free'")


# test save results file errors:
expect_error(diag_lmer(
  model = mod, data = sleepstudy, save = TRUE, save_dir = "hello"
), "Specified save directory does not exist")

expect_error(diag_lmer(
  model = mod, data = sleepstudy,  save = "hello"
), "Save is not logical")


