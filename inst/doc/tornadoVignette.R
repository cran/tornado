## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
has_survival <- require(survival)
has_caret <- require(caret) & require(randomForest)
has_glmnet <- require(glmnet)
require(tornado)

## ----datasets, echo=TRUE------------------------------------------------------
mtcars_w_factors <- mtcars
# Automatic vs Manual
mtcars_w_factors$am <- factor(mtcars$am)
# V or Straight cylinder arrangement
mtcars_w_factors$vs <- factor(mtcars$vs)
# number of cylinders
mtcars_w_factors$cyl <- factor(mtcars$cyl)
# number of forward gears
mtcars_w_factors$gear <- factor(mtcars$gear)
# number of carburetors
mtcars_w_factors$carb <- factor(mtcars$carb)

## ----lm1, echo=TRUE-----------------------------------------------------------
lm1 <- lm(mpg ~ cyl*wt*hp, data = mtcars)
torn1 <- tornado::tornado(lm1, type = "PercentChange", alpha = 0.10)
plot(torn1, xlabel = "MPG", geom_bar_control = list(width = 0.4))

## ----lm2, echo=TRUE-----------------------------------------------------------
torn2 <- tornado::tornado(lm1, type = "ranges")
plot(torn2, xlabel = "MPG", geom_bar_control = list(width = 0.4))

## ----lm3, echo=TRUE-----------------------------------------------------------
torn3 <- tornado::tornado(lm1, type = "percentiles", alpha = 0.05)
plot(torn3, xlabel = "MPG", geom_bar_control = list(width = 0.4))

## ----lm4, echo=TRUE-----------------------------------------------------------
lm4 <- lm(mpg ~ cyl + wt + hp + vs, data = mtcars_w_factors)
torn4 <- tornado::tornado(lm4, type = "percentiles", alpha = 0.05)
plot(torn4, xlabel = "MPG", geom_bar_control = list(width = 0.4))

## ----lm5, echo=TRUE-----------------------------------------------------------
dict <- list(old = c("cyl", "wt", "hp", "vs"),
             new = c("Cylinders", "Weight", "Horsepower", "V_or_Straight"))
torn5 <- tornado::tornado(lm4, type = "percentiles", alpha = 0.05, dict = dict)
plot(torn5, xlabel = "MPG", geom_bar_control = list(width = 0.4))

## ----lm6, echo=TRUE-----------------------------------------------------------
dict <- list(old = c("cyl", "wt", "hp", "vs"),
             new = c("Cylinders", "Weight", "Horsepower", "V_or_Straight"))
torn5 <- tornado::tornado(lm4, type = "percentiles", alpha = 0.05)
plot(torn5, xlabel = "MPG", geom_bar_control = list(width = 0.4),
     sensitivity_colors = c("#FC8D62", "#66C2A5"),
     geom_point_control = list(size = 3, fill = "purple", col = "purple"))

## ----lm7, echo=TRUE-----------------------------------------------------------
g <- plot(torn5, plot = FALSE, xlabel = "MPG", geom_bar_control = list(width = 0.4),
          sensitivity_colors = c("#FC8D62", "#66C2A5"),
          geom_point_control = list(size = 3, fill = "purple", col = "purple"))
g <- g + ggtitle("Test Plot")
g <- g + geom_hline(yintercept = 0, col = "black", lwd = 2)
plot(g)

## ----glm1, echo = TRUE--------------------------------------------------------
glm1 <- glm(vs ~ wt + disp + cyl, data = mtcars, family = binomial(link = "logit"))
torn1 <- tornado::tornado(glm1, type = "ranges", alpha = 0.10)
plot(torn1, xlabel = "V or Straight Engine", geom_bar_control = list(width = 0.4))

## ----censored1, echo=TRUE-----------------------------------------------------
survreg1 <- survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps + rx + age + resid.ds, 
                              survival::ovarian, dist = 'weibull', scale = 1)
torn1 <- tornado::tornado(survreg1, modeldata = survival::ovarian, 
                          type = "PercentChange", alpha = 0.10)
plot(torn1, xlabel = "Survival Time", geom_bar_control = list(width = 0.4))

## ----censored2, echo=TRUE-----------------------------------------------------
coxph1 <- survival::coxph(survival::Surv(stop, event) ~ rx + size + number,
                          survival::bladder)
torn1 <- tornado::tornado(coxph1, modeldata = survival::bladder, type = "PercentChange",
                          alpha = 0.10)
plot(torn1, xlabel = "Risk", geom_bar_control = list(width = 0.4))

## ----glmnet1, echo=TRUE-------------------------------------------------------
if (has_glmnet)
{
  form <- formula(mpg ~ cyl*wt*hp)
  mf <- model.frame(form, data=mtcars)
  mm <- model.matrix(form, mf)
  gtest <- glmnet::cv.glmnet(x = mm, y = mtcars$mpg, family = "gaussian")
  torn <- tornado::tornado(gtest, modeldata = mtcars, 
                           form = formula(mpg ~ cyl*wt*hp), 
                           s = "lambda.1se",
                           type = "PercentChange", alpha = 0.10)
  plot(torn, xlabel = "MPG", geom_bar_control = list(width = 0.4))
} else
{
  print("glmnet is not available for vignette rendering")
}

## ----train_regression, echo = TRUE--------------------------------------------
if (has_caret)
{
  gtest <- caret::train(x = subset(mtcars_w_factors, select = -mpg), 
                        y = mtcars_w_factors$mpg, method = "rf")
  torn <- tornado::tornado(gtest, type = "percentiles", alpha = 0.10)
  plot(torn, xlabel = "MPG")
} else
{
  print("caret is not available for vignette rendering")
}

## ----train_section, echo = TRUE-----------------------------------------------
if (has_caret)
{
  gtest <- caret::train(x = subset(iris, select = -Species), 
                        y = iris$Species, method = "rf")
  torn <- tornado::tornado(gtest, type = "percentiles", alpha = 0.10, class_number = 1)
  g <- plot(torn, plot = FALSE, xlabel = "Probability of the Setosa Species")
  g <- g + ggtitle("Classifier caret::train randomforest, 10th to 90th percentiles of each variable")
  plot(g)

  torn <- tornado::tornado(gtest, type = "percentiles", alpha = 0.10, class_number = 2)
  g <- plot(torn, plot = FALSE, xlabel = "Probability of the versicolor Species")
  plot(g)
} else
{
  print("caret is not available for vignette rendering")
}

## ----imp.lm, echo=TRUE--------------------------------------------------------
gtest <- lm(mpg ~ cyl*wt*hp + gear + carb, data = mtcars)
gtestreduced <- lm(mpg ~ 1, data = mtcars)
imp <- tornado::importance(gtest, gtestreduced)
plot(imp)

## ----imp.lm.dict, echo=TRUE---------------------------------------------------
dict <- list(old = c("cyl", "wt", "hp", "vs", "gear", "carb"),
             new = c("Cylinders", "Weight", "Horsepower", "V_or_Straight", "Num Gears", "Num Carbs"))
imp <- tornado::importance(gtest, gtestreduced, dict = dict)
plot(imp, col_importance_alone = "#8DD3C7", 
     col_importance_cumulative = "#FFFFB3")

## ----imp.glm, echo=TRUE-------------------------------------------------------
gtest <- glm(vs ~ wt + disp + gear, data = mtcars, family = binomial(link = "logit"))
gtestreduced <- glm(vs ~ 1, data = mtcars, family = binomial(link = "logit"))
imp <- tornado::importance(gtest, gtestreduced)
plot(imp)

## ----imp_survreg, echo=TRUE---------------------------------------------------
model_final <- survival::survreg(survival::Surv(futime, fustat) ~ ecog.ps*rx + age,
                                 data = survival::ovarian,
                                 dist = "weibull")
imp <- tornado::importance(model_final, survival::ovarian, nperm = 100)
plot(imp, geom_bar_control = list(width = 0.4, fill = "blue"))

## ----train.regression.imp, echo = TRUE----------------------------------------
if (has_caret)
{
  gtest <- caret::train(x = subset(mtcars_w_factors, select = -mpg), 
                        y = mtcars_w_factors$mpg, method = "rf")
  imp <- tornado::importance(gtest)
  plot(imp, nvar = 7)
} else
{
  print("caret is not available for vignette rendering")
}

## ----train.classification.imp, echo = TRUE------------------------------------
if (has_caret)
{
  gtest <- caret::train(x = subset(iris, select = -Species), 
                        y = iris$Species, method = "rf")
  imp <- tornado::importance(gtest)
  g <- plot(imp, plot = FALSE)
  g <- g + ggtitle("Classifier caret::train randomforest: variable improtance")
  plot(g)
} else
{
  print("caret is not available for vignette rendering")
}

