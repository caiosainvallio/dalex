# R snippets for DALEX

# For pipes
library("magrittr")


# Prepare data
library("DALEX")
head(titanic_imputed)
dim(titanic_imputed)

# Train a model
library("ranger")
set.seed(1313)
titanic_rf = ranger(survived ~ class + gender + age +
                 sibsp + parch + fare + embarked,
                 data = titanic_imputed,
                 probability = TRUE,
                 classification = TRUE)
titanic_rf


# Prepare an explainer

# basic use
titanic_ex = explain(titanic_rf,
                data  = titanic_imputed,
                y     = titanic_imputed$survived,
                label = "Regression Forest")


# advanced use
titanic_ex = explain(titanic_rf,
                data  = titanic_imputed,
                y     = titanic_imputed$survived,
                label = "Regression Forest",
                predict_function = function(model, data)
                  matrix(predict(model, data,
                                 probability = TRUE)$predictions,
                         ncol=2)[,2]
)


# internals
titanic_ex$model                # encapsulated model
titanic_ex$model_info           # version of model factory
titanic_ex$data |> head()       # encapsulated data
titanic_ex$predict_function     # derived predict
titanic_ex$y_hat |> head()      # calculated predictions
titanic_ex$residuals |> head()  # calculated residuals
titanic_ex$label                # the model label

# explanations from the DALEX package
# instance level
(single_passanger = titanic_imputed[5,])
# prediction
predict(single_passanger, titanic_ex)

# prediction parts (see more in episode 3 and 4)
plot(predict_parts(titanic_e, new_observation = single_passanger))

# prediction profile (see more in episode 5)
plot(predict_profile(titanic_ex, new_observation = single_passanger), variables = c("age", "fare", "parch"))


# dataset level
# model performance (see more in episode 6)
plot(model_performance(titanic_ex), geom = "roc")

# model parts (see more in episode 7)
plot(model_parts(titanic_ex), show_boxplots = FALSE)

# model profile (see more in episode 8)
plot( model_profile(titanic_ex), variables = c("age", "fare", "parch"))

# model diagnostic (see more in episode 9)
plot(model_diagnostics(titanic_ex), variable = "age", yvariable = "abs_residuals")


# champion challenger
# second model - logistic regression with rms
library("rms")
set.seed(1313)
titanic_lmr= lrm(survived == "yes" ~ gender + rcs(age) +
                   class + sibsp + parch + fare +
                   embarked, titanic)
titanic_ex2 = explain(titanic_lmr,
                    data  = titanic_imputed,
                    y     = titanic_imputed$survived,
                    label = "Logistic regression")

# ROC for both
plot(model_performance(titanic_ex) ,
     model_performance(titanic_ex2) ,
     geom = "roc")

# LIFT for both
plot(model_performance(titanic_ex) ,
     model_performance(titanic_ex2) ,
     geom = "lift")

# PDP for both
plot(model_profile(titanic_ex)$agr_profiles ,
     model_profile(titanic_ex2)$agr_profiles ,
     variables = c("age", "fare", "parch"))


# example for model down
# static HTML site with data explainers for models
library("modelDown")
modelDown(titanic_ex, titanic_ex2)

# example for model studio
# interactive HTML site with data explainers for models

library("modelStudio")
modelStudio(titanic_ex, single_passanger)
