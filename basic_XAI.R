####################################################################################
# BASIC XAI with DALEX— Part 1: Introduction
####################################################################################

#library
library(DALEX)


#apartments dataset from DALEX
#data have 5 numerical variables and 1 factor
head(apartments)


#we use one-hot encoding for district variable - one_hot() function from mltools
data <- mltools::one_hot(data.table::data.table(apartments))


#we created a random forest model using ranger library
library(ranger)
model <- ranger(m2.price~., data = data)


#we created an explainer with DALEX package
explainer <- explain(model, data = data, y = data$m2.price)


####################################################################################
# BASIC XAI with DALEX — Part 2: Permutation-based variable importance
####################################################################################

# we created a model_parts object
mp <- model_parts(explainer, loss_function = loss_root_mean_square)

# we can see a data.frame with results
mp

# we can plot the results
plot(mp)



####################################################################################
# BASIC XAI with DALEX — Part 3: Partial Dependence Profile
####################################################################################

# PDP profile for surface and construction.year variable
rf_mprofile <- model_profile(explainer = explainer, variables = c("surface", "construction.year"), type = "partial")

# plot PDP
plot(rf_mprofile)

# PDP profile for surface and construction.year variable grouped by no.rooms
rf_mprofile_group <- model_profile(explainer = explainer, variables = c("surface", "construction.year"), groups = "no.rooms", type = "partial")

# plot PDP
plot(rf_mprofile_group)

# create a linear regresion model
model_lm <- lm(m2.price~., data = data)

# create an explainer for linear regresion model
explainer_lm <- explain(model_lm, data = data, y = data$m2.price)

# PDP profile for surface and construction.year variable
lm_mprofile <- model_profile(explainer = explainer_lm, variables = c("surface", "construction.year"))

# comparison for random forest and linear regression model
plot(rf_mprofile, lm_mprofile)

####################################################################################
# BASIC XAI with DALEX — Part 4: Break Down method
####################################################################################

# apartment which prediction we explain
apartment = data.frame(construction.year = 1995,
                       surface = 93,
                       floor = 7,
                       no.rooms = 3,
                       district_Bemowo = 0,
                       district_Bielany = 0,
                       district_Mokotow = 0,
                       district_Ochota = 1,
                       district_Praga = 0,
                       district_Srodmiescie = 0,
                       district_Ursus = 0,
                       district_Ursynow = 0,
                       district_Wola = 0,
                       district_Zoliborz = 0)


# Break Down for apartment observation
rf_pparts = predict_parts(explainer = explainer, new_observation = apartment, type = "break_down")


# plot Break Down
plot(rf_pparts)


####################################################################################
# BASIC XAI with DALEX — Part 5: Shapley values
####################################################################################

# apartment which prediction we explain
apartment = data.frame(construction.year = 1995,
                       surface = 93,
                       floor = 7,
                       no.rooms = 3,
                       district_Bemowo = 0,
                       district_Bielany = 0,
                       district_Mokotow = 0,
                       district_Ochota = 1,
                       district_Praga = 0,
                       district_Srodmiescie = 0,
                       district_Ursus = 0,
                       district_Ursynow = 0,
                       district_Wola = 0,
                       district_Zoliborz = 0)

# Shapley values for apartment observation
rf_pparts = predict_parts(explainer = explainer, new_observation = apartment, type = "shap")

# plot Shapley values
plot(rf_pparts)


####################################################################################
# BASIC XAI with DALEX — Part 6: LIME method
####################################################################################

# apartment which prediction we explain
apartment = data.frame(construction.year = 1995,
                       surface = 93,
                       floor = 7,
                       no.rooms = 3,
                       district_Bemowo = 0,
                       district_Bielany = 0,
                       district_Mokotow = 0,
                       district_Ochota = 1,
                       district_Praga = 0,
                       district_Srodmiescie = 0,
                       district_Ursus = 0,
                       district_Ursynow = 0,
                       district_Wola = 0,
                       district_Zoliborz = 0)

# LIME for apartment observation
# install.packages("lime")
library(lime)

# lime object 
explain_mod <- lime(data, model)

# lime explanation
mod_lime <- explain(apartment, explain_mod, n_features = 8)

# plot LIME
plot_features(mod_lime)

####################################################################################
# BASIC XAI with DALEX — Part 7: Ceteris Paribus profiles
####################################################################################

# apartment which prediction we explain
apartment = data.frame(construction.year = 1995,
                       surface = 93,
                       floor = 7,
                       no.rooms = 3,
                       district_Bemowo = 0,
                       district_Bielany = 0,
                       district_Mokotow = 0,
                       district_Ochota = 1,
                       district_Praga = 0,
                       district_Srodmiescie = 0,
                       district_Ursus = 0,
                       district_Ursynow = 0,
                       district_Wola = 0,
                       district_Zoliborz = 0)

# Ceteris Paribus profile for apartment observation
rf_pprofile = predict_profile(explainer = explainer, new_observation = apartment)

# plot Ceteris Paribus profile
plot(rf_pprofile, variables = c("construction.year", 'surface', 'floor', 'no.rooms'))






