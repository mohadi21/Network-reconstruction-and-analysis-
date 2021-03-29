# Load the package plspm
library(plspm)

# Load data spainfoot
data("spainfoot")
head(spainfoot)
?spainfoot

# INNER MODEL
# ------
# Rows of the inner model matrix
# Format: square and lower triangle boolean matrix
Attack = c(0, 0, 0)
Defense = c(0, 0, 0)
Success = c(1, 1, 0)

foot_path = rbind(Attack, Defense, Success)
colnames(foot_path) =  rownames(foot_path)
foot_path

# Nice plot
innerplot(foot_path)

# OUTER MODEL
# ------
# Define the list of indicators for each block
# Attack, Defense, Success
foot_blocks = list(1:4, 5:8, 9:12)

# REFLECTIVE or FORMATIVE
# ------
# By default: mode A -- REFLECTIVE
foot_modes = c("A", "A", "A")
# else "B"

# RUN PLS-PM analysis
# ------
foot_pls = plspm(Data = spainfoot, 
                 path_matrix = foot_path,
                 blocks = foot_blocks, 
                 modes = foot_modes)
foot_pls
class(foot_pls)

# Path quantification
foot_pls$path_coefs
# Coefficient significance level
foot_pls$inner_model

summary(foot_pls)

# Plotting the analysis results
# ------
# Inner model
plot(foot_pls)

# Loadings
plot(foot_pls, what = "loadings", arr.width = 0.1)

# Scores for the index of success
head(foot_pls$scores, n = 10)
tail(foot_pls$scores, n = 10)

# ------------------------
# Analyzing the results, assessing the measurement model and assessing the structural model
# ------------------------
foot_pls
summary(foot_pls)

# Measurement model assessment
# Reflective indicators
# ----
# Reflective indicators are measuring the same underlying latent variables
# --> they need to have a strong mutual association/correlation
# --> they need to get along with its latent variable, one and only one!
# We must evaluate:
# (1) Unidimensionality of the indicators
# (2) Check that indicators are well explained by its latent variable
# (3) Assess the degree to which a given construct is different from other constructs

# ----
# Unidimensionality of the indicators (3 possible metrics)
# ----
# Cronbach's alpha
# Dillon-Goldstein's rho
# The first eigenvalue of the indicators' correlation matrix
foot_pls$unidim

# Cronbach's alpha ~ average inter-variable correlation
# ----
# !! The computation of this metric requires the observed variables 
# to be standardized and positively correlated
# Cronbach's index requires all indicators in reflective blocks to be positively correlated
foot_pls$unidim[, 3, drop = FALSE]

# Cronbach's alpha > 0.7 ==> acceptable
# Pbl with 'Defense'...

# Dillon-Goldstein's rho ~ variance of the sum of variables
# ----
# Takes into account to which extend the latent variable explains its block of indicators
foot_pls$unidim[, 4, drop = FALSE]

# Dillon-Goldstein's rho > 0.7 ==> acceptable
# Pbl with 'Defense'...

# First eigenvalue ~ eigen analysis of the correlation matrix of indicators
# ----
foot_pls$unidim[, 5:6]

# The first eigenvalue should be much more than 1
# and the second eigenvalue should be smaller than 1
# Pbl with 'Defense'...

# Solving the 'Defense' issue
# ----
# Plot loadings
plot(foot_pls, what = "loadings")
foot_pls$outer_model
subset(foot_pls$outer_model, block == "Defense")

# Plot weights
plot(foot_pls, what = "weights")

# GCH and GCA are measuring lack of "Defense". 
# high GCH or GCA ==> concede a lot of goals ==> poor defense!
# We need to change the signs...

# add two more columns NGCH and NGCA
spainfoot$NGCH = -1 * spainfoot$GCH
spainfoot$NGCA = -1 * spainfoot$GCA

# rerun plspm
new_blocks_pos = list(1:4, c(15, 16, 7, 8), 9:12)
## -- Better Defense Block
#new_blocks_pos = list(1:4, c(16, 7, 8), 9:12)
## -- Better Defense Block


new_blocks_str = list(
    c("GSH", "GSA", "SSH", "SSA"),
    c("NGCH", "NGCA", "CSH", "CSA"),
    ## -- Better Defense Block
    #c("NGCA", "CSH", "CSA"),
    ## -- Better Defense Block
    c("WMH", "WMA", "LWR", "LRWL"))
foot_pls = plspm(spainfoot, foot_path, new_blocks_str, modes = foot_modes)
plot(foot_pls, "loadings")

# better results!
foot_pls$unidim

# ----
# Loadings and communalities
# ----
# loadings: correlations between latent variable and its indicators
# loadings ~ the amount of variance shared between the contruct and its indicators
# communalities: squared correlations
foot_pls$outer_model

# loadings > 0.7 ==> acceptable

# communalities ~ the amount of variability explained by the latent variable
# loadings 0.7² ~ 50% of the variability in an indicator is captured by its latent construct

# NGCH Defense 0.1087511 0.4836811   0.2339474  0.0000000
# Does it make sense to keep this indicator in the block?

# ----
# Cross-Loadings
# ----
# Loadings of an indicator with the rest of latent variables
foot_pls$crossloadings

# Measurement model assessment
# Formative indicators
# ----
# Formative indicators are not supposed to be correlated...

# Structural model assessment
# ----
# Inspect the result of each regression
foot_pls$inner_model

# The quality of the structural model
# --> the R² determination coefficients
# --> the redundancy index
# --> the Goodness-of-Fit (GoF)

# Coefficients of determination R² (for endogenous latent variables)
# ----
foot_pls$inner_summary
foot_pls$inner_summary[, "R2", drop = FALSE]
# R² indicates the amount of variance in the endogeneous latent variable explained 
# by its independant latent variables

# Low: R² < 0.3 (or R² < 0.2)
# Moderate: 0.3 < R² < 0.6 (or 0.2 < R² < 0.5)
# High: R² > 0.6 (or R² > 0.5)

# Redundancy
# ----
# Reflects the ability of a set of independant latent variables to explain 
# the variation in the dependant latent variable (loading².R²)
# High redundancy ==> high ability to predict
foot_pls$inner_summary
# For expl.: The average redundancy for Success represents that Attack and 
# Defense predict 68% of the variability of Success indicators

# The Block_Communality: how much of the block variability is reproducible 
# by the latent variable

# GoF
# ----
# To assess both the measurement and the structural models.
# It is the geometric mean of the average communality and the average R²
foot_pls$gof
# --> global criterion (both inner and outer)!: assess the overall 
# prediction performance of the model.
# but no threshold... (empirical acceptability: GoF > 0.7)

# Validation
# ----
# ----
# We use Bootstrapping to estimate the variability of the parameters
foot_val = plspm(spainfoot, foot_path, new_blocks_str, modes = foot_modes,
                 boot.val = TRUE, br = 200)
foot_val$boot
