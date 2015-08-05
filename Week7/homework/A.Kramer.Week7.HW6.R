##---------------------------------------------------------------
## Data Science 350
## Week 7 homework 
## Aleksey Kramer
##---------------------------------------------------------------

require(pls)
require(glmnet)

# Retrieve Breast Cancer Expression Data From the following Study:
#http://www.ncbi.nlm.nih.gov/pubmed/21532620

setwd('/Users/voitel/TRAINING/UW_Data_Science/UW_Data_Science_350/Week7/homework')
# setwd('C:\\Users\\db345c\\Desktop\\UW_TRAIN\\Week7\\homework')

# Read the study data in memory
micro_data=read.table("MicroArray.txt", header=TRUE)

# Explora micro_data
dim(micro_data)

# Normalize each column
micro_data = scale(micro_data)

# Breast Cancer Samples:
cancer_samples = c(0,0,0,0,0,1,0,0,0,1,0,1,0,0,1,0,0,0,1)

# Think of this as ~ 10,000 possible variables (genes) to predict 19 outcomes.

# Convert to data.frame
micro_frame = data.frame(t(micro_data))
micro_frame$outcomes = cancer_samples

##-----Lasso Regression-----
# user glmnet, where family = 'binomial'

# Create predictor and xfactors
predictor <- cancer_samples
xfactors <- model.matrix(outcomes ~ . , data = micro_frame)[,-1]

outcomes_lasso = glmnet(xfactors, predictor, alpha=1, family='binomial')

plot(outcomes_lasso, xvar="lambda")

outcomes_lasso 

coef(outcomes_lasso)[,20][coef(outcomes_lasso)[,20]>1e-10]

# Now use cv.glmnet to test different lasso cutoffs
outcomes_lasso_cv = cv.glmnet(xfactors, predictor, alpha=1, family='binomial')
plot(outcomes_lasso_cv) # There is a minimum in MSE!


# find the minumum lambda.min
best_lambda = outcomes_lasso_cv$lambda.min

# Find the coefficients that are greater than zero
# Takes some time to compute
repeat{
    outcomes_lasso_cv = cv.glmnet(xfactors, predictor, alpha=1, family='binomial')
    best_lambda = outcomes_lasso_cv$lambda.min
    best_coef = coef(outcomes_lasso)[,outcomes_lasso$lambda == best_lambda]
    if(length(which(best_coef > 0) > 0)){
        print(which(best_coef > 0))
        break
    }
}

best_coef = best_coef[best_coef > 1e-10]

# extract names
m_names <- paste(names(best_coef)[2:length(best_coef)], collapse = " + micro_frame$")

# Plug this into the glm(...,family='binomial') to get the logistic outcome
m_outcome <- glm(micro_frame$outcomes ~ micro_frame[,best_coef], family = binomial)

# Compare with the real outcome, cancer_samples above

