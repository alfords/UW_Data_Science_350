##---------------------------------------------------------------
## Data Science 350
## Week 7 homework 
## Aleksey Kramer
##---------------------------------------------------------------

require(pls)
require(glmnet)
require(logging)

# Retrieve Breast Cancer Expression Data From the following Study:
#http://www.ncbi.nlm.nih.gov/pubmed/21532620
if(interactive()) {
    
    setwd('/Users/voitel/TRAINING/UW_Data_Science/UW_Data_Science_350/Week7/homework')
    # setwd('C:\\Users\\db345c\\Desktop\\UW_TRAIN\\Week7\\homework')
    
    # Setup Test Logger
    basicConfig()
    addHandler(writeToFile, file="HW-WEEK7-LOG.txt", level='DEBUG')

    loginfo("# Read the study data in memory")
    micro_data=read.table("MicroArray.txt", header=TRUE)

    loginfo("# Explora micro_data")
    dim(micro_data)

    loginfo("# Normalize each column")
    micro_data <- scale(micro_data)

    loginfo("# Breast Cancer Samples:")
    cancer_samples <- c(0,0,0,0,0,1,0,0,0,1,0,1,0,0,1,0,0,0,1)

    # Think of this as ~ 10,000 possible variables (genes) to predict 19 outcomes.

    loginfo("# Convert to data.frame")
    micro_frame <- data.frame(t(micro_data))
    micro_frame$outcomes <- cancer_samples

    ##-----Lasso Regression-----
    # user glmnet, where family = 'binomial'

    loginfo("# Create predictor and xfactors")
    predictor <- cancer_samples
    
    loginfo("Using glmnet")
    outcomes_lasso = glmnet(t(micro_data), predictor, alpha=1, family='binomial')

    plot(outcomes_lasso, xvar="lambda")
    outcomes_lasso 

    loginfo("Printing lasso coefficients")
    coef(outcomes_lasso)[,20][coef(outcomes_lasso)[,20]>1e-10]

    loginfo("# Now use cv.glmnet to test different lasso cutoffs")
    outcomes_lasso_cv = cv.glmnet(t(micro_data), predictor, alpha=1, family='binomial')
    plot(outcomes_lasso_cv) # There is a minimum in MSE!


    loginfo("# find the minumum lambda.min")
    best_lambda = outcomes_lasso_cv$lambda.min

    loginfo("# Find the coefficients that are greater than zero")
    # may take some time to compute
    repeat{
        outcomes_lasso_cv = cv.glmnet(t(micro_data), predictor, alpha=1, family='binomial')
        best_lambda = outcomes_lasso_cv$lambda.min
        best_coef = coef(outcomes_lasso)[,outcomes_lasso$lambda == best_lambda]
        if(length(which(best_coef > 0) > 0)){
            print(which(best_coef > 0))
            break
        }
    }

    loginfo("Finding best coefficient")
    best_coef = best_coef[best_coef > 1e-10]

    loginfo("# extract names")
    m_names <- gsub("V","X",paste(names(best_coef)[2:length(best_coef)], collapse = " + "))
    formula_string <- paste("outcomes ~ ", m_names, collapse = "")

    loginfo("# Plug this into the glm(...,family='binomial') to get the logistic outcome")
    m_outcome <- glm(as.formula(formula_string), data = micro_frame, family = binomial)

    loginfo("# Comparing with the real outcome, cancer_samples above")
    print(summary(m_outcome))
    print("Cancer Samples")
    print(cancer_samples)
    print("Predictions")
    print(round(as.numeric(m_outcome$fitted.values)))
}


