# biostat625_hw
biostat625_rpackage
## Introduction
The package loglik models provides users with capabilities of running MLE analysis using two basic models. The package contains main function ll.models, an R vignette with the description of the MLE estimation process and examples as well as data sets Affairs_dataset and iris_dataset to play around with the function. Please follow standard installation procedure required for any R package. You can provide either dataset or matrix to the model, the function will automatically convert everything to a matrix of (1, X).

## Limitations
1. Currently only two models are provided, the output consists of the point estimates and associated standard errors
2. For logistic regression only logit model is available
3. The model does not support automatic creation of the dummy variables for factor and character type variables
