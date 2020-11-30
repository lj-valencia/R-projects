# Time-Series Forecasting using Autoregressive LASSO
This folder contains files used for my time-series forecasting exercise using an autoregressive LASSO model. The macroeconomic indicator used for this project is the U.S. Industrial Production Index. In this exercise, I demonstrate how to create a lagged time-series dataset, how to fit an Autoregressive LASSO model using this lagged dataset, and how to use this fitted model for forecasting. In addition, I also use time-series cross validation to determine the best lambda (log-lambda) value and the model with the best predictive power. BibTeX is used to cite my sources. 

## About the files

- [autoregressive-lasso.R](Autoregressive-LASSO/autoregressive-lasso.R): This file is the R script I used for the forecasting exercise.
- [autoregressive-lasso.Rmd](Autoregressive-LASSO/autoregressive-lasso.Rmd): This is the R-markdown file used to turn my R code into a document.
- [autoregressive-lasso.pdf](Autoregressive-LASSO/autoregressive-lasso.pdf): The document outlining how to forecast using an Autoregressive LASSO Model. 
- [dataset.csv](Autoregressive-LASSO/dataset.csv): Quarterly series of U.S. macroeconomic data (1970 Q1-2019Q2), including the Industrial Production Index 
- [cite.bib](Autoregressive-LASSO/cite.bib): BibTeX file used for my citations.

## License
This project is made available under the [GNU General Public License v3.0](https://www.gnu.org/licenses/gpl-3.0.en.html).
