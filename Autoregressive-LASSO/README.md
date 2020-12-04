# Time-Series Forecasting using Autoregressive LASSO
This folder contains files used for how to conduct time-series forecasting using an autoregressive LASSO model. The macroeconomic indicator used for this project is the U.S. Industrial Production Index. I demonstrate how to create a lagged time-series dataset, how to estimate an autoregressive LASSO model using this lagged dataset, and how to use this estimated model for forecasting. Time-series cross validation is used to determine the best log-lambda value which yields the model with the best predictive power. I used BibTeX to cite my sources. Here are a brief description of the files.

## About the files
- [autoregressive-lasso.R](autoregressive-lasso.R): This file is the R script I used for the forecasting exercise.
- [autoregressive-lasso.Rmd](autoregressive-lasso.Rmd): This is the R-markdown file used to turn my R code into a document.
- [autoregressive-lasso.pdf](autoregressive-lasso.pdf): The PDF-formatted document outlining how to forecast using an Autoregressive LASSO Model. 
- [dataset.csv](dataset.csv): Quarterly series of U.S. macroeconomic data (1970 Q1-2019Q2), including the Industrial Production Index 
- [cite.bib](cite.bib): BibTeX file used for my citations.

## License
This project is made available under the [GNU General Public License v3.0](https://www.gnu.org/licenses/gpl-3.0.en.html).
