# Description
- `Credit risk project` that involves generating and analyzing simulating financial data to assess credit risk.  

# Content
- [database](https://github.com/dzima22/credit-risk-project-/tree/main/database) - raw dataset 
- [risk_code.R](https://github.com/dzima22/credit-risk-project-/blob/main/risk_code.R) - analysis, modeling and testing procedure stored as R script

# Methodology
- **Data Generation**:
    + Use the provided 00_database_generation.R script to generate loans.csv and clients.csv.
- **Data Merging**:
    + Merge the two datasets
- **Feature Engineering**:
    + Create explanatory variables for the Probability of Default (PD) model using the client data.
- **Outlier Handling**:
    + Verify if the explanatory variables contain outliers and adjust the data series accordingly using winsorization.
- **Probability Estimation**:
    + Estimate the default probability for each client using a logistic regression model and probit model.
- **Contingency Table**:
    + Create and interpret a confusion matrix to evaluate the classification performance.
- **ROC Curve and AUC**:
    + Plot the ROC curve and calculate the AUC and also interpret the results.
- **Bootstrap Confidence Intervals**:
    + Calculate bootstrap confidence intervals for the AUC with a 90% confidence level.
# Findings

- This plot shows the density function of the loan values after applying winsorization.
<div align="center">
  <img src="https://github.com/dzima22/credit-risk-project-/blob/main/results/density%20function%20after%20winsorization.jpg" alt="" width="600"/>
</div>

- This plot  present  `ROC (Receiver Operating Characteristic) curve` for the train dataset. `AUC` = 0,52
<div align="center">
  <img src="https://github.com/dzima22/credit-risk-project-/blob/main/results/Roc%20curve%20for%20training%20dataset.jpg" alt=""/>
</div>

- This plot  present ` ROC (Receiver Operating Characteristic) curve` for the test dataset. `AUC`=0,5
<div align="center">
  <img src="https://github.com/dzima22/credit-risk-project-/blob/main/results/ROC%20curve%20for%20test%20dataset.jpg" alt=""/>
</div>

