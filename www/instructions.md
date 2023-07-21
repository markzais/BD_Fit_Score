---
title: "instructions"
author: "Mark Zais"
date: "June 8, 2023"
output: html_document
---

## i3 Pipeline P-Win Model
The i3 P-Win Model is a tool designed to inform prioritization of business development resources. The tool incorporates logistic regression to predict the win or loss outcome of a proposal bid. It also provides a confidence level for each prediction. i3 P-Win is built using <a href="http://www.r-project.org/" target="_blank">R</a> and <a href="http://shiny.rstudio.com/" target="_blank">Shiny</a>. The regression component uses a linear programming API called <a href="https://search.r-project.org/CRAN/refmans/parsnip/html/logistic_reg.html" target="_blank">logistic_reg</a> to fit the model.  

### Model Instructions
1. Download the CSV template (*Input_Data_Template.csv*) for required fields and formatting. See **Glossary** in the sidebar menu for data field descriptions.  <span style="color:red">**Do not delete any template columns!** </span>  
2. Import the i3 Pipeline raw data (CSV file) using the **Import** button.
3. Train the i3 Pipeline data with a logistics regression model using the **Train** button.  
4. Adjust the parameters and input values for a new opportunity.  
5. Click the **Load Parameters** button to set the contraints and parameters for solving.  The **Run** button will appear when complete.  
6. Click the **Run** button to begin making predictions. When finished, a message box with the  individual prediction will appear in the Status Report box.
7. Click the **Reset Model** button before subsequent parameter loads. 
6. View glossary and explanation of statistics from the sidebar menu.

