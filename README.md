# Overview

This project replicates the findings of Holden (2016), which examines the impact of textbook funding on school-level test performance in California schools using a regression discontinuity (RD) design. The study focuses on the discontinuity in API scores to evaluate the effects of additional textbook funding on student performance, particularly in elementary, middle, and high schools.

# Structure
Data: The project uses datasets located in the Data/ directory. Ensure the data files (CA_schools_es.dta and CA_schools_ms.dta) are correctly placed in this folder before running the scripts.
Log Files: All logs are stored in proj_log.log.
Output Files: Tables and graphs are generated and saved in the project directory.
Tables: 644_tables.doc, 1.doc, and 6.doc
Figures: F1, F2, F3, and combined graphs for various proficiency levels.
Dependencies
Packages: This project requires the eclplot and asdoc packages.
Install eclplot with ssc install eclplot
Install asdoc using net install asdoc, from(http://fintechprofessor.com) replace
Graph Settings: The project uses the "Helvetica" font and the "stcolor" color scheme for graph outputs.
Instructions
Setup: Change the working directory to your project path:
cd ""
Run the Script: Execute the .do file using Stata to perform the analysis. The script includes data preparation, regression analysis, and graphical presentation of results.

# Analysis:

Elementary Schools: The script evaluates the API score cutoffs for elementary schools and assesses the impact on various test scores and performance metrics.
Middle and High Schools: It similarly processes data for middle and high schools, albeit with a focus on summary statistics and basic regressions.
Outputs: Review the generated tables and figures for the results of the regression analyses and visual representations of the data.

# Key Points
The project restricts regression analysis to observations within a 200 API point window around the cutoff for funding.
Normalization of test scores is conducted to standardize the results.
The RD design allows for a clear analysis of the causal impact of textbook funding on student performance.
Notes
The project includes a falsification index and lagged test scores to detect potential biases or spurious correlations.
Ensure that all dependent variables and interactions are correctly specified for accurate regression outputs.
Contact
For any questions or clarifications regarding this project, please contact Eesha Narayana Iyer at eesha@umd.edu.
