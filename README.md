# FluoroLogger README

## What is the FluoroLogger?
The Fluorologger Shiny app is an R based graphic user interface that allows for an efficient approach to both visualize and normalize fluorolog data (Winston et al. 2019; R Core Team 2019). Output files from the Fluorolog (.dat format) are input into the application. Data are gathered, categorized, and put into a usable format. Negative controls and replicates of Fluorolog scans are chosen from the entered data and are visualized using an interactive plot. With user input, data are normalized according methods described in Millwood et al. (2003). This process can be repeated several times for various configurations of negative controls and replicates. Normalized data are conveniently merged as chosen by the user resulting in a final exportable table.

## References
<i> 
Millwood, R.J., Halfhill, M.D., Harkins, D., Rusotti, R. and Stewart, C.N. Jr (2003) Instrumentation and methodology for quantifying GFP fluorescence in intact plant organs. Biotechniques, 34, 638– 643.
<br>
R Core Team (2019). R: A language and environment for statistical computing. R Foundation for
Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.
<br>
Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2019). shiny: Web Application
Framework for R. R package version 1.3.0. https://CRAN.R-project.org/package=shiny
</i>

### Information Page
For more information, see the information page for the app at https://jaredbrabazon.github.io/FluoroLogger/