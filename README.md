**Purpose** :This project aims to extend an item response tree model (i.e. multiple -response processes model) thereby incorporating the concept of "an ideal point process". To demonstrate the application of the proposed model, the attitude toward welfare state policies in the 2016 European Social Survey was applied. 

**Data**: The 2016 European Social Survey can be downloaded here: [THE EUROPEAN SOCIAL SURVEY](https://www.europeansocialsurvey.org/)

**Code**: 
1. ESS_GUMM.R: creating two functions for cleaning data and fitting the GGUM model by using the “GGUM” package in R.
2. ESS_CZ_Winbugs.R: fitting the GGUM-tree model with Winbugs under an R environment.
3. GGUM-tree.R: using the estimated paramters from the GGUM-tree model to plot the 3-D item response surface plot.
4. Model_CZ_new.txt: the GGUM-tree model for Winbugs.

**References**: 

1. Bockenholt, U. (2012). Modeling multiple response processes in judgment and choice. 
   Psychological Methods, 17, 665-678.
2. Roberts, J. S., Donoghue, J. R., & Laughlin, J. E. (2000). A general item response theory model for unfolding unidimensional polytomous    responses. Applied Psychological Measurement, 24, 3-32.
3. Thissen-Roe, A., & Thissen, D. (2013). A two-decision model for responses to Likert-type items. 
   Journal of Educational and Behavioral      Statistics, 38, 522-547.


