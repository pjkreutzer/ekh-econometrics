********************************************************************************
*																			   *
* SUMMARY OF COMMANDS SEEN IN THE LABS (AND A FEW MORE THAT MIGHT BE USEFUL)   *
* ROUGHLY GROUPED BY FUNCTIONALITY 										   	   *
* MAKE SURE TO ADAPT THE PATH TO YOUR WORKING DIRECTORY						   *
* AND MAKE SURE TO CHANGE THE NAME OF THE VARIABLES IN THE COMMANDS            *
*																			   *
********************************************************************************


/*------------------------------------------------------------------------------
THE COMMANDS IN THIS DO FILE SHOULD BE CONSIDERED SINGULARLY, NAMELY, COPY AND
PASTE THE COMMANDS THAT YOU NEED LINE BY LINE UNLESS AT THE END OF THE LINE 
YOU FIND /// WHICH MEANS THAT THE COMMAND IS CONTINUING IN THE NEXT LINE
------------------------------------------------------------------------------*/


/*------------------------------------------------------------------------------
ALWAY REMEMBER TO PAY EXTRA ATTENTION TO: 
- MISSING (WHICH IN STATA ARE CONSIDERED AS INFINITE)
- OUTLIERS
- STRANGE VALUES (E.G. NEGATIVE AGES)
- ZERO (ESPECIALLY WHEN WORKING WITH LOGARITHMS)

- MISSING VALUES WHEN THE VARIABLE IS A STRING ARE OFTEN REFERRED TO AS ""
------------------------------------------------------------------------------*/


/*
GOOD SUPPORTING MATERIAL:
https://stats.idre.ucla.edu/stata/webbooks/reg/
*/





*****************************
* SETTING UP STATA AND DATA *
*****************************

* CLEAR EVERYTHING
clear all

* CHANGE WORKING DIRECTORY
cd "C:\...."

* LOAD DATA
use "lab6_2019_income.dta", clear

* SAVE DATASET
save "lab6_2019_income.dta", replace 

compress /*HELPS IN SAVING MEMORY ON HARD DRIVE (USEFUL WHEN USING VERY LARGE DATASETS)*/

* OPEN LOG FILE
log using "lab_1.log", replace 

* CLOSE LOG FILE
log close

* IF YOU WANT TO IMPORT DATA FROM EXCEL (REMEMBER THAT YOU CAN ALWAYS COPY AND PASTE)
import excel "C:\....\excelfilename.xls", firstrow clear

* TO CHANGE THE FOLDER WHERE YOU INSTALL NEW COMMANDS
sysdir set PLUS "C:\...."

* TO INSTALL A NEW COMMAND
ssc install mplotoffset
ssc install outreg2
ssc install coefplot





*********************************
* EXPLORE DATASET AND VARIABLES *
*********************************

describe

label list

inspect x

list x
list x in 36/44

browse 
browse x
browse if x=="abc" & z==4
br x if z==1

sort x z w

gsort x -z





***********************************************
* DESCRIBE VARIABLES - DESCRIPTIVE STATISTICS *
***********************************************

tab x
tab x z
tab x z, row
tab x z, column
tab x z, column nofreq
tab x z, row nofreq

codebook z
codebook x

sum
sum x z
sum x, detail

tab x, sum(z)

centile x, centile(0 (5) 100)
centile x, centile(0 (1) 100)


table x, contents(mean z)
table x, contents(median z)
table x, contents(max z)
table x, contents(min z)
table x, contents(sd z)

tab x, summarize(w)
tab z, summarize(w)





*********************
* DATA MANIPULATION *
*********************

rename x new_x
 
drop x /*THIS DROPS THE VARIABLE (COLUMN IN THE DATASET)*/

drop if x==. /*THIS DROPS ALL THE OBSERVATIONS (ROWS IN THE TABLE) FOR WHICH x IS EQUAL TO MISSING*/
drop if w=="" /*IN THIS EXAMPLE w IS A STRING (E.G. THE NAME OF THE INDIVIDUAL) AND WITH THIS COMMAND YOU DROP EVERY OBSERVATION FOR WHICH w IS EMPTY*/

generate z = 1 if x<=9
replace z = 2 if x>=10 & x<=12
replace z = 3 if x>12 & x!=.
replace z = 4 if x==.

label define label_for_z 1 "Category one" 2 "Category two" 3 "Category three" 4 "Category four"
label values z label_for_z

label variable x "This is variable x"

gen flag=1 if x==.

bysort x: egen w=mean(z)

egen xmean = mean(x) 	
egen xysum = sum(xy)			

gen xy = x*y	

destring x_string, replace force
destring x_string, generate(x_number)

encode x, gen(z)

bysort x: gen z=_N /*_N GIVES YOU THE TOTAL NUMBER OF OBSERVATIONS WITHIN EACH GROUP IDENTIFIED WITH bysort*/
bysort x: gen z=_n /*_n GIVES A NUMBER THAT INCREASES AS YOU GO FROM ONE OBSERVATION TO THE NEXT WITHIN THE GROUP IDENTIFIED WITH bysort*/

display 2062407/48 /*TO SEE THE RESULT OF SIMPLE CALCULATIONS*/		





*****************
* SIMPLE GRAPHS *
*****************

histogram x
histogram z, percent normal bin(30) /*WITH bin(#) YOU DEFINE HOW MANY BARS YOU WANT*/
histogram z, percent normal width(30) /*WITH width(#) YOU DEFINE HOW LARGE YOUR BARS SHOULD BE*/
histogram x if x<10, percent normal bin(10)

graph matrix x z w 





*****************
* DATA ANALYSIS *
*****************

corr x z
corr x z, covariance

reg y x w z 
reg y x w z if x<10 & z>2
reg y x w i.z /*NOW z WILL BE CONSIDERED AS A CATEGORICAL VARIABLE*/
reg y x w i.z, base /*THE base OPTION WILL SHOW THE REFERENCE CATEGORIES IN THE REGRESSION OUTPUT WHEN WORKING WITH CATEGORICAL VARIABLES*/
reg y x i.w##i.z /*DUMMY-CATEGORICAL INTERACTION*/
reg y x i.w#c.z /*DUMMY-CONTINUOUS INTERACTION WITH SHARED INTERCEPT (c. IN FRONT OF z IS TELLING STATA TO CONSIDER z AS A CONTINUOS VARIABLE)*/
reg y x i.w##c.z /*DUMMY-CONTINUOS INTERACTION WITH DIFFERENT INTERCEPTS*/
reg y x i.w z i.w#c.z /*ALTERNATIVE NOTATION FOR DUMMY-CONTINUOS INTERACTION WITH DIFFERENT INTERCEPTS*/
reg y i.w##c.z i.w##c.z2 /*DUMMY-QUADRATIC INTERACTION WITH DIFFERENT INTERCEPTS*/

predict e, resi /*GENERATE A VARIABLE EQUAL TO THE RESIDUALS*/
predict yhat /*GENERATE A VARIABLE EQUAL TO THE PREDICTED VALUES*/





**********************
* HYPOTHESIS TESTING *
**********************
 
test x==0

test x==z

test x==1

test x z w

test 2.z==3.z /*WHEN z IS A CATEGORICAL VARIABLE AND YOU WANT TO TEST IF THE SECOND CATEGORY IS DIFFERENT FROM THE THIRD CATEGORY*/




**********************
* ASSUMPTIONS CHECKS *
**********************

* TEST FOR NORMALITY IN THE RESIDUALS
hist e, norm percent bin (25)

qnorm e /*THE DOTS SHOULD LIE ON THE STRAIGHT LINE*/

sktest e, noadjust

jb6 e

* USE LOG TO TRY TO CORRECT FOR NON NORMALITY IN RESIDUALS
gen lncom = ln(combined)
gen lnweight = ln(weight)



* HETEROSKEDASTICITY TESTS
estat hettest /*BREUSCH-PAGAN TEST*/
estat imtest, white /*WHITE'S TEST*/

* USE THE vce(robust) OPTION WHEN RUNNING A REGRESSION TO TRY TO CORRECT FOR HETEROSKEDASTICITY
reg y x w z, vce(robust)



* MULTICOLLINEARITY TEST
vif 





**********************
* PRESENTING RESULTS *
**********************

***********************
* TABLES WITH outreg2 *
***********************
ssc install outreg2 /*YOU NEED TO INSTALL THE COMMAND BEFORE USING IT*/

reg y x if x==1 & z==0, vce(robust) baselevels
outreg2 using results.doc, replace ctitle(Model A)  /*CREATES A WORD FILE WITH THE FORMATTED TABLE*/
reg y x i.w i.z if x==1 & z==0, vce(robust) baselevels
outreg2 using results.doc, append ctitle(Model B) /*APPENDS THE RESULTS OF THIS SECOND REGRESSION IN THE PREVIOUS TABLE (CREATES A NEW COLUMN IN THE TABLE)*/
reg y x i.w##i.z if x==1 & z==0, vce(robust) baselevels
outreg2 using results.doc, append ctitle(Model C) 

/*
YOU WILL FIND THE FILE results.doc IN YOU WORKING DIRECTORY

YOU CAN APPEND AS MANY MODELS AS YOU WANT

YOU CAN ALSO CREATE AN EXCEL FILE INSTEAD OF A WORD FILE BY SUBSTITUTING using results.doc WITH using results.xls

USEFUL OPTIONS:
- ctitle: CHANGES THE TITLE OF THE COLUMN
- label: USES VARIABLE LABELS IN THE TABLE
- stats(coef ci): THE DEFAULT IS stats(coef se) MEANING THAT BELOW THE COEFFICIENT YOU WILL FIND THE STANDARD ERROR. USE stats(coef ci) IF YOU WANT THE CONFIDENCE INTERVAL INSTEAD OF THE STANDARD ERROR
- addstat(adjusted R squared, `e(r2_a)'): THIS ADDS THE ADJUSTED R2 AT THE BOTTOM OF THE TABLE. AS DEFAULT YOU ONLY GET NUMBER OF OBSERVATIONS AND R2.
- nose: WITH THIS OPTION YOU WILL NOT SEE THE STANDARD ERRORS IN THE TABLE
- noaster: WITH THIS OPTION YOU WILL NOT SEE THE STARS NEXT TO THE COEFFICIENTS
- baselevels: SHOWS THE BASELEVELS FOR CATEGORICAL VARIABLES

USE help outreg2 FOR MANY MORE OPTIONS
*/




*****************************
* TABLES WITH eststo/esttab *
*****************************

eststo clear /*THIS GETS RID OFF PREVIOUSLY SAVE ESTIMATES*/

reg y x
eststo m1
reg y x z 
eststo m2
reg y x z w
eststo m3


* NOTICE THE /// AT THE END OF THE LINE
* IT MEANS THAT YOU HAVE TO SELECT ALL THESE LINES TOGETHER AND THEN EXECUTE (CTR+D)
* IF YOU DO NOT WANT TO RUN ONE OF THE OPTIONS SHOWN BELOW JUST DELETE THE LINE
*--------------------------START OF THE COMMAND--------------------------------*
esttab m1 m2 m3 using results1.rtf, replace ///
label /*USE VARIABLE LABEL RATHER THAN THE NAME*/ ///
not /*DO NOT REPORT THE t STATISTICS UNDER THE COEFFICIENT*/ ///
nonumbers /*DO NOT SHOW THE NUMBER OF THE COLUMN*/ ///
drop(z w) /*THE VARIABLES WITHIN () WILL NOT BE SHOWN IN THE TABLE*/ ///
b(%5.3f) /*REPORT THE COEFFICIENTS WITH 3 DECIMALS. b(%5.2f) WILL REPORT THE COEFFICIENT WITH 2 DECIMALS*/ ///
ci(%5.3f) /*REPORT THE CONFIDENCE INTERVAL WITH 3 DECIMALS UNDER THE COEFFICIENT*/ ///
coeflabels( /*USE THE VARIABLE LABELS LISTED BELOW*/ ///
x "This is variable x" ///
z "This is variable z" ///
0.w "This is the zero category of the categorical variable w" ///
1.w "This is the first category of the categorical variable w" ///
2.w "This is the second category of the categorical variable w" ///
) ///
mtitles("model 1" "model 2" "model 3") /*THIS SPECIFIED THE HEADER OF EACH COLUMN*/ ///
/*nomtitles*/ /*IF YOU DO NOT WANT THE HEADER OF THE COLUMN*////
ti("Table 1: Effect of age on income - OLS with robust standard error") /*TITLE OF THE TABLE*/ ///
ar2 /*REPORTS THE ADJUSTED R2 AT THE BOTTOM OF THE TABLE*/ ///
addnote("Models are further controlled for ... - This analysis refers to people with monthly income lower than 1000$")
*---------------------------END OF THE COMMAND---------------------------------*

help esttab /*FOR MANY MORE OPTIONS*/





**********************
* DESCRIPTIVE TABLES *
**********************
* CREATE DESCRIPTIVE TABLES (THE EXAMPLE BELOW REFERS TO THE DATASET USED IN ASSIGNMENT 2)
* LET'S SAY THAT YOU WANT TO REPORT mean, standard deviation, min, AND max OF SOME VARIABLES
estpost tabstat wage iq educ age, column(statistics) s(mean sd min max N)
eststo savetabstat
esttab savetabstat using des.rtf, cell("mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2)) count") nonumber nomtitle noobs replace

* IF YOU WANT TO REPORT IT SEPERATELY FOR MARRIED AND NON MARRIED YOU CAN DO THIS
estpost tabstat wage iq educ age if married==1, column(statistics) s(mean sd min max N)
eststo savetabstat_marr
estpost tabstat wage iq educ age if married==0, column(statistics) s(mean sd min max N)
eststo savetabstat_nonmarr
esttab savetabstat_marr savetabstat_nonmarr using des.rtf, cell("mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2)) count") nonumber mtitles("Married" "Non-married") noobs replace
* OR YOU CAN RUN THIS TO HAVE THE NUMBER OF OBSERVATIONS AT THE BOTTOM
esttab savetabstat_marr savetabstat_nonmarr using des.rtf, cell("mean(fmt(2)) sd(fmt(2)) min(fmt(2)) max(fmt(2))") nonumber mtitles("Married" "Non-married") replace

* REMEMBER THAT YOU CAN ALWAYS COPY ANY STATA TABLE AND PASTE IS IN WORD OR EXCEL AND EDIT IT THERE
* YOU CAN RUN THIS
tabstat wage iq educ age, column(statistics) s(mean sd min max N)
* THEN YOU GO IN THE OUTPUT WINDOW, YOU SELECT THE TABLE, RIGHT CLICK, SELECT Copy Table





**********
* GRAPHS *
**********
* IN THIS EXAMPLE I USE THE DATASET ON INCOME
use "C:\Users\en8215de\OneDrive - Lund University\EKHM65_Econometrics1\2019\lab6_2019\lab6_2019_income.dta", clear
gen x=938*age
bysort age: egen meaninc=mean(inctot)

*--------------------------START OF THE COMMAND--------------------------------*
twoway 	(scatter meaninc age, ///
				title("Regression", size(medium) color(black)) /*ADD TITLE TO THE GRAPH, SPECIFY THE OPTION size AND color*/ ///
				ytitle("Mean income", size(medium)) /*SPECIFY TITLE FOR Y AXIS*/ ///
				xtitle("Age") /*SPECIFY TITLE FOR X AXIS*/ ///
				xscale(range(18 65)) /*CHANGE THE SCALE OF X AXIS*/ ///
				xlab(18 25 35 45 55 65, labcolor(red)) /*CHANGE THE LABELS OF X AXIS*/ ///
				xline(52) xline(65, lcolor(blue)) /*ADD REFERENCE VERTICAL LINE(S)*/ ///
				yline(48000, lcolor(orange) lwidth(thick) lpattern(dash)) /*ADD REFERENCE HORIZONTAL LINE(S)*/ ///
				msymbol(T) /*CHANGE THE MARKER SYMBOL USED IN THE SCATTER PLOT*/ ///
				mcolor(blue)  /*CHANGE THE MARKER COLOR USED IN THE SCATTER PLOT*/ /// 
				msize(medium)  /*CHANGE THE MARKER SIZE USED IN THE SCATTER PLOT*/ ///
				mlcolor(black)  /*CHANGE THE MARKER BORDER COLOR USED IN THE SCATTER PLOT*/ ///
				) ///
		(line x age, ///
				xscale(range(18 65)) ///
				lcolor(green) ///
				lwidth(medium) ///
				lpattern(longdash) ///
				), ///
graphregion(lcolor(white)) /*CHANGE THE BORDER COLOR OF THE GRAPH REGION*/ ///
graphregion(fcolor(white)) /*CHANGE THE FILLING COLOR OF THE GRAPH REGION*/ ///
plotregion(lcolor(white)) /*CHANGE THE BORDER COLOR OF THE PLOT REGION (WHICH IS NOT THE SAME AS THE GRAPH REGION)*/ /// 
plotregion(fcolor(ltblue)) /*CHANGE THE FILLING COLOR OF THE PLOT REGION*/ ///
/*legend(off)*/ /*IF YOU DON'T WANT THE LEGEND*/ ///
legend( ///
label(1 "dots") label(2 "line") ///
rows(2) /*THIS SPECIFIES THAT THE LEGEND SHOULD BE ON TWO ROWS. TRY TO CHANGE IT WITH col(2)*/ ///
ring(0) /*THIS MEANS THAT YOU WANT THE LEGEND INSIDE THE PLOT REGION*/ ///
position(5) /*THIS SPECIFY THE POSITION IN WHICH YOU WANT THE LEGEND. LOOK HERE TO SEE THE POSSIBLE POSITIONS AND WHAT 5 MEANS: help clockposstyle*/ ///
) ///
name(ugly_graph, replace)
*---------------------------END OF THE COMMAND---------------------------------*


help textsizestyle /*THIS SHOWS YOU THE OPTIONS YOU HAVE IN TERMS OF TEXT SIZE*/
help colorstyle /*THIS SHOWS YOU THE OPTIONS YOU HAVE IN TERMS COLORS*/


graph export ugly_graph.png, replace 
graph export ugly_graph.emf, replace /*.emf EXTENSION ALLOWS YOU TO RE FORMAT THE GRAPH WITHOUT LOSING QUALITY*/




***************************
* REGRESSION OUTPUT GRAPH *
***************************

* COEFPLOT IS USEFUL TO SHOW THE COEFFICIENT AND CONFIDENCE INTERVAL THAT YOU GOT FROM THE REGRESSION IN A GRAPHICAL WAY
* SINCE YOU WANT TO HAVE A CLEAR GRAPH WITHOUT TOO MUCH GOING ON, IT MIGHT BE A GOOD IDEA TO SHOW ONLY THE MAIN VARIABLE OF INTEREST
* IT IS PARTICULARLY HELPFUL WHEN YOU HAVE CATEGORICAL VARIABLES

* AS AN EXAMPLE I WILL USE THE INCOME DATA FROM LAB 6

* LET'S SAY THAT YOU ARE PARTICULARLY INTERESTED IN THE ASSOCIATION OF RACE ON INCOME CONTROLLING FOR OTHER COVARIATES
reg inctot i.race_cat age i.marst i.edu_cat i.female i.sector if employed==1 & income_flag==0
coefplot /*AS MENTIONED ABOVE IT MIGHT NOT BE SO EXPLICATIVE AND CLEAR TO HAVE ALL THE VARIABLES AT ONCE IN THE GRAPH*/

* LET'S PLOT ONLY THE EFFECT OF RACE
*--------------------------START OF THE COMMAND--------------------------------*
coefplot,  ///
	keep(*.race_cat) /*WITH keep YOU SPECIFY THE VARIABLES THAT YOU WANT TO BE PLOTTED. (WITH drop YOU WOULD SPECIFY THE VARIABLES THAT YOU DO NOT WANT TO BE PLOTTED. *.race_cat IS A NOTATION TO SAY "PLOT ALL THE CATEGORIES OF race_cat" */ ///
	yline(0, lcolor(orange) lpattern(dash)) /*SAME AS ABOVE IN twoway GRAPH*/ ///
	msymbol(d) /*SAME AS ABOVE IN twoway GRAPH*/ ///
	mlcolor(black) /*SAME AS ABOVE IN twoway GRAPH*/ ///
	mcolor(black) /*SAME AS ABOVE IN twoway GRAPH*/ ///
	ciopts(lwidth(thick) lcolor(blue*0.4)) /*HERE YOU SPECIFY THE OPTIONS REGARDING THE CONFIDENCE INTERVALS. blue*0.4 IS THE NOTATION TO SPECIFY THAT YOU WANT THE COLOR blue TO HAVE SOME TRANSPARENCY*/ ///
base /*PLOT ALSO THE REFERENCE CATEGORY*/ ///
vertical /*WITH vertical YOU SPECIFY THAT YOU WANT THE CONFIDENCE INTERVALS TO BE PLOTTED VERTICALLY (THE race_cat CATEGORIES WILL BE ON THE x AXIS) (horizontal IS THE DEFAULT OPTION AND WOULD PUT THE race_cat CATEGORIES ON THE y AXIS)*/ ///
graphregion(fcolor(white)) /*SAME AS ABOVE IN twoway GRAPH*/ ///
plotregion(fcolor(white)) /*SAME AS ABOVE IN twoway GRAPH*/ ///
xlab(1 "White" 2 "African American" 3 "Native American" 4 "Asian / Pacific islander" 5 "Two or more races", angle(45)) /*IN THIS CASE, IF YOU DO NOT angle THE LABELS THEY WILL OVERLAP AND BE UNREADABLE*/ ///
ytitle("Income") /*SAME AS ABOVE IN twoway GRAPH*/ ///
xtitle("Race category") /*SAME AS ABOVE IN twoway GRAPH*/ 
*---------------------------END OF THE COMMAND---------------------------------*





* LET'S SAY THAT YOU ARE PARTICULARLY INTERESTED IN THE ASSOCIATION OF RACE ON INCOME CONTROLLING FOR OTHER COVARIATES
* AND LET'S SAY THAT YOU WANT TO PLOT THE COEFFICIENTS OF TWO DIFFERENT MODELS AT THE SAME TIME
* IN THE FIRST MODEL YOU ONLY INCLUDE race_cat WITHOUT CONTROLS, AND IN THE SECOND MODEL YOU INCLUDE ALL THE COVARIATES

* THEN, YOU RUN THE TWO MODELS AND YOU SAVE THE ESTIMATES LIKE THIS:
reg inctot i.race_cat if employed==1 & income_flag==0
eststo m1
reg inctot i.race_cat age i.marst i.edu_cat i.female i.sector if employed==1 & income_flag==0
eststo m2

* LET'S PLOT THE EFFECT OF RACE FOR BOTH MODELS ON THE SAME GRAPH
* IN THIS WAY YOU CAN CLEARLY SEE THE ATTENUATION IN THE EFFECT OF race_cat WHEN YOU INTRODUCE OTHER CONTROLS
*--------------------------START OF THE COMMAND--------------------------------*
coefplot ///
( ///
m1, /*NOW WE SPECIFY ALL THE OPTIONS REGARDING THE FIRST MODEL*/ ///
	keep(*.race_cat) /*WITH keep YOU SPECIFY THE VARIABLES THAT YOU WANT TO BE PLOTTED. (WITH drop YOU WOULD SPECIFY THE VARIABLES THAT YOU DO NOT WANT TO BE PLOTTED. *.race_cat IS A NOTATION TO SAY "PLOT ALL THE CATEGORIES OF race_cat" */ ///
	msymbol(d) /*SAME AS ABOVE IN twoway GRAPH*/ ///
	mlcolor(black) /*SAME AS ABOVE IN twoway GRAPH*/ ///
	mcolor(white) /*SAME AS ABOVE IN twoway GRAPH*/ ///
	msize(small) ///
	ciopts(lwidth(thick) lcolor(green*0.4)) /*HERE YOU SPECIFY THE OPTIONS REGARDING THE CONFIDENCE INTERVALS. blue*0.4 IS THE NOTATION TO SPECIFY THAT YOU WANT THE COLOR blue TO HAVE SOME TRANSPARENCY*/ ///
) ///
( ///
m2, /*NOW WE SPECIFY ALL THE OPTIONS REGARDING THE SECOND MODEL*/ ///
	keep(*.race_cat) /*WITH keep YOU SPECIFY THE VARIABLES THAT YOU WANT TO BE PLOTTED. (WITH drop YOU WOULD SPECIFY THE VARIABLES THAT YOU DO NOT WANT TO BE PLOTTED. *.race_cat IS A NOTATION TO SAY "PLOT ALL THE CATEGORIES OF race_cat" */ ///
	msymbol(d) /*SAME AS ABOVE IN twoway GRAPH*/ ///
	mlcolor(black) /*SAME AS ABOVE IN twoway GRAPH*/ ///
	mcolor(black) /*SAME AS ABOVE IN twoway GRAPH*/ ///
	msize(small) ///
	ciopts(lwidth(thick) lcolor(blue*0.4)) /*HERE YOU SPECIFY THE OPTIONS REGARDING THE CONFIDENCE INTERVALS. blue*0.4 IS THE NOTATION TO SPECIFY THAT YOU WANT THE COLOR blue TO HAVE SOME TRANSPARENCY*/ ///
), ///
yline(0, lcolor(orange) lpattern(dash)) /*SAME AS ABOVE IN twoway GRAPH*/ ///
base /*PLOT ALSO THE REFERENCE CATEGORY*/ ///
vertical /*WITH vertical YOU SPECIFY THAT YOU WANT THE CONFIDENCE INTERVALS TO BE PLOTTED VERTICALLY (THE race_cat CATEGORIES WILL BE ON THE x AXIS) (horizontal IS THE DEFAULT OPTION AND WOULD PUT THE race_cat CATEGORIES ON THE y AXIS)*/ ///
graphregion(fcolor(white)) /*SAME AS ABOVE IN twoway GRAPH*/ ///
plotregion(fcolor(white)) /*SAME AS ABOVE IN twoway GRAPH*/ ///
xlab(1 "White" 2 "African American" 3 "Native American" 4 "Asian / Pacific islander" 5 "Two or more races", angle(45)) /*IN THIS CASE, IF YOU DO NOT angle THE LABELS THEY WILL OVERLAP AND BE UNREADABLE*/ ///
ytitle("Income") /*SAME AS ABOVE IN twoway GRAPH*/ ///
xtitle("Race category") /*SAME AS ABOVE IN twoway GRAPH*/ ///
legend(order(2 "No controls" 1 "95% CI" 4 "Adjusted for covariates" 3 "95% CI"))
*---------------------------END OF THE COMMAND---------------------------------*
