*****************************************************
*	EKHM62: COMPUTER LAB 3 - REGRESSION ANALYSIS 	*
*****************************************************

cd "path to your folder with the files that you downloaded from live@lund"
use "lab3_2019_data.dta", clear





****REVIEW OF PREVIOUS LAB****
/* 

SO FAR WE HAVE SEEN:
	- LOADING DATA IN STATA
	- LOG FILES
	- GETTING TO KNOW THE DATA
		- describe, sum, tab (ALSO COMBINED WITH THE OPTION sum), codebook, browse, sort, gsort
	- GETTING TO KNOW THE DATA WITH GRAPHS
		- hist, twoway
	- IF STATEMENTS
		<, >, <=, >=, ==, !=
		&, |
	- LABELS
		- label variables
		- label values
WE NEED TO PAY ATTENTION TO:
	- MISSING DATA
	- OUTLIERS
	- WEIRD VALUES (e.g. NEGATIVE AGES)

HELP FILES!!!

*/







****SECTION I. GETTING STARTED WITH THE NEW DATASET****


* IF WE WANT TO BROWSE A SPECIFIC VALUE OF A STRING VARIABLE, WE NEED TO USE QUOTATION MARKS:
browse if type=="suv"
browse if type!="suv"

* WE CAN ALSO IMPOSE MULTIPLE CONDITIONS WHEN WE BROWSE OR USE OTHER COMMANDS. FOR EXAMPLE:
browse if type == "sedan" & cylinder==4

* USING THE 'sort' COMMAND, FIND OUT WHAT IS THE LOWEST HORSEPOWER.

* WHAT IS ANOTHER COMMAND FOR FINDING OUT THE LOWEST HORSEPOWER? (THERE ARE SEVERAL POSSIBILITIES) 

* WE MENTIONED LAST TIME THAT WE CAN ALSO EXAMINE HOW THE DATA ARE STORED AND THEIR LABELS USING THE 'describe' COMMAND.
describe




********************************************************************************
* WE SEE THAT THERE ARE VARIABLES MISSING LABELS.
* USE THE 'label' COMMAND TO APPLY LABELS TO 'cylinder' AND 'type'.
*		'cylinder' IS THE NUMBER OF CYLINDERS IN THE CAR.
*		'type' IS THE TYPE OF VEHICLE.
*
* WE DON'T WANT TO HAVE TO TYPE _ EVERYTIME WE TYPE OUT 'horse_power'
* RENAME THE VARIABLE 'horse_power' TO 'hpower'
********************************************************************************

rename horse_power hpower






********************************SUMMARY STATISTICS******************************




* IF YOU ARE EVER UNSURE ABOUT THE EXACT SYNTAX, YOU CAN ALWAYS USE THE 'help' COMMAND

* NOW THAT WE HAVE LOOKED AT THE STRUCTURE OF THE DATA, LET'S LOOK AT SOME SUMMARY STATISTICS
sum

* WE CAN ALSO LOOK ONLY AT CERTAIN VARIABLES BY USING A VARIABLE LIST AFTER THE COMMAND
sum cylinder hpower fuel

* IF WE USE THE OPTION 'detail' WE CAN GET A BIT MORE INFORMATION, LIKE THE MEDIAN AND INTERQUARTILE RANGE, AMONG OTHER THINGS.
* THIS COMMAND CAN BE USEFUL FOR US TO QUICKLY LOOK FOR ANY OUTLIERS
sum liter length, detail

* IF SOME VARIABLES HAVE 0 OBSERVATIONS, IT IS PROBABLY BECAUSE THEY ARE STRING VARIABLES.
sum type

* WE CAN ALSO LOOK AT CHARACTERISTICS WITHIN GROUPS
* WHICH CAR TYPE HAS THE LONGEST AVG LENGTH?
tab type, sum(length)

* NOTE: YOU CAN ATTACH NUMERIC LABELS TO STRING VARIABLES BY USING THE 'encode' COMMAND.
encode type, gen(types)
codebook types

* NOW 'type' IS A USELESS VARIABLE SO WE CAN GO AHEAD A DROP IT
drop type


* WE HAVE LOOKED AT THE SUMMARY STATISTICS AND DATA STRUCTURE, SO NOW LET'S VISUALIZE OUR DATA.
* WE CAN DO A SIMPLE SCATTER PLOT USING THE 'twoway' COMMAND.
* REMEMBER: THE DEPENDENT VARIABLE (Y) SHOULD COME FIRST IN THE COMMAND.
twoway (scatter hpower liter)

* LET'S SEE HOW THE SCATTER PLOT WOULD LOOK FOR VEHICLES WITH LESS THAN 350 HP
twoway (scatter hpower liter if hpower<350)

*THERE SEEMS TO BE A CLEAR TREND WHETHER OR NOT WE RESTRICT THE SAMPLE.
*WE CAN APPLY A LINEAR FITTED LINE TO THE PLOT TO GET A BETTER IDEA.
twoway (scatter hpower liter if hpower<350) (lfit hpower liter if hpower<350)

*REMEMBER: YOU NEED TO SPECIFY THE SAME RESTRICTIONS FOR THE 'lfit' COMMAND AS YOU DO FOR THE 'scatter' COMMAND.
*THE BELOW EXAMPLE SHOWS YOU WHY:
twoway (scatter hpower liter if hpower<350) (lfit hpower liter if hpower<350) (lfit hpower liter)

* IT TURNS OUT THERE IS AN UPWARD, OR POSITIVE, TREND.
* THE 'graph matrix' COMMAND IS A SIMPLE WAY TO LOOK AT SEVERAL SCATTER PLOTS AT THE SAME TIME.
* THIS COMMAND WORKS BEST WHEN YOU HAVE MORE CONTINOUS VARIABLES
graph matrix hpower fuel liter



* WE CAN USE HISTOGRAMS TO GET AN IDEA OF HOW OUR VARIABLES ARE DISTRIBUTED.
* THE BIN SELECTION IS UP TO YOU.  I CHOSE 15 HERE BECAUSE I THOUGHT IT GIVES A REASONABLE PICTURE OF THE DISTRIBUTION.
* HERE THE DISTRIBUTION IS SLIGHTLY POSITIVELY SKEWED (LONG TAIL TOWARD THE RIGHT)
hist liter, bin(15) norm percent

hist fuel, bin(15) percent norm
* HERE WE SEE AN EXAMPLE OF A (RELATIVELY) NORMAL DISTRIBUTION

corr liter fuel cylinder hpower
*WE CAN DO A QUICK CORRELATION MATRIX TO SEE CORRELATION COEFFICIENTS BETWEEN DIFFERENT VARIABLES


/*
IT IS GOOD PRACTICE TO BEGIN ANY DATA ANALYSIS WITH THESE COMMANDS.
UNDERSTANDING THE DATA STRUCTURE IS ALWAYS IMPORTANT.
SIMPLY USING THE 'browse', 'sum' OR 'scatter' COMMANDS, FOR EXAMPLE, CAN ALERT YOU TO ANY OUTLIERS THAT MAY NEED SOME ATTENTION.

A NOTE ON OUTLIERS.  
I WOULD NOT RECOMMEND DROPPING OBSERVATIONS UNLESS THEY ARE CLEARLY MISTAKES OR IF THEY ARE OUTSIDE THE POPULATION OF INTEREST
ESPECIALLY WHEN WE HAVE SUCH A SMALL SAMPLE SIZE DROPPING OBSERVATIONS CAN BE DETRIMENTAL. 
SOMETIMES SECONDARY RESEARCH WILL BE NECESSARY TO VERIFY YOUR DATA.
*/







*****SECTION II. SIMPLE LINEAR REGRESSION*****





/*
YOU ARE INTERESTED IN UNDERSTANDING THE RELATIONSHIP BETWEEN A VEHICLE'S FUEL EFFICIENCY (COMBINED MILES PER GALLON) AND ITS WEIGHT.
TO DO THIS, WE HAVE DECIDED TO RUN A SIMPLE LINEAR REGRESSION: y_i = a + BX_i + e
OUR VARIABLES ARE: Y_i = COMBINED MILES PER GALLON AND X_i = WEIGHT.
*/


* FIRST, IF WE DON'T HAVE THE VARIABLE WE NEED WE MAY NEED TO GENERATE IT OURSELVES.
* IN THIS CASE, WE NEED TO FIGURE OUT THE COMBINED MILES PER GALLON.
* COMBINED MILES PER GALLON = 1 / ((.45/HIGHWAY MPG) + (.55/CITY MPG))

gen combined = 1/((.55/city) + (.45/high))
label var combined "Avg fuel efficiency"
br city high combined

* WE SHOULD EXAMINE OUR NEW VARIABLE TO SEE THAT IT WAS CORRECTLY DEFINED.
sum combined, detail


* THE VARIABLE SEEMS PROPERLY CALCULATED (THERE ARE NO VALUES THAT SEEM UNREASONABLE).
* IT IS ALWAYS IMPORTANT TO CHECK WHETHER VARIABLES YOU CREATE ARE REASONABLE 

* WE SHOULD ALSO LOOK AT THE TWO VARIABLES OF INTEREST FOR AN IDEA OF THEIR RELATIONSHIP:
corr combined weight
twoway (scatter combined weight) (lfit combined weight)


* IT SEEMS LIKE THERE IS A NEGATIVE ASSOCIATION BETWEEN THE VARIABLES.
* NOW THAT WE HAVE EXAMINED THE DATA, WE WANT TO TRY OUT OUR MODEL.
* REMEMBER THAT WE ARE LOOKING AT CARS NOT PEOPLE (I.E. UNIT OF OBSERVATION = CARS), 
* AND ALL OF YOUR ANALYSIS SHOULD KEEP THIS IN MIND.

* WHAT IS THE UNIT OF MEASURE???? POUNDS AND MPG (MILES PER GALLON)
reg combined weight
predict yhat
twoway (scatter combined weight) (scatter yhat weight)

egen mean=mean(combined)
twoway (scatter combined weight) (line yhat weight)(line mean weight)

/*
OUR COEFFICIENT b= -.00583
LOOK AT THE SIGN FIRST
INTEPRETATION: A 1 UNIT INCREASE IN THE VEHICLES WEIGHT LEADS TO A .0058 UNIT DECREASE IN ITS COMBINED MPG.

100 pounds more --> .6 miles per gallon less

IF WE LOOK AT OUR R^2 VALUE, IT SEEMS THAT OUR MODEL LEAVES MUCH OF Y_i'S VARIANCE UNEXPLAINED (R^2=0.7127). 
THAT IS, 71.27% OF THE VARIANCE IN Y_I IS EXPLAINED BY X_I.


* LETS TAKE A CLOSER LOOK AT THE OUTPUT PROVIDED!

*******ANOVA TABLE******* --> model, residual, and total SUM OF SQUARES, DF, and MEAN SQUARES

* SS = SUM OF SQUARES
* MODEL:
* RESIDUAL:
* TOTAL:

DF = DEGREES OF FREEDOM
TOTAL DF = n-1
MODEL DF = TOTAL NUMBER OF COVARIATES MINUS 1
RESIDUAL DF = TOTAL DF - MODEL DF 

MS = MEAN SQUARES (SS DIVIDED BY THEIR RESPECTIVE DF)

*******OVERALL MODEL FIT*******

* F = F-STATISTIC (CALCULATED AS: MS MODEL / MS RESIDUAL)
* PROB > F = THE P-VALUE ASSOCIATED WITH THE F-STAT.  IT IS USED IN TESTING THE MODEL SIGNIFICANCE. 
* R-SQUARED: proportion of variance in the dependent variable which can be predicted from the independent variables
* ADJ R-SQUARED: 1 – ( (1-R-sq)(N-1 / N – k – 1) ) it is a measure of model fit which takes into account the number of predictors (independent variables) in the model
* ROOT MSE = STANDARD DEVIATION OF THE ERROR TERM (SQUARE ROOT OF THE MEAN SQUARES RESIDUAL)

(F-TEST=617.75; p= 0.0000; I.E. WE REJECT H0 THAT THERE IS NO LINEAR RELATIONSHIP BETWEEN INDEPENDENT AND DEPENDENT VARIABLES).

*******T-TEST*******

OUR COEFFICIENT b= -.00583 IS SIGNIFICANT AT a=0.01
WE REJECT THE NULL HYPOTHESIS THAT b=0

WHAT IS THE P-VALUE TELLING US HERE?
HOW CONFIDENT CAN WE BE THAT WE ARE NOT COMMITTING A TYPE 1 ERROR?
WHAT ABOUT THE CONFIDENCE INTERVALS?

TYPE 1 ERROR: WE REJECT THE NULL HYPOTHESIS WHEN IN REALITY, IT IS TRUE
TYPE 2 ERROR: WE RETAIN THE NULL HYPOTHESIS WHEN IN REALITY, IT IS FALSE







BUT BEFORE WE CAN MAKE ANY CONCLUSIONS WE NEED TO CHECK THE PROPERTIES OF THE MODEL TO BE SURE IT FULFILLS THE OLS ASSUMPTIONS.

YOU'LL RECALL THAT THE OLS ESTIMATOR IS ONLY 'BLUE' (BEST LINEAR UNBIASED ESTIMATE) WHEN CERTAIN ASSUMPTIONS ARE FULFILLED.

ASSUMPTIONS ABOUT 
 - NO AUTOCORRELATION, 
 - INDEPENDENCE OF X'S AND 
 - THE MODEL BEING LINEAR IN ITS PARAMETERS 
ARE TYPICALLY FULFILLED IN CROSS SECTIONAL DATA AND OFTEN DO NOT NEED TO BE FORMALLY TESTED.

IN SIMPLE LINEAR REGRESSION, 
 - MULTICOLLINEARITY 
IS NOT A PROBLEM, SINCE WE ONLY HAVE ONE PREDICTOR (INDEPENDENT VARIABLE).

ONE WAY TO DETERMINE IF OUR ESTIMATE IS BLUE IS BY LOOKING AT THE RESIDUALS AND FITTED VALUES.
*/













/*
****TESTING FOR NORMALITY IN THE RESIDUALS****

WE SHOULD TEST FOR NORMALITY IN THE RESIDUALS. THIS CAN BE DONE MULTIPLE WAYS.

THE 'predict' COMMAND TELLS STATA TO USE THE PARAMETERS WE ESTIMATED TO GENERATE A NEW VARIABLE.
FIRST, STATA PREDICTS THE RESIDUALS (I.E. THE DISTANCE BETWEEN OUR REGRESSION LINE AND OUR DATA POINTS).
*/
predict e, resi

*SECOND, STATA PREDICTS FITTED VALUES OF Y (IN THIS CASE THE COMBINED GAS MILEAGE OF THE VEHICLE).
predict yhat

/*
ONE NECESSARY THOUGH INSUFFICIENT INDICATOR OF NORMALITY IS THAT MOST OBSERVATIONS 
(ABOUT 95%) FALL WITHIN 2 STANDARD DEVIATIONS OF THE MEAN.
THIS IS A CHARACTERISTIC OF A NORMAL DISTRIBUTION (A.K.A. THE 68-95-99.7 RULE)
*/
egen stdevplus= sd(e)
replace stdevplus=stdevplus*2
gen stdevmin=-1*stdevplus
twoway (scatter e yhat) (lfit e yhat) (line stdevmin yhat) (line stdevplus yhat)
/*
HERE OUR GRAPH INDICATES THAT THE SCATTER OF OUR DATA POINTS FALLS INCONSISTENTLY WITHIN THE APPROPRIATE RANGE
IT DOES NOT, HOWEVER, CONFIRM NON-NORMALITY AND FORMAL TESTS ARE REQUIRED
*/

hist e, norm percent bin (25)
/*
THE HISTOGRAM OVERLAYED WITH A NORMAL DISTRIBUTION DOES NOT SEEM TO INDICATE NORMALITY IN THE RESIDUALS.
THERE IS A POSITIVE SKEW PRESENT ALTHOUGH IT IS QUITE CLOSE
*/

qnorm e
/*
IF THE RESIDUALS ARE NORMALLY DISTRIBUTED, THEY SHOULD MORE OR LESS FOLLOW THE STRAIGHT LINE OF THE Q-Q PLOT
IN THIS CASE, THE RESIDUALS DEVIATE SLIGHTLY BIT FROM THE NORMAL DISTRIBUTION AROUND THE TAILS.
*/

sktest e, noadjust
/*
WE DON'T JUST NEED TO USE GRAPHICS TO GAUGE NORMALITY.  WE CAN ALSO USE A FORMAL TEST FOR NORMALITY.
THIS IS A TEST OF THE NULL HYPOTHESIS H0: variable is normally distributed 
HERE WE REJECT THAT THE VARIABLE IS NORMALLY DISTRIBUTED (p=0.000).  
*/


/*
WE CAN ALSO USE THE JARQUE-BERA TEST.  THIS TEST DOES NOT COME WITH STATA AS A DEFAULT, 
BUT SOME USERS HAVE WRITTEN THE PROGRAM TO RUN THE TEST.
YOU CAN FIND A PROGRAM BY USING -FINDIT- , WHICH WILL PROVIDE A DESCRIPTION FOR YOU.
*/
findit jarque-bera 

* IF YOU KNOW THE PROGRAM YOU WANT, YOU CAN SIMPLY USE -SSC INSTALL- TO INSTALL IT.
sysdir set PLUS "your folder"

ssc install jb6

jb6 e

* HERE AGAIN WE REJECT THE NULL HYPOTHESIS OF NORMALITY AT .001









****CORRECTING FOR NON-NORMALITY IN THE RESIDUALS****
/*
AT THIS POINT WE HAVE CONCLUDED THAT THE RESIDUALS ARE NOT NORMALLY DISTRIBUTED.
SIMPLE SOLUTION FOR IMPROVING NON-NORMALLY DISTRIBUTED RESIDUALS: LOGARITHMS!
THIS ALSO MAKES FOR A SIMPLE INTERPRETATION OF THE BETA COEFFICIENTS AS ELASTICITIES WHEN THE VARIABLES ARE CONTINUOUS.
*/
gen lncom = ln(combined)
gen lnweight = ln(weight)

reg lncom lnweight
predict e2, res
predict yhat2
twoway (scatter e2 yhat2) (lfit e2 yhat2)
hist e2, norm bin(10)
* THIS HISTOGRAM LOOKS A BIT CLOSER TO A NORMAL DISTRIBUTION.

sktest e2
jb6 e2
sktest e
jb6 e

/*
AND THE TEST FOR NORMALITY STILL REJECTS H0: NORMAL DISTRIBUTION
THIS IS MUCH CLOSER TO NOT REJECTING THE NULL THAN BEFORE (SEE CHI2 VALUE)
PRIOR TO THE LOG ADJUSTMENT, THE DISTRIBUTION WAS ALSO CLOSE TO NORMALLY DISTRIBUTED

YOU SHOULDN'T ONLY RELY ON TESTS WHEN GRAPHICAL EVIDENCE POINTS IN THE OPPOSITE DIRECTION.
YOU NEED TO USE YOUR JUDGEMENT.
*/


/*
A WORD ABOUT COEFFICIENTS & REAL WORLD SIGNIFICANCE

BEFORE OUR COEFFICIENT WAS b=-.0058 IN THE LINEAR-LINEAR MODEL. 
INTEPRETATION: A 1 UNIT INCREASE IN THE VEHICLES WEIGHT LEADS TO A .0058 UNIT DECREASE IN ITS COMBINED MPG.

THIS MODEL HAS STATISTICAL SIGNIFICANCE, BUT DOES IT HAVE REAL WORLD SIGNIFICANCE?  
IF WE LOOK AT OUR R^2 VALUE, IT SEEMS THAT OUR MODEL LEAVES MUCH OF Y_i'S VARIANCE UNEXPLAINED (R^2=0.7127). 
THAT IS, 71.27% OF THE VARIANCE IN Y_I IS EXPLAINED BY X_I.


OUR NEW COEFFICIENT IS b=-0.9341 IN THE LOG-LINEAR MODEL. 
INTERPRETATION: A 10% INCREASE IN THE WEIGHT OF A VEHICLE IS ASSOCIATED WITH ROUGHLY A 9.34% DECREASE IN ITS COMBINED MPG.
  
IF WE LOOK AT OUR R^2 VALUE, 77.95% OF THE VARIANCE IN Y_I IS EXPLAINED BY X_I.
SINCE WE HAVE MORE DATA AVAILABLE, WE CAN STRIVE TO IMPROVE THE FIT OF OUR MODEL.
*/


drop e* yhat* stdev*
/*
PUTTING AN ASTERISK (*) AFTER SOMETHING WILL TELL STATA DO DROP EVERYTHING THAT BEGINS WITH WHATEVER COMES BEFORE THE ASTERISK.
IN THIS CASE, WHEN WE SAY 'drop yhat*' WE ARE TELLING STATA DO DROP ALL VARIABLES BEGINNING WITH 'yhat'.
USE CAUTIOUSLY!
*/





