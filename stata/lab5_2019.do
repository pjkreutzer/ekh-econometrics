*****************************
*                           *
*    LAB 5: INTERACTIONS    *
*                           *
*****************************

************SECTION I. LOAD DATA AND PREPARE DATASET AS IN PREVIOUS LAB***************

/*
IN THIS SECTION WE WILL BE USING THE CURRENT POPULATION SURVEY FROM MARCH, 2012 CONDUCTED 
BY THE U.S. CENSUS BUREAU. THIS DATASET IS MUCH LARGER THAN THE LAST ONE WE USED, 
BUT EVERYTHING WE LEARNED BEFORE STILL APPLIES.
THE EXTRACTION INCLUDES ONLY INDIVIDUALS AGE 18 TO 65.
*/
clear all
cd "C:\Users\jo1173an\econometrics i"
use "lab5_2019_income.dta", clear

drop if inctot<0

bysort serial: gen adultsinhh=_N
label var adultsinhh "Number of aduts in household"

gen industryflag= (indly==0)
tab industryflag
drop if industryflag==1
drop industryflag

gen sector=.
replace sector=1 if indly>1 & indly<=500
replace sector=2 if indly>500 & indly<4000
replace sector=3 if indly>4000
label define sector_lbl 1 "primary" 2 "secondary" 3 "tertiary"
label val sector sector_lbl 
label var sector "Job sector categories"
tab sector


gen race_cat=. 
replace race_cat=1 if race==100
replace race_cat=2 if race==200
replace race_cat=3 if race==300
replace race_cat=4 if race>=650 & race<=652
replace race_cat=5 if race>652
label define race_cat_lbl 1 "White" 2 "African American" 3 " Native American" 4 " Asian/Pacific Islander" 5 "two or more races"
label val race_cat race_cat_lbl
label var race_cat "Racial categories"
tab race_cat


gen edu_cat= .
replace edu_cat=1 if educ99<10
replace edu_cat=2 if educ99==10
replace edu_cat=3 if educ99>10 & educ99<15
replace edu_cat=4 if educ99==15
replace edu_cat=5 if educ99==16
replace edu_cat=6 if educ>16 & educ!=. 
label define educ 1 "less than HS diploma" 2 "HS Diploma" 3 "Some college/Associates Degree" 4 "Bachelor's Degree" 5 "Master's Degree" 6 "Ph.D. or Professional", replace
label val edu_cat educ
label var edu_cat "Education categories"
tab edu_cat

tab sex, summarize(inctot)
tab race_cat, summarize(inctot)
tab sector, summarize(inctot)
tab edu_cat, sum(inctot)

gen income_flag= (inctot>200000)

codebook empstat
gen employed= (empstat<14)


gen female=(sex==2)
gen disability= (disabwrk==2)


gen age2= age^2

gen age18to24= (age>=18 & age<26)
gen age26to35= (age>=26 & age<36)
gen age36to45= (age>=36 & age<46)
gen age46to55= (age>=46 & age<56)
gen age56to65= (age>=56 & age<66)

gen age_cat=1 if (age>=18 & age<26)
replace age_cat=2 if (age>=26 & age<36)
replace age_cat=3 if (age>=36 & age<46)
replace age_cat=4 if (age>=46 & age<56)
replace age_cat=5 if (age>=56 & age<66)



************SECTION III. INTERACTION TERMS*********
{
*IT MAY ALSO BE OF INTEREST TO LOOK AT HOW VARIABLES INTERACT WITH ONE ANOTHER.
*IN THIS CASE, WE WANT TO SEE HOW WOMEN COMPARE TO MEN AND TO EACH OTHER ACROSS AND WITHIN RACIAL GROUPS. 

/*
INTERACTION TERMS ARE MULTIPLICATIVE TERMS AND CAN BE DEFINED MANUALLY BY MAKING NEW VARIABLES, BUT STATA ALSO HAS A BUILT-IN COMMAND
FOR RUNNING INTERACTIONS. 
# BETWEEN TWO VARIABLES WILL GIVE ONLY THE INTERACTION TERM
## BETWEEN TWO VARIABLES WILL GIVE BOTH THE INTERACTION TERMS AND THE RESPECTIVE BASE EFFECTS (SO YOU DON'T HAVE TO INCLUDE THEM SEPARATELY).

THE REGRESSION BELOW CAN ALSO BE RUN AS (IN OLDER VERSIONS OF STATA):
xi: reg  inctot age26to35 age36to45 age46to55 age56to65 i.sector i.edu_cat disability i.marst adultsinhh i.race_cat*female if employed==1 & income_flag==0, robust
*/

*DUMMY-CATEGORICAL INTERACTION
reg inctot i.age_cat i.sector i.edu_cat disability i.marst adultsinhh i.race_cat i.female if employed==1 & income_flag==0, robust baselevels
reg inctot i.age_cat i.sector i.edu_cat disability i.marst adultsinhh i.race_cat##i.female if employed==1 & income_flag==0, robust baselevels

*MARGINS AND MARGINSPLOT
margins race_cat#female /*THIS GIVES YOU THE PREDICTED VALUE OF inctot FOR EACH COMBINATION OF female AND race_cat*/
margins race_cat, dydx(female) /*THIS GIVES YOU THE MARGINAL EFFECT ON inctot OF BEING FEMALE IN EACH RACE*/
margins female, dydx(race_cat) /*THIS GIVES YOU THE MARGINAL EFFECT ON inctot OF BEING NOT WHITE IN SEX*/

margins female, at(race_cat=(1(1)5))
marginsplot, xdimension(at(race_cat)) recast(scatter) horizontal

margins race_cat, at(female=(0(1)1))
marginsplot, xdimension(female) recast(scatter) horizontal
/*
sysdir set PLUS "your folder"

ssc install mploffset
*/
mplotoffset, xdimension(at(female)) recast(scatter) offset(0.1)

reg inctot i.race_cat##i.female if employed==1 & income_flag==0, robust baselevels
margins female, at(race_cat=(1(1)5))
marginsplot, xdimension(at(race_cat)) recast(scatter) horizontal

lincom 2.race+1.female+2.race#1.female - (5.race+1.female+5.race#1.female)
lincom 1.race+1.female+1.race#1.female - (2.race+0.female+2.race#0.female)

*DUMMY-CONTINUOUS INTERACTION WITH SHARED INTERCEPT
gen femage= female*age
br female age femage
reg inctot age femage
reg inctot age i.female#c.age
*THIS ASSUMPTION IS VERY DIFFICULT TO SUPPORT, SO IT IS NOT COMMONLY DONE THIS WAY

*DUMMY-CONTINUOUS INTERACTION WITH DIFFERENT INTERCEPTS
reg inctot age i.female femage
*THIS CAN ALSO BE WRITTEN AS (:
reg inctot i.female##c.age, base
*OR
reg inctot age i.female i.female#c.age
*NOTE THAT CONTINUOUS VARIABLES IN INTERACTIONS NEED TO BE PREFIXED WITH c.


*DUMMY-QUADRATIC INTERACTION WITH SHARED INTERCEPT
gen femage2=female*age2
reg inctot age age2 femage femage2
*THIS ASSUMPTION IS VERY DIFFICULT TO SUPPORT, SO IT IS NOT COMMONLY DONE THIS WAY

*DUMMY-QUADRATIC INTERACTION WITH UNIQUE INTERCEPTS
reg inctot age age2 female femage femage2
reg inctot i.female##c.age i.female##c.age2
}













log close
cmdlog close








/*
GOOD SUPPORTING MATERIAL:
https://stats.idre.ucla.edu/stata/webbooks/reg/
*/
