clear                          
set more off                    
capture log close               

cd ""  // USER CHANGE DIR
ssc install eclplot                                           //dependencies
net install asdoc, from(http://fintechprofessor.com) replace
graph set window fontface "Helvetica"      // Setting graph font
set scheme stcolor                        // Set the color scheme for Stata output

log using proj_log.log, replace            // log created

	/********** NOTES
	
	**Choice of reg, tables and figures:**
	
	- T1 - summary stats
	- T2, T3, T4 - RD
	- T5 - MS ans HS reg to show imprecesiness
	
	This is the do file for the replication project on the paper, Holden(2016). I have restricted my regression and analysis 
to discontinuity of effect of textbook funding on performance. I have not delved into specifications of distributions of the
schools. 

The paper finds that  This code implements a regression discontinuity (RD) design to examine the effect of textbook funding 
on school-level test performance in California schools. Significant positive effects are found in elementary schools, while estimates for middle and high schools are imprecise. The code includes data preparation, regression analysis, and graphical presentation of results
	
*/ 


********************************************************************************


// Generate running variable - based on API score in 2003. stype - M (middle schhol) | stype - H (high school) 643(E), 600(M), 584(H) significance - cutoff API score for additional funding of $96.90 per student over $54.22 based


********************************************************************************
								*ELEMENTARY SCHOOL*
********************************************************************************


clear
use "Data/CA_schools_es.dta"
gen norm = api_rank - 643
replace norm = api_rank - 600 if stype == "M"
replace norm = api_rank - 584 if stype == "H"

// Labeling variables
label variable mathscore "Math Score"
label variable readingscore "Reading Score"
label variable api_rank "API Score in 2003"
label variable percentfrl "Percent eligible for free or reduced lunch"
label variable total "Total enrollment"
label variable pct_hi "Percent Hispanic"
label variable pct_wh "Percent White"
label variable pct_other "Percent Other"
label variable fte_t "FTE for teachers"
label variable fte_a "FTE for admin"
label variable fte_p "FTE for pupil service"
label variable classsize "Pupil/teacher"


** TABLE 1 **

asdoc, row(Variable,\i, \i, Full Sample, \i, \i, 19.099 API Cutoff Bandwidth) ///
	   title(Table 1: Summary Statistics) replace
asdoc, row(\i, \i, N, Mean\, \i, N, Mean) append
foreach var of varlist mathscore readingscore api_rank total pct_hi pct_wh pct_other percentfrl fte_t fte_a fte_p classsize{

// Generate summary statistics for the full sample
qui sum `var'
	local m = `r(mean)'
	asdoc, accum(\i, `r(N)', `m')


// Generate summary statistics for observations within API bandwidth
qui sum `var' if abs(norm) < 19.099
	local m = `r(mean)'
	asdoc, accum(\i, `r(N)', `m')
	
asdoc, label row(`var', $accum) dec(2) save(644_tables.doc)
}

// Restrict for elementary schools
keep if stype == "E"

gen ind = 0 
replace ind = 1 if api_rank <= 643

gen ind_norm = ind*norm

// Keep Observations within 200 API window of cutoff

keep if abs(norm) < 100

// Normalize test scores 

sum mathscore

replace mathscore = (mathscore - r(mean))/r(sd)

sum readingscore

replace readingscore = (readingscore - r(mean))/r(sd)

gen average_score = (mathscore+readingscore)/2

// Create an average for "percent meeting X standard" variables

gen a_1 = (farbelowbasic_read+farbelowbasic_math)/2
gen a_2 = (cstcapapercentagebelowbasic_read+cstcapapercentagebelowbasic_math)/2
gen a_3 = (cstcapapercentagebasic_read+cstcapapercentagebasic_math)/2
gen a_4 = (cstcapapercentageproficient_read+cstcapapercentageproficient_math)/2
gen a_5 = (cstcapapercentageadvanced_read+cstcapapercentageadvanced_math)/2




// Falsification Index - detect potential biases or spurious correlations in regression models.

// Generate lagged test scores - cds: unique identifier 
sort cds year
bysort cds: gen lag_average_score = average_score[_n-1]

// Generage index with various characteristics
reg average_score total pct_wh pct_hi percentfrl if year < 2005 & norm < 19.099 & norm > -19.099
predict y_hat1

reg average_score total pct_wh pct_hi percentfrl fte_t fte_a fte_p yrs_teach yrs_dist if year < 2005 & norm < 19.099 & norm > -19.099
predict y_hat2

reg average_score total pct_wh pct_hi percentfrl fte_t fte_a fte_p yrs_teach yrs_dist lag_average_score if year < 2005 & norm < 19.099 & norm > -19.099
predict y_hat3


reg mathscore total pct_wh pct_hi percentfrl if year < 2005 & norm < 19.099 & norm > -19.099
predict ym_hat1

reg mathscore total pct_wh pct_hi percentfrl fte_t fte_a fte_p yrs_teach yrs_dist if year < 2005 & norm < 19.099 & norm > -19.099
predict ym_hat2

reg mathscore total pct_wh pct_hi percentfrl fte_t fte_a fte_p yrs_teach yrs_dist lag_average_score if year < 2005 & norm < 19.099 & norm > -19.099
predict ym_hat3

reg readingscore total pct_wh pct_hi percentfrl if year < 2005 & norm < 19.099 & norm > -19.099
predict yr_hat1

reg readingscore total pct_wh pct_hi percentfrl fte_t fte_a fte_p yrs_teach yrs_dist if year < 2005 & norm < 19.099 & norm > -19.099
predict yr_hat2

reg readingscore total pct_wh pct_hi percentfrl fte_t fte_a fte_p yrs_teach yrs_dist lag_average_score if year < 2005 & norm < 19.099 & norm > -19.099
predict yr_hat3

// Store regression results for average_score model


** TABLE 2 **

reg y_hat1 ind norm ind_norm if norm < 19.099 & norm > -19.099 & year < 2005 , vce(robust)
reg y_hat2 ind norm ind_norm if norm < 19.099 & norm > -19.099 & year < 2005 , vce(robust)
reg y_hat3 ind norm ind_norm if norm < 19.099 & norm > -19.099 & year < 2005 , vce(robust)

reg ym_hat1 ind norm ind_norm if norm < 19.099 & norm > -19.099 & year < 2005 , vce(robust)
reg ym_hat2 ind norm ind_norm if norm < 19.099 & norm > -19.099 & year < 2005 , vce(robust)
reg ym_hat3 ind norm ind_norm if norm < 19.099 & norm > -19.099 & year < 2005 , vce(robust)

reg yr_hat1 ind norm ind_norm if norm < 19.099 & norm > -19.099 & year < 2005 , vce(robust)
reg yr_hat2 ind norm ind_norm if norm < 19.099 & norm > -19.099 & year < 2005 , vce(robust)
reg yr_hat3 ind norm ind_norm if norm < 19.099 & norm > -19.099 & year < 2005 , vce(robust)



** TABLE 3 **

eststo clear // Clear any previously stored estimation results

reg average_score ind norm ind_norm if norm < 19.099 & norm > -19.099 & year >= 2005, vce(robust)

reg average_score ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2002, vce(robust)
reg average_score ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2003, vce(robust)
reg average_score ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2004, vce(robust)
reg average_score ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2005, vce(robust)
reg average_score ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2006, vce(robust)
reg average_score ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2007, vce(robust)
reg average_score ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2008, vce(robust)
reg average_score ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2009, vce(robust)

reg mathscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year >= 2005, vce(robust)

reg mathscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2002, vce(robust)
reg mathscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2003, vce(robust)
reg mathscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2004, vce(robust)
reg mathscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2005, vce(robust)
reg mathscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2006, vce(robust)
reg mathscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2007, vce(robust)
reg mathscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2008, vce(robust)
reg mathscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2009, vce(robust)


reg readingscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year >= 2005, vce(robust)

reg readingscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2002, vce(robust)
reg readingscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2003, vce(robust)
reg readingscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2004, vce(robust)
reg readingscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2005, vce(robust)
reg readingscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2006, vce(robust)
reg readingscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2007, vce(robust)
reg readingscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2008, vce(robust)
reg readingscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2009, vce(robust)
   


// Figures for elemenatry school level - only relevant outcome, reg not valid for MS and HS

// Generate bins for figures

local enr = "norm"
local bin =3


gen bin = round(`enr'+`bin'/2, `bin' )
replace bin= bin-`bin'/2

** TABLE 4 & 5 **
// Clear previous results
estimates clear

// Define list of dependent variables
local vars a_1 a_2 a_3 a_4 a_5

// Loop over each dependent variable
foreach var in `vars' {
    // Run regression for years post-2005
    forvalues year = 2002/2004 {
        quietly reg `var' ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == `year', vce(robust)
        eststo `var'_`year'
    }
}

// Combine results into one table
esttab * using 6.doc, ///
    cell(b(star fmt(3)) se(par fmt(3))) ///
    label ///
    nostar ///
    nomtitle ///
    collabels(none)
	
// Clear previous results
estimates clear

// Define list of dependent variables
local vars a_1 a_2 a_3 a_4 a_5

// Loop over each dependent variable
foreach var in `vars' {
    // Run regression for years post-2005
    forvalues year = 2005/2009 {
        quietly reg `var' ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == `year', vce(robust)
        eststo `var'_`year'
    }
}

// Combine results into one table
esttab * using 1.doc, ///
    cell(b(star fmt(3)) se(par fmt(3))) ///
    label ///
    nostar ///
    nomtitle ///
    collabels(none)
	
*****************************************************************************
					      * GRAPHS FOR ES *
*****************************************************************************


** FIGURE 1 ** 

// Generate average_score_bin variable
egen average_score_bin = mean(average_score), by(bin year)

// Scatter plot for year 2003
twoway (scatter average_score_bin bin if bin < 50 & bin > -50 & year == 2003, ylabel(#5) xline(0) xti("API in 2003 relative to cutoff") yti(" Predicted test score")), ///
name(F1, replace)


** FIGURE 2 **


// Estimate and store effects over time

foreach subject in average_ {

gen estimate_`subject' = .
gen lower95_`subject' = .
gen higher95_`subject' = .
gen id_`subject' = .


local count = 0

forval x = 2002/2011{
reg `subject'score ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == `x', vce(robust)
local est_`subject'_`x' = _b[ind]
local se_`subject'_`x' = _se[ind]

local low95_`subject'_`x' = `est_`subject'_`x'' - 1.96*`se_`subject'_`x''
local high95_`subject'_`x' = `est_`subject'_`x'' + 1.96*`se_`subject'_`x''


display `est_`subject'_`x''

local count = `count' +1

replace estimate_`subject' = `est_`subject'_`x'' in `count'
replace lower95_`subject' = `low95_`subject'_`x'' in `count'
replace higher95_`subject' = `high95_`subject'_`x'' in `count'
replace id_`subject' = `x' in `count'

}

local count = 0

}

eclplot estimate_average_ lower95_average higher95_average id_average, xti("Year") yti("Effect on average test scores") yline(0) xline(2004.5 , lp(dash)) name(F2, replace) 

	
** FIGURE 3 **


forval x = 1/5{

egen a_`x'_bin = mean(a_`x') if year == 2007, by(bin)

}

twoway (scatter  a_1_bin bin if bin < 50 & bin > -50 ,ylabel(#5)  xline(0) xti("API in 2003 relative to cutoff") yti(Percent of students) title("Far below basic") name(G1, replace))
twoway (scatter  a_2_bin bin if bin < 50 & bin > -50 ,ylabel(#5)  xline(0) xti("API in 2003 relative to cutoff") yti(Percent of students) title("Below basic") name(G2, replace))
twoway (scatter  a_3_bin bin if bin < 50 & bin > -50 ,ylabel(#5)  xline(0) xti("API in 2003 relative to cutoff") yti(Percent of students) title("Basic") name(G3, replace))
twoway (scatter  a_4_bin bin if bin < 50 & bin > -50 ,ylabel(#5)  xline(0) xti("API in 2003 relative to cutoff") yti(Percent of students) title("Proficient") name(G4, replace))
twoway (scatter  a_5_bin bin if bin < 50 & bin > -50 ,ylabel(#5)  xline(0) xti("API in 2003 relative to cutoff") yti(Percent of students) title("Advanced") name(G5, replace))
graph combine G1 G2 G3 G4 G5, rows(2) xcommon ycommon name(F3, replace)


*****************************************************************************
				* MIDDLE SCHOOL & HIGH SCHOOL *
*****************************************************************************

clear all
use "Data/CA_schools_ms.dta"


// Generate running variable

gen norm = api_rank - 600

// Create indicator for whether a school is below the cutoff

gen ind = 0 
replace ind = 1 if norm <= 0

// Interaction for flexible slope at cutoff

gen ind_norm = ind*norm


// Keep Observations within 200 API window of cutoff**

keep if abs(norm) < 100


// Normalize test scores to school-level average and sd

sum mathscore

replace mathscore = (mathscore - r(mean))/r(sd)

sum readingscore

replace readingscore = (readingscore - r(mean))/r(sd)

gen average_score = (mathscore+readingscore)/2



*PANEL A*
reg average_score ind norm ind_norm if norm < 19.099 & norm > -19.099 & year >= 2005, vce(robust)
reg mathscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year >= 2005, vce(robust)
reg readingscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year >= 2005, vce(robust)
// Only the above has been included in the paper regression since it sufficiently captures the effects


reg average_score ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2005, vce(robust)
reg average_score ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2006, vce(robust)
reg average_score ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2007, vce(robust)
reg average_score ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2008, vce(robust)
reg average_score ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2009, vce(robust)

reg mathscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2005, vce(robust)
reg mathscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2006, vce(robust)
reg mathscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2007, vce(robust)
reg mathscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2008, vce(robust)
reg mathscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2009, vce(robust)

reg readingscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2005, vce(robust)
reg readingscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2006, vce(robust)
reg readingscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2007, vce(robust)
reg readingscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2008, vce(robust)
reg readingscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2009, vce(robust)


*****************************************************************************

clear all
use "Data/CA_schools_hs.dta"

// Generate running variable

gen norm = api_rank - 600

// Create indicator for whether a school is below the cutoff

gen ind = 0 
replace ind = 1 if norm <= 0

// Interaction for flexible slope at cutoff

gen ind_norm = ind*norm


// Keep Observations within 200 API window of cutoff**

keep if abs(norm) < 100


// Normalize test scores to school-level average and sd

sum mathscore

replace mathscore = (mathscore - r(mean))/r(sd)

sum readingscore

replace readingscore = (readingscore - r(mean))/r(sd)

gen average_score = (mathscore+readingscore)/2


*PANEL B*
reg average_score ind norm ind_norm if norm < 19.099 & norm > -19.099 & year >= 2005, vce(robust)
reg mathscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year >= 2005, vce(robust)
reg readingscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year >= 2005, vce(robust)
// Only the above has been included in the paper regression since it sufficiently captures the effects


reg average_score ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2005, vce(robust)
reg average_score ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2006, vce(robust)
reg average_score ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2007, vce(robust)
reg average_score ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2008, vce(robust)
reg average_score ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2009, vce(robust)

reg mathscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2005, vce(robust)
reg mathscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2006, vce(robust)
reg mathscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2007, vce(robust)
reg mathscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2008, vce(robust)
reg mathscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2009, vce(robust)

reg readingscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2005, vce(robust)
reg readingscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2006, vce(robust)
reg readingscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2007, vce(robust)
reg readingscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2008, vce(robust)
reg readingscore ind norm ind_norm if norm < 19.099 & norm > -19.099 & year == 2009, vce(robust)

*****************************************************************************
					* DISTRICT SPENDING *
*****************************************************************************
clear all
use "Data/CA_districts.dta"

// Convert district-level spending to per-student levels

foreach x of var value2100 value4100 value5100 value5200 value5500 value6400{
replace `x' = `x'/total
}


foreach x of var value2100 value4100 value5100 value5200 value5500 value6400{
gen sd_`x' = `x'
}


collapse value* (sd) sd_value*, by(year ind_no)

foreach x of var value2100  value4100 value5100 value5200 value5500 value6400{

gen ub_`x'_control = `x' + 1.96*(sd_`x'^.5) if ind == 0
gen lb_`x'_control = `x' - 1.96*(sd_`x'^.5) if ind == 0

gen ub_`x'_treat = `x' + 1.96*(sd_`x'^.5) if ind == 1
gen lb_`x'_treat = `x' - 1.96*(sd_`x'^.5) if ind == 1

}

// Renaming for simplicity
rename value4100 txtbook
rename value6400 eqpt
rename value2100 inst_aid
rename value5100 services
rename value5500 opns
rename value5200 travel_conf


** FIGURE 4 **

// Spending on textbooks by districts over years
twoway (rarea ub_value4100_control lb_value4100_control year, color(gs15)) ///
       (line txtbook year if ind == 0 , lpattern(dash_dot) color(gs12)) ///
       (line txtbook year if ind == 1, color(gs8) ti() lpattern(dashed)),  ///
       legend(label(1 "95% CI Affected") label(2 "Spending in Affected Districts") ///
	   label(3 "Spending in Unaffected Districts") span position(6) rows(2)) ///
       legend(order(2 1 3)) xline(2004) yti("Spending per Student") ///
       name(F4, replace)
	   
/* 
The graphs below show effects of other variables such as : Spending on conferences, travel and institutional aid.
It has not been included in the paper since it is not relevant to the analysis or the primary question of effect of textbook 	funding
*/

twoway (rarea ub_value6400_control lb_value6400_control year, color(gs15))   (line eqpt year if ind ==0 , lpattern(dash_dot) color(gs12))(line eqpt year if ind ==1, color(gs8)ti() lpattern(dashed)) , legend( label(1 "95% CI Affected") label(2 "Spending in Affected Districts") label(3 "Spending in Unaffected Districts") span position(6) rows(2)) legend(order( 2 1 3)) xline(2004) yti("Spending per Student") name(E1, replace)

twoway (rarea ub_value2100_control lb_value2100_control year, color(gs15))   (line inst_aid year if ind ==0 , lpattern(dash_dot) color(gs12))(line inst_aid year if ind ==1, color(gs8)ti() lpattern(dashed)) , legend( label(1 "95% CI Affected") label(2 "Spending in Affected Districts") label(3 "Spending in Unaffected Districts") span position(6) rows(2)) legend(order( 2 1 3)) xline(2004) yti("Spending per Student") name(E2, replace)

twoway (rarea ub_value5100_control lb_value5100_control year, color(gs15))   (line services year if ind ==0 , lpattern(dash_dot) color(gs12))(line services year if ind ==1, color(gs8)ti() lpattern(dashed)) , legend( label(1 "95% CI Affected") label(2 "Spending in Affected Districts") label(3 "Spending in Unaffected Districts") span position(6) rows(2)) legend(order( 2 1 3)) xline(2004) yti("Spending per Student") name(E3, replace)

twoway (rarea ub_value5500_control lb_value5500_control year, color(gs15))   (line opns year if ind ==0 , lpattern(dash_dot) color(gs12))(line opns year if ind ==1, color(gs8)ti() lpattern(dashed)) , legend( label(1 "95% CI Affected") label(2 "Spending in Affected Districts") label(3 "Spending in Unaffected Districts") span position(6) rows(2)) legend(order( 2 1 3)) xline(2004) yti("Spending per Student") name(E4, replace)

twoway (rarea ub_value5200_control lb_value5200_control year, color(gs15))   (line travel_conf year if ind ==0 , lpattern(dash_dot) color(gs12))(line travel_conf year if ind ==1, color(gs8)ti() lpattern(dashed)) , legend( label(1 "95% CI Affected") label(2 "Spending in Affected Districts") label(3 "Spending in Unaffected Districts") span position(6) rows(2)) legend(order( 2 1 3)) xline(2004) yti("Spending per Student") name(E5, replace)
********************************************************************************

log close



