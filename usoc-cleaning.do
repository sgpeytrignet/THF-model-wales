***************************************************************************
******* Load Understanding Society data - clean from Lendlease data *******
***************************************************************************

use "$SHAREPOINT\Simetrica - Projects\Lendlease Social Value Framework\Global project\SVB development\Workstream B\Analysis\Input\clean_dataset_all_outcomes W1-9.dta", clear

*************************
******* Functions *******
*************************

cap program drop list_subset
program define list_subset
levelsof pidp if `1', local(idx)
set more off
foreach id of local idx {
sort pidp wave
list `2' if pidp==`id'
}
end

cap program drop list_subset_hh
program define list_subset_hh
levelsof hidp if `1', local(idx)
set more off
foreach id of local idx {
sort hidp pidp wave
list `2' if hidp==`id'
}
end

*********************************
******* Control variables *******
*********************************
	
global controls male_fe age age2 i.ethnicity degree i.wave leq_hhincome i.gor_dv goodhealth

*******************************
******* Health outcomes *******
*******************************

*Subjective general health as a control
tab ghealth, miss

*Objective general health as a control
tab LT_health, miss

*Outpatient
desc hl2hop
numlabel g_hl2hop, add mask("[#] ")
tab hl2hop if wave==9, miss
cap drop outpatient_lastyear
gen outpatient_lastyear=cond(hl2hop>0&hl2hop<=4,1,cond(hl2hop==0,0,.))
tab hl2hop outpatient_lastyear, miss
tab outpatient_lastyear if wave==9, miss

*Inpatient (may include childbirth)
desc hosp
numlabel g_hosp, add mask("[#] ")
tab hosp, miss
cap drop inpatient_lastyear
gen inpatient_lastyear=cond(hosp==1,1,cond(hosp==2,0,.))
tab hosp inpatient_lastyear, miss

*Inpatient or outpatient
cap drop patient_lastyear
gen patient_lastyear=.
replace patient_lastyear=1 if outpatient_lastyear==1|outpatient_lastyear==1
replace patient_lastyear=0 if outpatient_lastyear==0&outpatient_lastyear==0
tab patient_lastyear if wave==9, miss

*******************************
******* Other outcomes ********
*******************************

tab urban_dv if wave==9, miss
gen urban=cond(urban_dv==1,1,cond(urban_dv==2,0,.))
tab urban_dv urban, miss

**************************************
******* Longitudinal cleaning ********
**************************************

keep if wave==8|wave==9

*list_subset "gor_dv==10&patient_lastyear==1&age<30" "pidp wave age urban patient_lastyear inpatient_lastyear outpatient_lastyear"

sort pidp wave
by pidp: gen patient_nexttyear=patient_lastyear[_n+1]
by pidp: gen inpatient_nexttyear=inpatient_lastyear[_n+1]
by pidp: gen outpatient_nexttyear=outpatient_lastyear[_n+1]

*list_subset "gor_dv==10&patient_nexttyear==1&age<30" "pidp wave age urban patient_nexttyear patient_lastyear"
*list_subset "gor_dv==10&patient_nexttyear==0&age<30" "pidp wave age urban patient_nexttyear patient_lastyear"

keep if wave==8

#delimit ;
keep pidp wave age urban male_fe age age2 ethnicity degree wave leq_hhincome gor_dv goodhealth LT_health
patient_lastyear inpatient_lastyear outpatient_lastyear
patient_nexttyear inpatient_nexttyear outpatient_nexttyear;
#delimit cr

*probit patient_nexttyear age age2 male_fe i.ethnicity leq_hhincome i.gor_dv urban
*probit patient_nexttyear age age2 male_fe i.ethnicity leq_hhincome i.gor_dv LT_health urban

drop if urban==.

/*
Thought experiment: At average age, income, and subjective health rating, proximity to medical resource 1 and 2 are not good predictors of being hospitalised
*/

export delimited using "C:\Users\Sebastien\Documents\Health outcomes\Understanding-Society-Wave8.csv", replace