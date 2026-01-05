/// ***************************************************************
** PRELIMINARY CHECKS
* Import data
import delimited "/Users/eugenio/Desktop/ISTAT/Tiro24_sam_datie.txt"
gen sesso_num = .
replace sesso_num = 0 if sesso == "M"
replace sesso_num = 1 if sesso == "F"
save Income.dta, replace 

* Missing Values
forvalues i = 15/21 {
	gen income_miss`i' = missing(y_dispo`i')
}
foreach var of varlist sesso_num cittad rip4 titostud classeta {
    forvalues i = 15/21 {
        display "Frequency analysis for variable: `var' in year 20`i'"
        tabulate `var' income_miss`i', missing
    }
}
** It seems to be a correlation in the missing values and some baseline characteristics such as female, people living in South of Italy, and less educated people.

foreach var of varlist sesso_num cittad rip4 titostud classeta {
    forvalues i = 15/21 {
        display "Chi-square test for variable: `var' in year 20`i'"
        tabulate `var' income_miss`i', chi2
        display "P-value: " %20.18f r(p)
    }
}

* Duplicates
duplicates report
* There are no duplicates in the dataset.



/// *********************************
** GENERAL MEANS OF DISPOSABLE INCOME
use Income.dta, clear

* Nominal means of disposable income from 2015 to 2021
forvalues i = 15/21 {
    mean y_dispo`i' [aweight=coef] if !missing(y_dispo`i', coef)
    scalar mean_dispo_`i' = r(table)[1,1]
}

* Create a graph
clear
set obs 7
gen year = 2015 + _n - 1
gen mean_dispo = .
forvalues i = 15/21 {
	replace mean_dispo = scalar(mean_dispo_`i') if year == 20`i'
}
list

graph bar mean_dispo, over(year) ylabel(0(2000)24000) ytitle(" ") blabel(bar, size(medium) position(outside)) bar(1, color(green))

graph export "/Users/eugenio/Desktop/ISTAT/Analisi/AvDispInc.pdf", as(pdf) replace

* By codfam
forvalues i = 15/21 {
	egen y_dispofam`i' = total(y_dispo`i'), by(codfam)
    mean y_dispofam`i' [aweight=coef] if !missing(y_dispofam`i', coef)
    scalar mean_dispo_fam`i' = r(table)[1,1]
}

clear
set obs 7
gen year = 2015 + _n - 1
gen mean_dispofam = .
forvalues i = 15/21 {
	replace mean_dispofam = scalar(mean_dispo_fam`i') if year == 20`i'
}
list

graph bar mean_dispofam, over(year) ylabel(0(4000)38000) ytitle(" ") blabel(bar, size(medium) position(outside)) bar(1, color(orange))

graph export "/Users/eugenio/Desktop/ISTAT/Analisi/Average_Household_Disposable_Income.pdf", as(pdf) replace


** GENERAL MEANS OF DISPOSABLE INCOME IN REAL TERMS

* Import data and add consumer price index
use Income.dta, clear
gen ipc2015 = 100
gen ipc2016 = 99.9
gen ipc2017 = 101.2
gen ipc2018 = 102.3
gen ipc2019 = 102.9
gen ipc2020 = 102.7
gen ipc2021 = 104.7

* Generate real values
forvalues i = 15/21 {
    gen y_dispoR`i' = (y_dispo`i' / ipc20`i') * 100
}
save Income.dta, replace

* Real means of disposable income from 2015 to 2021
forvalues i = 15/21 {
    mean y_dispoR`i' [aweight=coef] if !missing(y_dispoR`i', coef)
    scalar mean_dispoR_`i' = r(table)[1,1]
}

* Income differentials
forvalues i = 15/20 {
	local j = `i' + 1
	scalar Rdiff`j' = ((mean_dispoR_`j' - mean_dispoR_`i')/mean_dispoR_`i') * 100
}

* Create a graph
clear
set obs 7
gen year = 2015 + _n - 1
gen mean_dispoR = .
forvalues i = 15/21 {
	replace mean_dispoR = scalar(mean_dispoR_`i') if year == 20`i'
}
gen pct_diff = .
forvalues i = 16/21 {
    replace pct_diff = scalar(Rdiff`i') if year == 20`i'
}
list

twoway (bar mean_dispoR year, barwidth(0.5) color(green) yaxis(1)) ///
       (line pct_diff year, lcolor(red) lwidth(medium) yaxis(2)) ///
       , title("Average Real Disposable Income and Differentials in Italy (2015-2021)") ///
         ylabel(0(4000)24000, axis(1) grid) ///
         ytitle("Income (EUR)", axis(1)) ///
         ylabel(-5(5)10, axis(2)) ///
         ytitle("Percentage Change", axis(2)) ///
         legend(order(1 "Mean Disposable Income" 2 "Percentage Change") pos(6)) ///
         xlabel(2015(1)2021, angle(45))

graph bar mean_dispoR, over(year) ylabel(0(2000)24000) ytitle(" ") blabel(bar, size(medium) position(outside)) bar(1, color(red))

graph export "/Users/eugenio/Desktop/ISTAT/Analisi/Average_Real_Disposable_Income.pdf", as(pdf) replace


* By codfam
use Income.dta, clear
forvalues i = 15/21 {
	egen y_dispoRfam`i' = total(y_dispoR`i'), by(codfam)
    mean y_dispoRfam`i' [aweight=coef] if !missing(y_dispoRfam`i', coef)
    scalar mean_dispoR_fam`i' = r(table)[1,1]
}

clear
set obs 7
gen year = 2015 + _n - 1
gen mean_dispoRfam = .
forvalues i = 15/21 {
	replace mean_dispoRfam = scalar(mean_dispoR_fam`i') if year == 20`i'
}
list

graph bar mean_dispoRfam, over(year) ylabel(0(4000)38000) ytitle(" ") blabel(bar, size(medium) position(outside)) bar(1, color(blue))

graph export "/Users/eugenio/Desktop/ISTAT/Analisi/Average_RHousehold_Disposable_Income.pdf", as(pdf) replace




/// ********************************
** SUBSTITUTION IN THE LABOR MARKET
use Income.dta, clear

* Generate variables for people active in the labor market, people retired and people out of the market
forvalues i = 15/21 {
    gen active`i' = (ye1_lav_dip`i' > 0 & !missing(ye1_lav_dip`i')| ye3_lav_aut`i' > 0 & !missing(ye3_lav_aut`i') & ye2_red_pen`i' == 0 & !missing(ye2_red_pen`i'))
	gen pen`i' = (ye1_lav_dip`i' == 0 & !missing(ye1_lav_dip`i') & ye2_red_pen`i' > 0 & !missing(ye2_red_pen`i') & ye3_lav_aut`i' == 0 & !missing(ye3_lav_aut`i'))
	gen out`i' = ((ye1_lav_dip`i' == 0 | missing(ye1_lav_dip`i')) & (ye2_red_pen`i' == 0 | missing(ye2_red_pen`i')) & (ye3_lav_aut`i' == 0 | missing(ye3_lav_aut`i')))
}
forvalues i = 15/21 {
    gen active`i' = ((ye1_lav_dip`i' > 0 | ye3_lav_aut`i' > 0) & ye2_red_pen`i' == 0)
	gen pen`i' = (ye1_lav_dip`i' == 0 & ye2_red_pen`i' > 0 & ye3_lav_aut`i' == 0)
	gen out`i' = (ye1_lav_dip`i' == 0 & ye2_red_pen`i' == 0 & ye3_lav_aut`i' == 0)
}
forvalues i = 15/21 {
    gen weight_active`i' = active`i' * coef
    gen weight_pen`i' = pen`i' * coef
    gen weight_out`i' = out`i' * coef
}
forvalues i = 15/21 {
	quietly sum weight_active`i' if active`i' == 1
    scalar active_weighted`i' = r(sum)
	quietly sum weight_pen`i' if pen`i' == 1
    scalar pen_weighted`i' = r(sum)
	quietly sum weight_out`i' if out`i' == 1
    scalar out_weighted`i' = r(sum)
}

* Compute rate of change in each category
forvalues i = 16/21 {
	scalar rate_act`i' = (scalar(active_weighted`i') - scalar(active_weighted`=`i'-1')) / scalar(active_weighted`=`i'-1') * 100
	scalar rate_pen`i' = (scalar(pen_weighted`i') - scalar(pen_weighted`=`i'-1')) / scalar(pen_weighted`=`i'-1') * 100
	scalar rate_out`i' = (scalar(out_weighted`i') - scalar(out_weighted`=`i'-1')) / scalar(out_weighted`=`i'-1') * 100
}
save Income.dta, replace

** Graph 
clear
set obs 7
gen year = 2015 + _n - 1
gen active = .
gen pen = .
gen out = .
gen rate_act = .
gen rate_pen = .
gen rate_out = .
forvalues i = 15/21 {
	replace active = scalar(active_weighted`i') if year == 20`i'
	replace pen = scalar(pen_weighted`i') if year == 20`i'
	replace out = scalar(out_weighted`i') if year == 20`i'
}
forvalues i = 16/21{
	replace rate_act = scalar(rate_act`i') if year == 20`i'
	replace rate_pen = scalar(rate_pen`i') if year == 20`i'
	replace rate_out = scalar(rate_out`i') if year == 20`i'
}
list

graph bar active out pen, over(year) ylabel(, format(%9.0f)) legend(order(1 "Employees" 2 "Inactive" 3 "Retirees"))

gen year_active = year - 0.2
gen year_pen = year
gen year_out = year + 0.2

twoway ///
    (bar active year_active, barwidth(0.3) color(green) yaxis(1)) ///
    (bar pen year_pen, barwidth(0.3) color(red) yaxis(1)) ///
    (line rate_act year, lcolor(green) lwidth(medium) lpattern(dash) yaxis(2)) ///
    (line rate_pen year, lcolor(red) lwidth(medium) lpattern(dash) yaxis(2)), ///
    ylabel(5000000(5000000)30000000, format(%9.0f)) ///
    ytitle("Absolute Numbers") ///
    ytitle("Rate of Change (%)", axis(2)) ///
	xlabel(2015(1)2021) ///
    xtitle("Year") ///
	legend(order(1 "Active" 2 "Retirees" 3 "Rate of Active" 4 "Rate of Retirees") rows(2) position(6))

graph export "/Users/eugenio/Desktop/ISTAT/Analisi/Sub_Labor2.pdf", as(pdf) replace



/// *******************
** INEQUALITY MEASURES

clear all
* Import data again
use Income.dta, clear

** Gini coefficients and Atkinson indices
ssc install ineqdeco
forvalues i = 15/21{
	ineqdeco y_dispo`i' [aweight=coef]
	scalar gini_`i' = r(gini)
	scalar atk_`i' = r(a1)
}

* Create a table
clear
set obs 7 
gen year = 2015 + _n - 1  
gen gini = . 
gen atkinson = .
forvalues i = 15/21{
	replace gini = scalar(gini_`i') if year == 20`i'
	replace atkinson = scalar(atk_`i') if year == 20`i'
}
* Export
ssc install asdoc
asdoc list year gini atkinson, replace

** measures biased by the lack of capital and financial.


** Income Quintile Ratio
use Income.dta, clear

forvalues i = 15/21 {
	quietly{
		sort y_dispoR`i'
	count if !missing(y_dispoR`i')
	scalar n`i'=r(N)
	scalar s80_pos`i' = 0.80 * (scalar(n`i') + 1)
	scalar s20_pos`i' = 0.20 * (scalar(n`i') + 1)
    summarize y_dispoR`i' if _n == floor(scalar(s20_pos`i')) | _n == ceil(scalar(s20_pos`i')), meanonly
    scalar s20_val`i' = r(mean)
    summarize y_dispoR`i' if _n == floor(scalar(s80_pos`i')) | _n == ceil(scalar(s80_pos`i')), meanonly
    scalar s80_val`i' = r(mean)
	scalar qui_ratio`i' = scalar(s80_val`i') / scalar(s20_val`i')
	}
}
scalar list

* Create a table
clear
set obs 7 
gen year = 2015 + _n - 1  
gen S80_S20 = . 
forvalues i = 15/21{
	replace S80_S20 = scalar(qui_ratio`i') if year == 20`i'
}
* Export
asdoc list year S80_S20, replace


* not consistent with official data, lower figures.


** Poverty Rates
forvalues i = 15/21 {
	preserve
quietly {
	collapse (mean) y_dispoR`i', by(codfam)
	summarize y_dispoR`i', detail
	scalar med`i' = r(p50)
	scalar pov_lin`i' = 0.6 * med`i'
	gen below`i'= y_dispoR`i' < scalar(pov_lin`i')
	summarize below`i'
	scalar pov_rate`i' = r(mean) * 100
}
restore
}
scalar list

* consistent with official data: 20% of people at risk of poverty.

* Create a graph
clear
set obs 7
gen year = 2015 + _n - 1
gen pov_rate = .
forvalues i = 15/21 {
	replace pov_rate = scalar(pov_rate`i') if year == 20`i'
}
list
* Export
asdoc list year pov_rate, replace

graph bar pov_rate, over(year) title("% of People below 60% of Median Income in Italy (2015-2021)", color(red)) ylabel(0 "0%" 20 "20%" 40 "40%" 60 "60%" 80 "80%" 100 "100%") ytitle(" ") blabel(bar, size(medium) position(outside)) bar(1, color(green))


** Poverty Rates divided per categories
use Income.dta, clear

forvalues i = 15/21 {
    preserve
    quietly {
        collapse (mean) y_dispoR`i' sesso_num rip4, by(codfam)
        summarize y_dispoR`i', detail
        scalar med`i' = r(p50)
        scalar pov_lin`i' = 0.6 * med`i'
        gen below`i' = y_dispoR`i' < scalar(pov_lin`i')
        summarize below`i'
 
        summarize below`i' if sesso_num == 1
        scalar pov_rate_wom`i' = r(mean) * 100
		summarize below`i' if sesso_num == 0
		scalar pov_rate_men`i' = r(mean) * 100

        forvalues area = 1/4 {
            summarize below`i' if rip4 == `area'
            scalar pov_rate`area'_`i' = r(mean) * 100
        }
    }
    restore
}
scalar list

* Graphs
clear
set obs 7
gen year = 2015 + _n - 1
gen pov_rate_male = .
gen pov_rate_female = .

forvalues i = 15/21 {
    replace pov_rate_male = scalar(pov_rate_men`i') if year == 20`i'
    replace pov_rate_female = scalar(pov_rate_wom`i') if year == 20`i'
}
list

twoway (line pov_rate_male year, lcolor(blue) lwidth(medium) lp(dash)) (line pov_rate_female year, lcolor(red) lwidth(medium)), xlabel(2015(1)2021) ylabel(0(20)60, grid) legend(order(1 "Male" 2 "Female") rows(1) pos(6)) xtitle("Year") ytitle("Poverty Rate (%)")

graph export "/Users/eugenio/Desktop/ISTAT/Analisi/Pov_Gender.pdf", as(pdf) replace

gen pov_rate_area1 = .
gen pov_rate_area2 = .
gen pov_rate_area3 = .
gen pov_rate_area4 = .

forvalues i = 15/21 {
    replace pov_rate_area1 = scalar(pov_rate1_`i') if year == 20`i'
    replace pov_rate_area2 = scalar(pov_rate2_`i') if year == 20`i'
    replace pov_rate_area3 = scalar(pov_rate3_`i') if year == 20`i'
    replace pov_rate_area4 = scalar(pov_rate4_`i') if year == 20`i'
}
list

twoway (line pov_rate_area1 year, lcolor(blue) lwidth(medium)) (line pov_rate_area2 year, lcolor(green) lwidth(medium)) (line pov_rate_area3 year, lcolor(orange) lwidth(medium)) (line pov_rate_area4 year, lcolor(red) lwidth(medium)), xlabel(2015(1)2021) ylabel(0(20)60, grid) legend(order(1 "North-East" 2 "North-West" 3 "Centre" 4 "South") rows(1) pos(6)) xtitle("Year") ytitle("Poverty Rate (%)")

graph export "/Users/eugenio/Desktop/ISTAT/Analisi/Pov_Area.pdf", as(pdf) replace




/// ***************************************************
** INTERGENERATIONAL INEQUALITY MEASURES (NOT FINISHED)

clear all
* Import data again
use Income.dta, clear

* Correlation parental income and children income
gen parent_M = (sesso=="M" & eta>35 & eta<70)
gen parent_F = (sesso=="F" & eta>35 & eta<70)
gen child_y0 = (18 <= eta & eta <= 35 & y_dispo15 > 0)
gen has_parent = 0
bysort codfam: replace has_parent = 1 if parent_F == 1 | parent_M == 1
gen child_y = (child_y0 == 1 & has_parent == 1)

sort codfam eta
by codfam: gen child_y = (child_y0 ==1 & parent_M == 1| parent_F == 1)

by codfam: egen age_min_parent = min(cond(parent_M == 1 | parent_F == 1, eta, .))
by codfam: gen child_y = (18<=eta<=35 & y_dispo15>0 & parent_M == 1 | parent_F == 1)
by codfam: gen child_y = (18 <= eta & eta <= 35 & eta<age_min_parent - 18 & y_dispo15 > 0 & (parent_M == 1 | parent_F == 1))

*
sort codfam eta

gen groups = .
gen ref = 1

bysort codfam: gen diff = eta - eta[_n-1]
bysort codfam: replace diff = . if _n == 1

bysort codfam (eta): replace groups = ref if _n == 1
bysort codfam (eta): replace groups = ref if diff <= 16
bysort codfam (eta): replace groups = ref + 1 if diff > 16
bysort codfam (eta): replace ref = ref + 1 if diff > 16

bysort codfam: gen num_pers = _N
replace groups = 99 if num_pers == 1

bysort codfam: egen generaz = total(groups != groups[_n-1])

gen status = ""
replace status = "Individuo Singolo" if groups == 99
replace status = "Coppia senza figli" if groups == 1 & generaz == 1 & eta >= 25
replace status = "Fratelli" if groups == 1 & generaz == 1 & eta < 25
replace status = "Figlio" if groups == 1 & inlist(generaz, 2, 3)
replace status = "Genitore" if groups == 2 & inlist(generaz, 2, 3)
replace status = "Nonno" if groups == 3 & inlist(generaz, 2, 3)





/// *******************
** NO INCOME

** No income by gender
clear all
* Import data again
use Income.dta, clear

forvalues i = 15/21 {
    gen unemp_ind_men`i' = (sesso_num == 0 & eta >= 16 & y_dispo`i' == 0)
    gen unemp_ind_women`i' = (sesso_num == 1 & eta >= 16 & y_dispo`i' == 0)
    
    gen unemp_weight_men`i' = unemp_ind_men`i' * coef
    gen unemp_weight_women`i' = unemp_ind_women`i' * coef
    
    egen unemp_men_total`i' = total(unemp_weight_men`i')
    egen unemp_women_total`i' = total(unemp_weight_women`i')

}

forvalues i=15/21{
	scalar unemp_men`i' = unemp_men_total`i'[1]
    scalar unemp_women`i' = unemp_women_total`i'[1]
}

save Income.dta, replace


* Create a graph
clear
set obs 14

gen year = .  
gen gender = . 
gen unemp = . 

forvalues i = 15/21 {
    local year = 2000 + `i'
    
    local row_men = 2 * (`i' - 15) + 1 
    local row_women = 2 * (`i' - 15) + 2 

    replace year = `year' in `row_men'
    replace gender = 0 in `row_men'
    replace unemp = `=scalar(unemp_men`i')' in `row_men'

    replace year = `year' in `row_women'
    replace gender = 1 in `row_women'
    replace unemp = `=scalar(unemp_women`i')' in `row_women'
}
label define gender_lbl 0 "M" 1 "W"
label values gender gender_lbl
list

graph bar unemp, over(gender) over(year) ytitle(" ") bargap(25) ylabel(20000(20000)200000, format(%9.0f))

graph export "/Users/eugenio/Desktop/ISTAT/Analisi/No_Income_Gender.pdf", as(pdf) replace


//
** % of people active in the labour market
clear all
* Import data again
use Income.dta, clear

* Compute active figures by gender
forvalues i = 15/21 {
	gen active_men`i'= active`i' if sesso_num == 0
	gen active_women`i' = active`i' if sesso_num == 1
}
forvalues i = 15/21 {
    gen weight_active_men`i' = active_men`i' * coef
    gen weight_active_women`i' = active_women`i' * coef
}

forvalues i = 15/21 {
	summ weight_active_men`i', meanonly
    scalar active_men_sum`i' = r(sum)
    summ unemp_men`i', meanonly
    scalar unemp_men_sum`i' = r(sum)
    scalar tot_lab_men`i' = active_men_sum`i' + unemp_men_sum`i'

    summ weight_active_women`i', meanonly
    scalar active_women_sum`i' = r(sum)
    summ unemp_women`i', meanonly
    scalar unemp_women_sum`i' = r(sum)
    scalar tot_lab_women`i' = active_women_sum`i' + unemp_women_sum`i'

    scalar tot_lab`i' = tot_lab_men`i' + tot_lab_women`i'
}

* Compute rate
forvalues i = 15/21 {
	scalar unemp_rate_men`i' = unemp_men_sum`i' / tot_lab_men`i'  
    scalar unemp_rate_women`i' = unemp_women_sum`i' / tot_lab_women`i' 
    scalar unemp_rate`i' = (unemp_men_sum`i' + unemp_women_sum`i') / tot_lab`i' 
}

* Graph
clear
set obs 7 

* Add year variable
gen year = 2015 + _n - 1

* Add no income data to new dataset
gen unemp_men_graph = .
gen unemp_women_graph = .
gen unemp_total_graph = .

forvalues i = 15/21 {
    replace unemp_men_graph = scalar(unemp_men_sum`i') in `=`i' - 14'
    replace unemp_women_graph = scalar(unemp_women_sum`i') in `=`i' - 14'
    replace unemp_total_graph = scalar(unemp_men_sum`i' + unemp_women_sum`i') in `=`i' - 14'
}

* Plot no income figures
twoway (line unemp_men_graph year, lcolor(blue) lwidth(medium) lpattern(solid)) ///
    (line unemp_women_graph year, lcolor(red) lwidth(medium) lpattern(dash)) ///
    (line unemp_total_graph year, lcolor(green) lwidth(medium) lpattern(dot)), ///
    legend(order(1 "Men" 2 "Women" 3 "Total")) ///
    title("Unemployment by Gender and Total (2015-2021)") ///
    ytitle("Unemployed Individuals") ///
    xtitle("Year")


	
* Add no income rate data to new dataset
gen unemp_rate_men_graph = .
gen unemp_rate_women_graph = .
gen unemp_rate_total_graph = .

forvalues i = 15/21 {
    replace unemp_rate_men_graph = scalar(unemp_rate_men`i') in `=`i' - 14'
    replace unemp_rate_women_graph = scalar(unemp_rate_women`i') in `=`i' - 14'
    replace unemp_rate_total_graph = scalar(unemp_rate`i') in `=`i' - 14'
}

* Plot no income rates
twoway ///
    (line unemp_rate_men_graph year, lcolor(blue) lwidth(medium) lpattern(solid)) ///
    (line unemp_rate_women_graph year, lcolor(red) lwidth(medium) lpattern(dash)) ///
    (line unemp_rate_total_graph year, lcolor(green) lwidth(medium) lpattern(dot)), ///
    legend(order(1 "Men" 2 "Women" 3 "Total")) ///
    title("Unemployment Rates by Gender and Total (2015-2021)") ///
    ytitle("Unemployment Rate") ///
    xtitle("Year")

	
//	
** No income in education and gender
clear all
* Import data again
use Income.dta, clear

forvalues i = 15/21 {
	forvalues j = 1/8 {
		count if (sesso_num==0 & eta>=16 & y_dispo`i'==0 & titostud ==`j')
		scalar unemp_men_stud`j'_`i' = r(N)
		count if (sesso_num==1 & eta>=16 & y_dispo`i'==0 & titostud ==`j')
		scalar unemp_women_stud`j'_`i' = r(N)
	}
}

* Create a graph
clear
set obs 14

gen year = .  
gen gender = . 
forvalues j = 1/8 {
	gen unemp_title`j' = . 
}

forvalues i = 15/21 {
	forvalues j = 1/8 {
		local year = 2000 + `i'
    
		local row_men = 2 * (`i' - 15) + 1 
		local row_women = 2 * (`i' - 15) + 2 

		replace year = `year' in `row_men'
		replace gender = 0 in `row_men'
		replace unemp_title`j' = `=scalar(unemp_men_stud`j'_`i')' in `row_men'

		replace year = `year' in `row_women'
		replace gender = 1 in `row_women'
		replace unemp_title`j' = `=scalar(unemp_women_stud`j'_`i')' in `row_women'
	}
}
list

* Women graph
twoway (line unemp_title1 year if gender == 1) (line unemp_title2 year if gender == 1) (line unemp_title3 year if gender == 1) (line unemp_title4 year if gender == 1) (line unemp_title5 year if gender == 1) (line unemp_title6 year if gender == 1) (line unemp_title7 year if gender == 1) (line unemp_title8 year if gender == 1),legend(order(1 "No qualifications" 2 "Literates no qualifications" 3 "Elementary school diploma" 4 "Middle school diploma" 5 "High school diploma" 6 "Bachelor degree" 7 "Master degree" 8 "PhD")) title("Number of Women with no income" "based on Education in Italy (2015-2021)", color(red) margin(large)) xlabel(2015(1)2021) ytitle("") xtitle("Year") plotregion(margin(large))

graph export "/Users/eugenio/Desktop/ISTAT/Analisi/Educ_Women.pdf", as(pdf) replace

* Men graph
twoway (line unemp_title1 year if gender == 0) (line unemp_title2 year if gender == 0) (line unemp_title3 year if gender == 0) (line unemp_title4 year if gender == 0) (line unemp_title5 year if gender == 0) (line unemp_title6 year if gender == 0) (line unemp_title7 year if gender == 0) (line unemp_title8 year if gender == 0),legend(order(1 "No qualifications" 2 "Literates no qualifications" 3 "Elementary school diploma" 4 "Middle school diploma" 5 "High school diploma" 6 "Bachelor degree" 7 "Master degree" 8 "PhD")) title("Number of Men with no income" "based on Education in Italy (2015-2021)", color(red) margin(large)) xlabel(2015(1)2021) ytitle("") xtitle("Year") plotregion(margin(large))

graph export "/Users/eugenio/Desktop/ISTAT/Analisi/Educ_Men.pdf", as(pdf) replace


//
** No income age and gender
clear all
* Import data again
use Income.dta, replace
* Make SEX a numerical variable
gen sesso_num = .
replace sesso_num = 0 if sesso == "M"
replace sesso_num = 1 if sesso == "F"

forvalues i = 15/21 {
	forvalues j = 3/8 {
		count if (sesso_num==0 & (y_dispo`i'==. | y_dispo`i'==0) & classeta ==`j')
		scalar unemp_men_age`j'_`i' = r(N)
		count if (sesso_num==1 & (y_dispo`i'==. | y_dispo`i'==0) & classeta ==`j')
		scalar unemp_women_age`j'_`i' = r(N)
	}
}

* Create a graph
clear
set obs 14

gen year = .  
gen gender = . 
forvalues j = 3/8 {
	gen unemp_age`j' = . 
}

forvalues i = 15/21 {
	forvalues j = 3/8 {
		local year = 2000 + `i'
    
		local row_men = 2 * (`i' - 15) + 1 
		local row_women = 2 * (`i' - 15) + 2 

		replace year = `year' in `row_men'
		replace gender = 0 in `row_men'
		replace unemp_age`j' = `=scalar(unemp_men_age`j'_`i')' in `row_men'

		replace year = `year' in `row_women'
		replace gender = 1 in `row_women'
		replace unemp_age`j' = `=scalar(unemp_women_age`j'_`i')' in `row_women'
	}
}
list

* Women graph
twoway (line unemp_age3 year if gender == 1) (line unemp_age4 year if gender == 1) (line unemp_age5 year if gender == 1) (line unemp_age6 year if gender == 1) (line unemp_age7 year if gender == 1) (line unemp_age8 year if gender == 1),legend(order(1 "25-34" 2 "35-44" 3 "45-54" 4 "55-64" 5 "65-74" 6 "> 75")) title("Number of Women with no income" "based on Age in Italy (2015-2021)", color(red) margin(large)) xlabel(2015(1)2021) ytitle("") xtitle("Year") plotregion(margin(large))

graph export "/Users/eugenio/Desktop/ISTAT/Analisi/Age_Women.pdf", as(pdf) replace

* Men graph
twoway (line unemp_age3 year if gender == 0) (line unemp_age4 year if gender == 0) (line unemp_age5 year if gender == 0) (line unemp_age6 year if gender == 0) (line unemp_age7 year if gender == 0) (line unemp_age8 year if gender == 0),legend(order(1 "25-34" 2 "35-44" 3 "45-54" 4 "55-64" 5 "65-74" 6 "> 75"))  title("Number of Men with no income" "based on Education in Italy (2015-2021)", color(red) margin(large)) xlabel(2015(1)2021) ytitle("") xtitle("Year") plotregion(margin(large))

graph export "/Users/eugenio/Desktop/ISTAT/Analisi/Age_Men.pdf", as(pdf) replace


//
** No income geography and gender
clear all
* Import data again
use Income.dta, replace

forvalues i = 15/21 {
	forvalues j = 1/4 {
		count if (sesso_num==0 & (y_dispo`i'==. | y_dispo`i'==0) & rip4 ==`j')
		scalar unemp_men_rip`j'_`i' = r(N)
		count if (sesso_num==1 & (y_dispo`i'==. | y_dispo`i'==0) & rip4 ==`j')
		scalar unemp_women_rip`j'_`i' = r(N)
	}
}

* Create a graph
clear
set obs 14

gen year = .  
gen gender = . 
forvalues j = 1/4 {
	gen unemp_rip`j' = . 
}

forvalues i = 15/21 {
	forvalues j = 1/4 {
		local year = 2000 + `i'
    
		local row_men = 2 * (`i' - 15) + 1 
		local row_women = 2 * (`i' - 15) + 2 

		replace year = `year' in `row_men'
		replace gender = 0 in `row_men'
		replace unemp_rip`j' = `=scalar(unemp_men_rip`j'_`i')' in `row_men'

		replace year = `year' in `row_women'
		replace gender = 1 in `row_women'
		replace unemp_rip`j' = `=scalar(unemp_women_rip`j'_`i')' in `row_women'
	}
}
list

twoway (line unemp_rip1 year if gender == 1) (line unemp_rip2 year if gender == 1) (line unemp_rip3 year if gender == 1) (line unemp_rip4 year if gender == 1) (line unemp_rip1 year if gender == 0, lpattern(dash)) (line unemp_rip2 year if gender == 0, lpattern(dash)) (line unemp_rip3 year if gender == 0, lpattern(dash)) (line unemp_rip4 year if gender == 0, lpattern(dash)),legend(order(1 "North-East (Women)" 2 "Nort-West (Women)" 3 "Centre (Women)" 4 "South (Women)" 5 "North-Est (Men)" 6 "North-West (Men)" 7 "Centre (Men)" 8 "South (Men)")) title("Number of People with no income" "based on Age and Gender in Italy (2015-2021)", color(red) margin(large)) xlabel(2015(1)2021) ytitle("") xtitle("Year") plotregion(margin(large))

graph export "/Users/eugenio/Desktop/ISTAT/Analisi/Geo_Gender.pdf", as(pdf) replace



/// **********
** GENDER GAP

** Gender gap in education
clear all
* Import data again
use Income.dta, replace

forvalues i = 1/8{
	count if (sesso_num==0 & titostud==`i')
	scalar stud_men`i' = r(N)
	count if (sesso_num==1 & titostud==`i')
	scalar stud_women`i' = r(N)
}
forvalues i = 1/8 {
    generate weight_men`i' = coef if (sesso_num==0 & titostud==`i')
    sum weight_men`i'
    scalar stud_men`i' = r(sum)

    generate weight_women`i' = coef if (sesso_num==1 & titostud==`i')
    sum weight_women`i'
    scalar stud_women`i' = r(sum)
}

* Create a graph
clear
set obs 8
gen stud_men = .
gen stud_women = .
gen title = _n
label define titoli 1 "No qualifications" 2 "Literates no qualifications" 3 "Elementary school diploma" 4 "Middle school diploma" 5 "High school diploma" 6 "Bachelor degree" 7 "Master degree" 8 "PhD"
label values title titoli

forvalues i = 1/8 {
    replace stud_men = stud_men`i' in `i'
    replace stud_women = stud_women`i' in `i'
}

graph bar (asis) stud_men stud_women, over(title, label(angle(45))) legend(label(1 "Men") label(2 "Women")) ytitle(" ") ylabel(,format(%9.0f)) plotregion(margin(large))

graph export "/Users/eugenio/Desktop/ISTAT/Analisi/Gender_Gap_Educ2.pdf", as(pdf) replace


//
** Gender gap in mean income
clear all
* Import data again
use Income.dta, replace

forvalues i = 15/21{
	mean y_dispoR`i' [aweight=coef] if !missing(y_dispoR`i',coef), over(sesso_num)
	scalar mean_dispo_men`i' = r(table)[1,1]
	scalar mean_dispo_women`i' = r(table)[1,2]
}

* Create a graph
clear
set obs 7
gen year = 2015 + _n - 1
gen mean_men = .
gen mean_women = .

forvalues i = 15/21 {
	replace mean_men = scalar(mean_dispo_men`i') if year == 20`i'
	replace mean_women = scalar(mean_dispo_women`i') if year == 20`i'
}

twoway (line mean_men year, lcolor(blue) lpattern(solid)) (line mean_women year, lcolor(red) lpattern(solid)), legend(order(1 "Men" 2 "Women")) xlabel(2015(1)2021) ylabel(10000(3000)25000) ytitle("") xtitle("Year")

graph export "/Users/eugenio/Desktop/ISTAT/Analisi/Income_Gender_Gap.pdf", as(pdf) replace



/// ***************************************
** EDUCATIONAL INEQUALITY ON AVERAGE INCOME

clear all
* Import data again
use Income.dta, replace

forvalues i = 15/21 {
	forvalues j = 1/8 {
		mean y_dispoR`i' [aweight=coef] if titostud == `j' & !missing(y_dispoR`i', coef)
		scalar mean_dispo_`i'stud`j' = r(table)[1,1]
	}
}

* Create graph
clear
set obs 7
gen year = 2015 + _n - 1

forvalues i = 1/8 { 
	gen mean_stud`i' = .
}

forvalues i = 15/21 {
	forvalues j = 1/8 {
		replace mean_stud`j' = scalar(mean_dispo_`i'stud`j') if year == 20`i'
	}
}
list

twoway (line mean_stud1 year) (line mean_stud2 year) (line mean_stud3 year) (line mean_stud4 year) (line mean_stud5 year) (line mean_stud6 year) (line mean_stud7 year) (line mean_stud8 year),legend(order(1 "No qualifications" 2 "Literates" "no qualifications" 3 "Elementary" "school diploma" 4 "Middle" "school diploma" 5 "High" "school diploma" 6 "Bachelor degree" 7 "Master degree" 8 "PhD")) xlabel(2015(1)2021) ylabel(9000(3000)35000) ytitle("") xtitle("Year")

graph export "/Users/eugenio/Desktop/ISTAT/Analisi/Educ_Ineq.pdf", as(pdf) replace



/// ***********************
** INCOME INEQUALITY BY AGE
clear all
* Import data again
use Income.dta, replace

forvalues i = 15/21 {
	forvalues j = 1/8 {
		mean y_dispoR`i' [aweight=coef] if classeta == `j' & !missing(y_dispoR`i', coef)
		scalar mean_dispo_`i'age`j' = r(table)[1,1]
	}
}

* Create a graph
clear
set obs 7
gen year = 2015 + _n - 1
forvalues i = 1/8 { 
	gen mean_age`i' = .
}

forvalues i = 15/21 {
	forvalues j = 1/8 {
		replace mean_age`j' = scalar(mean_dispo_`i'age`j') if year == 20`i'
	}
}
list

twoway (line mean_age1 year) (line mean_age2 year) (line mean_age3 year) (line mean_age4 year) (line mean_age5 year) (line mean_age6 year) (line mean_age7 year) (line mean_age8 year),legend(order(1 "0-14" 2 "15-24" 3 "25-34" 4 "35-44" 5 "45-54" 6 "55-64" 7 "65-74" 8 "> 75")) xlabel(2015(1)2021) ylabel(500(3000)25000) ytitle("") xtitle("Year")

graph export "/Users/eugenio/Desktop/ISTAT/Analisi/Age_Ineq.pdf", as(pdf) replace



/// **************************
** INCOME INEQUALITY GEOGRAPHY
clear all
* Import data again
use Income.dta, replace

forvalues i = 1/4 {
	forvalues j = 15/21 {
    mean y_dispoR`j' [aweight=coef] if rip4 == `i' & !missing(y_dispoR`j', coef)
    scalar mean_dispo_`j'rip`i' = r(table)[1,1]
	}
}

* Create a graph
clear
set obs 7
gen year = 2015 + _n - 1
forvalues i = 1/4 { 
	gen mean_rip`i' = .
}
forvalues i = 1/4 {
	forvalues j = 15/21 {
	replace mean_rip`i' = scalar(mean_dispo_`j'rip`i') if year == 20`j'
	}
}
list

twoway (line mean_rip1 year) (line mean_rip2 year) (line mean_rip3 year) (line mean_rip4 year),legend(order(1 "North-East" 2 "North-West" 3 "Centre" 4 "South")) xlabel(2015(1)2021) ylabel(12000(3000)25000) ytitle("") xtitle("Year")

graph export "/Users/eugenio/Desktop/ISTAT/Analisi/Geo_Ineq.pdf", as(pdf) replace


/// ****************************
** INCOME INEQUALITY CITIZENSHIP
clear all
* Import data again
use Income.dta, replace

forvalues i = 1/6 {
	forvalues j = 15/21 {
    mean y_dispo`j' [aweight=coef] if cittad == `i' & !missing(y_dispo`j', coef)
    scalar mean_dispo_`j'cittad`i' = r(table)[1,1]
	}
}

* Create a graph
clear
set obs 7
gen year = 2015 + _n - 1
forvalues i = 1/6 { 
	gen mean_cittad`i' = .
}
forvalues i = 1/6 {
	forvalues j = 15/21 {
	replace mean_cittad`i' = scalar(mean_dispo_`j'cittad`i') if year == 20`j'
	}
}
list

twoway (line mean_cittad1 year) (line mean_cittad2 year) (line mean_cittad3 year) (line mean_cittad4 year) (line mean_cittad5 year) (line mean_cittad6 year),legend(order(1 "Italian" 2 "UE" 3 "Extra-UE" 4 "Africa" 5 "Asia" 6 "Rest of the world")) xlabel(2015(1)2021) ylabel(7000(3000)23000) ytitle("") xtitle("Year")

graph export "/Users/eugenio/Desktop/ISTAT/Analisi/Citizenship_Ineq.pdf", as(pdf) replace



/// ****************************
** INEQUALITY ON TYPE OF INCOME
clear all
* Import data again
use Income.dta, replace

forvalues i = 15/21 {
	mean ye1_lav_dip`i' [aweight=coef] if !missing(ye1_lav_dip`i',coef) & ye1_lav_dip`i'>0
	scalar mean_dip`i' = r(table)[1,1]
	mean ye2_red_pen`i' [aweight=coef] if !missing(ye2_red_pen`i',coef) & ye2_red_pen`i'>0
	scalar mean_pen`i' = r(table)[1,1]
	mean ye3_lav_aut`i' [aweight=coef] if !missing(ye3_lav_aut`i',coef) & ye3_lav_aut`i'>0
	scalar mean_aut`i' = r(table)[1,1]
}

* Graph
clear
set obs 7
gen year = 2015 + _n - 1
gen mean_dip = .
gen mean_pen = .
gen mean_aut = .

forvalues i = 15/21 {
	replace mean_dip = scalar(mean_dip`i') if year == 20`i'
	replace mean_pen = scalar(mean_pen`i') if year == 20`i'
	replace mean_aut = scalar(mean_aut`i') if year == 20`i'
}
list

twoway (line mean_dip year) (line mean_pen year) (line mean_aut year),legend(order(1 "Employees" 2 "Retirees" 3 "Self-employed")) xlabel(2015(1)2021) ylabel(10000(3000)25000) ytitle("") xtitle("Year")

graph export "/Users/eugenio/Desktop/ISTAT/Analisi/Occupation_Ineq.pdf", as(pdf) replace



/// **************
** TAX BURDEN
clear all
* Import data again
use Income.dta, replace

** Absolute value of tax burden
forvalues i = 15/21 {
	gen tax_burden`i'= (y_red_tote`i' - y_dispo`i')
}
forvalues i = 15/21 {
	mean tax_burden`i' [aweight=coef]
	scalar mean_tax`i' = r(table)[1,1]
}

* Graph
clear
set obs 7
gen year = 2015 + _n - 1
gen mean_tax = .

forvalues i = 15/21 {
	replace mean_tax = scalar(mean_tax`i') if year == 20`i'
}
list

graph bar mean_tax, over(year) title("Average Tax Burden in Italy (2015-2021)", color(red)) ylabel(0(1000)5000) ytitle(" ") blabel(bar, size(medium) position(outside)) bar(1, color(orange))

graph export "/Users/eugenio/Desktop/ISTAT/Analisi/Average_Tax.pdf", as(pdf) replace


//
** Tax burden as a share of income
clear all
* Import data again
use Income.dta, replace

forvalues i = 15/21 {
	gen tax_burden`i'= (y_red_tote`i' - y_dispo`i') if y_red_tote`i' > 0 & y_dispo`i' > 0
}
forvalues i = 15/21 {
	gen share_tax_gross`i'= (tax_burden`i' / y_red_tote`i') if tax_burden`i'>= 0 & y_red_tote`i'> 0
}
forvalues i = 15/21 {
	mean share_tax_gross`i' [aweight=coef]
	scalar mean_share_tax`i' = r(table)[1,1]
}

* Create a table
clear
set obs 7 
gen year = 2015 + _n - 1  
gen tax_share = . 
forvalues i = 15/21{
	replace tax_share = scalar(mean_share_tax`i') if year == 20`i'
}
replace tax_share = tax_share * 100
format tax_share %9.2f 

asdoc list year tax_share, replace




/// ********************
** INFERENTIAL ANALYSIS
clear all
* Import data again
use Income.dta, replace

* Aggregate variables: From rip4 to rip2
gen rip2 = .
replace rip2 = 0 if rip4 == 1 | rip4 == 2
replace rip2 = 1 if rip4 == 3 | rip4 == 4
label define region_lbl 0 "North" 1 "South"
label values rip2 region_lbl
tabulate rip2

* Division low educated up to High School Diploma to high educated from Bachelor's to PhD
gen titostud2 = .
replace titostud2 = 0 if titostud <= 5 & !missing(titostud)
replace titostud2 = 1 if titostud >= 6 & !missing(titostud)
label define stud_lbl 0 "Low" 1 "High"
label values titostud2 stud_lbl
tabulate titostud2

* Division age
gen eta3 = .
replace eta3 = 0 if classeta <= 3
replace eta3 = 1 if classeta >= 4 & classeta<= 6
replace eta3 = 2 if classeta >= 7
label define eta_lbl 0 "Young" 1 "Adult" 2 "Elderly"
label values eta3 eta_lbl
tabulate eta3

save Income.dta, replace

local varlist sesso_num rip2 titostud2 eta3

foreach var of local varlist {
    display "Creating dummies for: `var'"
    qui tabulate `var', generate(dummy_`var')
    local i = 1
    foreach level of varlist dummy_`var'* {
        label variable `level' "`var' == `i'"
        local ++i
    }
}


//
* T-tests
ttest y_dispoR21, by(sesso_num)
ttest y_dispoR21, by(rip2)
ttest y_dispoR21, by(titostud2)

//
** LASSO Method
lasso linear y_dispoR21 y_dispoR20 y_dispoR19 y_dispoR18 y_dispoR17 y_dispoR16 y_dispoR15 dummy_sesso_num1 dummy_rip21 dummy_titostud21 dummy_eta31 dummy_eta32
lassocoef

//
* Regression on disposable income
reg y_dispoR21 y_dispoR20 y_dispoR19 y_dispoR18 y_dispoR17 y_dispoR16 y_dispoR15 dummy_sesso_num1 dummy_rip21 dummy_titostud21 dummy_eta31 dummy_eta32

ssc install outreg2
outreg2 using regression_results.tex, replace tex


* Comment: lagged disposable income have significant effects on 2021 disposable income with earlier lags showing a potential reverse effect. Being a men leads to a 141 units increase in disposable income on average and holding everything else constant. There is no significant difference between North and South which is surprising. People with lower education have 969 units less in disposable income compared to high education. The elderlies are the category with fewer disposable income compared to adults and young.

* diagnostics
estat vif
estat hettest
estat ovtest


reg ye1_lav_dip21 ye1_lav_dip20 ye1_lav_dip19 ye1_lav_dip18 ye1_lav_dip17 ye1_lav_dip16 ye1_lav_dip15 dummy_sesso_num1 dummy_rip21 dummy_titostud21 dummy_eta31 dummy_eta32

reg ye3_lav_aut21 ye3_lav_aut20 ye3_lav_aut19 ye3_lav_aut18 ye3_lav_aut17 ye3_lav_aut16 ye3_lav_aut15 dummy_sesso_num1 dummy_rip21 dummy_titostud21 dummy_eta31 dummy_eta32

reg y_dispoR21 y_dispoR20 y_dispoR19 y_dispoR18 y_dispoR17 y_dispoR16 y_dispoR15 dummy_sesso_num1##dummy_rip21 dummy_sesso_num1##dummy_titostud21 dummy_sesso_num1##dummy_eta31 dummy_sesso_num1##dummy_eta32

outreg2 using regression_results2.tex, replace tex
outreg2 using results.doc, replace


* Comment: Males in the North earn 185.74 units more than expected from the additive effects of being male and being in the North. Males with low education earn 490.87 units less than expected from the additive effects of being male and having low education. Young males earn 1301.68 units more than expected from the additive effects of being male and being young. Adult males earn 997.35 units more than expected from the additive effects of being male and being an adult.

// ANOVA
anova y_dispoR21 sesso_num rip2 titostud2 eta3 
predict yhat
predict residuals, residuals
scatter residuals yhat


// Logistic regression
logit active21 active20 active19 active18 active17 active16 active15 dummy_sesso_num1 dummy_rip21 dummy_titostud21 dummy_eta31 dummy_eta32
esttab, eform 

* Comment: The strongest predictor of labor market activity in 2021. A one-unit increase in labor market activity in 2020 (active vs. inactive) is associated with an increase of 3.14 in the log-odds of being active in 2021. Activity from earlier years (active19 to active15) also positively predicts activity in 2021, but the influence diminishes with time (e.g., active19 = 1.14, active18 = 0.57).
* Males are more likely to be active in the labor market than females (reference category), with an increase of 0.24 in the log-odds.
* Individuals in the North are more likely to be active compared to those in the South, with a 0.11 increase in the log-odds.
* Individuals with low education are less likely to be active in the labor market compared to those with high education (reference category). The log-odds decrease by 0.77, indicating a significant disadvantage.
* Young individuals are significantly more likely to be active in the labor market compared to the elderly (reference category). The log-odds increase by 2.61.
* Adults are also more likely to be active in the labor market compared to the elderly, with a 2.12 increase in log-odds.








/// ****************************************
** NATURAL EXPERIMENT: Bonus Occupazione Sud
use Income.dta, clear

/* Context: The Bonus Occupazione Sud was an Italian employment incentive introduced to promote job creation in the southern regions of Italy, which suffered from high unemployment rates. Implemented on January 1, 2017, the policy specifically targeted regions such as Abruzzo, Molise, Campania, Puglia, Basilicata, Calabria, Sicily, and Sardinia.
It focused on unemployed individuals, particularly young people aged 16 to 34 and adults over 35 who had been unemployed for at least six months. Employers hiring eligible workers on permanent contracts, including apprenticeships, were granted a full exemption from social security contributions up to €8,060 per worker annually for a period of 12 months. Initially valid for the year 2017, the policy was extended to cover 2018 and 2019, with slight adjustments to the eligibility criteria and implementation process. The policy aimed to reduce youth and long-term unemployment while stimulating economic activity in the disadvantaged southern regions.
*/

/* Diff-in-Diff setting:
To define treatment and control groups, the variable which defines the eligibility will be rip4 since the program is geographically targeted. Individuals living in the Southern Regions (rip4 = 4) are in the treatment group, while individuals living in the Northern regions (rip4 = 1 | 2) are in the control group. We will leave apart the Centre regions because Abruzzo is classified as a central region and there could be overlapping problems. All the individuals both in the control and treatment groups should be aged 16+ and be unemployed for at least 6 months, I will take the variable out16 == 1 which signals all the individuals who received no income during 2016. 
Pre-policy period: 2015-2016 / post-policy period: 2017-2021.
DiD model: Yit = B0 + B1 Post + B2 Treat + B3 (Post * Treat) + Controls + e 
Assumptions to check: Parallel Trend Assumption to be checked with 2015 and 2016 data.
No spillovers effect could be safely assumed since people in the Northern regions, even if unemployed tend not to move to the Southern regions to work. Also taking as a control regions far away from the southern area will increase the likelihood that this spillover does not happen.
*/


**********************
* Analysis with noinc variable

** LEO ANALYSIS THE PRINCESS

* Treatment and control groups
gen treat = 0
replace treat = 1 if reg>12

* Control for possible welfare transfers
forvalues i = 15/21 {
	drop if missing(y_dispoR`i')
	drop if (ye2_red_pen`i'>0)
	drop if classeta < 3
}

* Subset
keep codind coef treat sesso_num cittad rip4 titostud2 classeta y_dispoR*

* Reshape the dataset
reshape long y_dispoR, i(codind) j(year)
gen post = year >= 17
gen interaction = treat * post
gen noinc = 0
replace noinc= 1 if (y_dispoR == 0)
gen inc = 1
replace inc = 0 if noinc == 1

save Income_long.dta, replace


preserve
collapse (sum) noinc inc, by(treat year)
gen rate = noinc/(inc + noinc)
save colldata.dta, replace
twoway (line rate year if treat == 1, sort lcolor(blue) lwidth(medium)) (line rate year if treat == 0, sort lcolor(red) lwidth(medium)), xlabel(, angle(horizontal)) ylabel(, angle(horizontal)) xline(17, lcolor(black) lwidth(medium)) legend(order(1 "Treatment" 2 "Control")) title("Total Unemployment Trends: Treatment vs. Control")

merge m:1 treat year using "colldata.dta", keep(match) nogen
reg rate treat post interaction classeta titostud2 sesso_num cittad



** EUGENIO ANALYSIS
* Drop the individuals who got self-employed income the years before and people around the pension age
forvalues i = 15/21 {
	drop if (ye2_red_pen`i'>0)
}
drop if (ye3_lav_aut15>0 | ye3_lav_aut16>0)

* Drop young individuals
drop if classeta < 3

* Drop people living in the Centre
drop if rip4 == 3

* Drop if people got income in 2016
drop if y_dispo16 != 0

* Subset
keep codind coef sesso_num cittad rip4 titostud2 classeta y_dispoR15 y_dispoR16 y_dispoR17 y_dispoR18 y_dispoR19 y_dispoR20 y_dispoR21 noinc15 noinc16 noinc17 noinc18 noinc19 noinc20 noinc21 inc15 inc16 inc17 inc18 inc19 inc20 inc21

* Generate treatment and control groups
gen treat = 1 if (rip4 == 4)
replace treat = 0 if (rip4 == 1 | rip4 == 2)
* The groups are perfectly balanced in terms of numbers.

* Reshape the dataset
reshape long y_dispoR noinc inc, i(codind) j(year)
gen post = year >= 17
gen interaction = treat * post


/* Effect on Employment */

** Parallel Trend Assumption
preserve
collapse (sum) noinc inc, by(treat year)
gen rate = noinc/(inc+noinc)
replace rate = . if (inc + noinc) == 0
save colldata.dta, replace
twoway (line rate year if treat == 1, sort lcolor(blue) lwidth(medium)) (line rate year if treat == 0, sort lcolor(red) lwidth(medium)), xlabel(, angle(horizontal)) ylabel(, angle(horizontal)) xline(17, lcolor(black) lwidth(medium)) legend(order(1 "Treatment" 2 "Control")) title("Total Unemployment Trends: Treatment vs. Control")


* Estimate the DiD model on probability of employment
merge m:1 treat year using "colldata.dta", keep(match) nogen
reg rate treat post interaction classeta titostud2 sesso_num cittad

/* Effect on Real Income */
preserve
collapse (mean) y_dispoR, by(treat year)
twoway (line y_dispoR year if treat == 1, sort lcolor(blue)) (line y_dispoR year if treat == 0, sort lcolor(red)), xlabel(, angle(horizontal)) ylabel(, angle(horizontal)) xline(17, lcolor(black) lwidth(medium)) legend(order(1 "Treatment" 2 "Control")) title("Real Income Trends: Treat vs Control")

* Estimate the effects
reg y_dispoR treat post interaction

/* The results from the real wage and employment regressions reveal important insights into the potential mechanisms of the tax exemption policy targeting Southern workers. In terms of employment, the policy likely incentivized firms to hire more workers from the South, leading to an increase in employment in the treatment group. However, this may have come at the expense of job quality or wages, as firms could use the cost savings from the tax exemption to offer lower-paying or less secure positions. This is supported by the real wage regression, which shows a significant negative treatment effect, suggesting that while the policy increased employment, it may have also contributed to wage stagnation or reductions in the South. Structural differences between the North and South, such as weaker labor markets, lower productivity, and concentration in lower-paying sectors, may have amplified these effects. Additionally, firms might have shifted hiring to maximize cost efficiencies, further dampening wage growth for Southern workers. Together, these results highlight a trade-off between quantity and quality in labor market outcomes, with increased employment potentially undermining income levels in the treatment group. */



****************
* Analysis with out variable

* Subset
keep codind coef sesso_num cittad rip4 titostud2 classeta y_dispoR15 y_dispoR16 y_dispoR17 y_dispoR18 y_dispoR19 y_dispoR20 y_dispoR21 out15 out16 out17 out18 out19 out20 out21


* Generate treatment and control groups
gen treat = 1 if (rip4 == 4 & out16 == 1 & classeta >= 3)
replace treat = 0 if ((rip4 == 1 | rip4 == 2) & out16 == 1 & classeta >= 3)
* The groups seems to be balanced in terms of numbers.

* Generate the % of those categories with respect of their population in their region
gen 

* Reshape the dataset
reshape long y_dispoR out, i(codind) j(year)

gen post = year >= 17
gen interaction = treat * post

* Keep only treatment and control groups
keep if treat == 1 | treat == 0

/* Effect on Employment */

** Parallel Trend Assumption
preserve
collapse (sum) out, by(treat year)
twoway (line out year if treat == 1, sort lcolor(blue) lwidth(medium)) (line out year if treat == 0, sort lcolor(red) lwidth(medium)), xlabel(, angle(horizontal)) ylabel(, angle(horizontal)) xline(17, lcolor(black) lwidth(medium)) legend(order(1 "Treatment" 2 "Control")) title("Total Unemployment Trends: Treatment vs. Control")

* Estimate the DiD model on probability of employment
reg out treat post interaction classeta titostud sesso_num cittad


/* Effect on Real Income */
preserve
collapse (mean) y_dispoR, by(treat year)
twoway (line y_dispoR year if treat == 1, sort lcolor(blue)) (line y_dispoR year if treat == 0, sort lcolor(red)), xlabel(, angle(horizontal)) ylabel(, angle(horizontal)) xline(17, lcolor(black) lwidth(medium)) legend(order(1 "Treatment" 2 "Control")) title("Real Income Trends: Treat vs Control")

* Estimate the effects
reg y_dispoR treat post interaction eta titostud sesso_num cittad






















