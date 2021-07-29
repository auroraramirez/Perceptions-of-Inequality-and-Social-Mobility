***
*Program to estimate social mobility and inequality
****

clear all
set rmsg on
set seed 12345
set more off
**Para que salgan figuras bonitas
grstyle init
grstyle set plain, horizontal grid


global drive="C:"
global data="$drive\Users\Aurora Ramírez\OneDrive - El Colegio de México A.C\Proyectos\AFD\Paper_2\WD\code"
global graph="$drive\Users\Aurora Ramírez\OneDrive - El Colegio de México A.C\Proyectos\AFD\Paper_2\WD\code"
global tablas="$drive\Users\Aurora Ramírez\OneDrive - El Colegio de México A.C\Proyectos\AFD\Paper_2\WD\code"

********************************************************************************
************************                                           1/2 Main text
********************************************************************************


use "$data\DesigualdadFinal.dta", clear
gen C=p30==1
gen T1=p30==2
gen T2=p30==3

**Variables creencia

mca p1_1 p1_2 p1_3 p1_4 p1_5 [fw=weight]
predict hh_index
label var hh_index "Socioeconomic Index"
**Higher hh index more wealth or socioec status

**Indice de que cree que pobre es pobre porque quiere
gen p_q43=6-p22c
gen p_q45=6-p22e
egen rowindex=rowtotal(p22a p22b p_q43 p22d p_q45)
sum rowindex [aw=weight]
gen index_beliefspoverty=(rowindex-r(mean))/r(sd)
drop p_q43 p_q45 rowindex
label var index_beliefspoverty "Index Poverty"
**Higher index means higher belief each person owns its destiny

**Colectivismo vs Individualismo
egen rowindex=rowtotal(p23a p23b p23c p23d p22e)
sum rowindex [aw=weight]
gen index_colectivism=(rowindex-r(mean))/r(sd)
drop rowindex
label var index_colectivism "Index Colectivism"
**Higher index higher belief in government

gen cd_mxcity=ciudad==2
gen cd_norte=ciudad==1|ciudad==5
gen cd_sur=ciudad==4|ciudad==7
gen cd_centro=ciudad==3|ciudad==6

**P21 gini
gen p21_gini=0.746 if p21==1
replace p21_gini=0.654 if p21==2
replace p21_gini=0.532 if p21==3
replace p21_gini=0.380 if p21==4
replace p21_gini=0.200 if p21==5
replace p21_gini=0 if p21==6

gen p33_gini=0.746 if p33==1
replace p33_gini=0.654 if p33==2
replace p33_gini=0.532 if p33==3
replace p33_gini=0.380 if p33==4
replace p33_gini=0.200 if p33==5
replace p33_gini=0 if p33==6

gen educ_universidad=educ_group==4
label var educ_universidad "% Universidad"



********************************************************************************
************************                         Table 1: Descriptive Statistics
********************************************************************************

putexcel set "$tablas\Tables.xls", sheet("table1", replace) modify 
putexcel B2= "Variable"
putexcel C2= "All"
putexcel D2= "Control"
putexcel E2= "Treatment 1"
putexcel F2= "Treatment 2"
putexcel G2= "p-value"
quietly sum female 
putexcel C3 = `r(N)'
quietly sum female if C==1
putexcel D3 = `r(N)'
quietly sum female if T1==1
putexcel E3 = `r(N)'
quietly sum female if T2==1
putexcel F3 = `r(N)'

local i=4
foreach var in female edad yrs_educ educ_universidad married work seguro hh_index index_beliefspoverty index_colectivism padres_ind padres_educbaja ////
				cd_mxcity cd_norte cd_sur cd_centro {
putexcel B`i'="`var'"
quietly sum `var'  [aw=weight]
local meanx = string(`r(mean)',"%9.2f")
local sdx = string((`r(sd)'/sqrt(`r(N)')),"%9.2f")
local meansd = "`meanx' [`sdx']"
putexcel C`i'="`meansd'"
foreach var2 in C T1 T2 {
quietly sum `var'  [aw=weight] if `var2'==1
local meanx = string(`r(mean)',"%9.2f")
local sdx = string((`r(sd)'/sqrt(`r(N)')),"%9.2f")
local meansd = "`meanx' [`sdx']"
	if "`var2'"=="C" {
	putexcel D`i'="`meansd'"
	}
	else if "`var2'"=="T1" {
	putexcel E`i'="`meansd'"
	}
	else {
	putexcel F`i'="`meansd'"
	}
}
quietly reg `var' T1 T2 [aw=weight], robust
quietly test T1=T2=0
local t=string(`r(p)', "%9.3f")
putexcel G`i'="[`t']"
local i=`i'+1
}

foreach var in p15 p17 p19 p24a p24b p25a p25b p26a p26b p27a p27b p28a p28b p29a p29b {
quietly replace `var'=`var'*10
}

save "$data\Desigualdad_temp.dta", replace

********************************************************************************
************************                                                Figure 1
********************************************************************************

use "$data\Desigualdad_temp.dta", clear

gen tax_willing=100*(p20/10000)
sum p14 [aw=weight]
local coef1 : di %6.0f r(mean)
sum p16 [aw=weight]
local coef2 : di %6.0f r(mean)
collapse (mean) p15 p17 p19 tax_willing (sd) sd_p15=p15 sd_p17=p17 sd_p19=p19 sd_tax_willing=tax_willing ////
		(count) n_p15=p15 n_p17=p17 n_p19=p19 n_tax_willing=tax_willing [aw=weight]

local j=1
foreach var in p15 p17 p19 tax_willing {
rename `var' ms`j'
rename sd_`var' sd_ms`j'
rename n_`var' n_ms`j'
local j=`j'+1
}
gen varn=1
reshape long ms sd_ms n_ms, i(varn) j(tipo)
drop varn
label var tipo "MS"
label def tipo 0 " " 1 `" "% that are poor" "' 2 `" "% that are rich" "' ////
	3 `" "Tax you pay" "' 4 `" "Willingness to give up" "from $10,000 to eliminate" "poverty and inequality" "' 5 " "
label val tipo tipo

gen icp=ms+1.96*sd_ms/sqrt(n_ms)
gen icn=ms-1.96*sd_ms/sqrt(n_ms)


gen tipo2=tipo
drop if tipo2==3 | tipo2==4
graph bar ms, over(tipo2, relabel(1 `" "% that are poor" "' 2 `" "% that are rich" "' ////
	) label(labsize(medium))) ////
	blabel(bar, size(medsmall) format(%9.1g)) ytitle("% People", size(medium)) ////
	ylabel(0(10)80, grid glwidth(medthin) glpattern(dash)) ////
	text(75 24 "Avg. Maximum income to be", place(c) size(medsmall)) ////
	text(71 24 "considered poor MXN $`coef1'", place(c) size(medsmall)) ////
	text(55 78 "Avg. Minimum income to be", place(c) size(medsmall)) ////
	text(51 78 "considered rich MXN $`coef2'", place(c) size(medsmall)) ////
	legend(off) graphregion(fcolor(white))
graph export "$graph\Fig1_Perceptions.emf", replace font("Times New Roman")



********************************************************************************
************************                                                Figure 2
********************************************************************************
use "$data\Desigualdad_temp.dta", clear
pca yrs_educ seguro hh_index padres_ind padres_educbaja ageb_graproes [aw=weight]	//localidad//
predict wealth
xtile pca_wealth=wealth [aw=weight], nq(20)
foreach var in p34a p34b p35a p35b p36a p36b p37 p38 p39 p40 {
replace `var'=`var'*10
}
collapse (mean) p14 p15 p16 p17 p21_gini p33_gini p34a p35b p27a p28b p19 p37 p38 p39 p40 wealth (count) nx=p21_gini (rawsum) weight [aw=weight], by(pca_wealth)
replace pca_wealth=(5*pca_wealth)-2.5

**Figure 2A
quietly reg p14 pca_wealth [aw=weight], rob
local coef : di %6.3f _b[pca_wealth]
local pval : di %6.3f  (2 * ttail(e(df_r), abs(_b[pca_wealth]/_se[pca_wealth])))

twoway (scatter p14 pca_wealth, sort msize(medsmall) msymbol(circle)) ////
	(lfit p14 pca_wealth [aw=weight], lwidth(medthick) lpattern(solid) lcolor(navy)) ////
	, ////
	ytitle("Max. Income to be considered poor") ////
	ytitle(, size(medium)) ylabel(1000(500)4000, grid glwidth(medthin) glpattern(dash)) ////
	xtitle("SES rank", size(medium)) ////
	xlabel(0(10)100, grid glwidth(medthin) glpattern(dash)) ////
	text(1500 75 "Coefficient `coef' [`pval']", place(c) size(medium)) ////
	legend(off) graphregion(fcolor(white))
graph export "$graph\Fig2A_Poor.emf", replace font("Times New Roman")


**Figure 2B
quietly reg p16 pca_wealth [aw=weight], rob
local coef : di %6.3f _b[pca_wealth]
local pval : di %6.3f  (2 * ttail(e(df_r), abs(_b[pca_wealth]/_se[pca_wealth])))

twoway (scatter p16 pca_wealth, sort msize(medsmall) msymbol(circle)) ////
	(lfit p16 pca_wealth [aw=weight], lwidth(medthick) lpattern(solid) lcolor(navy)) ////
	, ////
	ytitle("Min. Income to be considered rich") ////
	ytitle(, size(medium)) ylabel(25000(5000)50000, grid glwidth(medthin) glpattern(dash)) ////
	xtitle("SES rank") ////
	xtitle(, size(medium)) xlabel(0(10)100, grid glwidth(medthin) glpattern(dash)) ////
	text(35000 75 "Coefficient `coef' [`pval']", place(c) size(medium)) ////
	legend(off) graphregion(fcolor(white))
graph export "$graph\Fig2B_Rich.emf", replace font("Times New Roman")


**Figure 2C
quietly reg p15 pca_wealth [aw=weight], rob
local coef : di %6.3f _b[pca_wealth]
local pval : di %6.3f  (2 * ttail(e(df_r), abs(_b[pca_wealth]/_se[pca_wealth])))

twoway (scatter p15 pca_wealth, sort msize(medsmall) msymbol(circle)) ////
	(lfit p15 pca_wealth [aw=weight], lwidth(medthick) lpattern(solid) lcolor(navy)) ////
	, ////
	ytitle("% Perceived to be poor") ////
	ytitle(, size(medium)) ylabel(0(10)100, grid glwidth(medthin) glpattern(dash)) ////
	xtitle("SES rank") ////
	xtitle(, size(medium)) xlabel(0(10)100, grid glwidth(medthin) glpattern(dash)) ////
	text(40 75 "Coefficient `coef' [`pval']", place(c) size(medium)) ////
	legend(off) graphregion(fcolor(white))
graph export "$graph\Fig2C_Poverty.emf", replace font("Times New Roman")

**Figure 2D
quietly reg p17 pca_wealth [aw=weight], rob
local coef : di %6.3f _b[pca_wealth]
local pval : di %6.3f  (2 * ttail(e(df_r), abs(_b[pca_wealth]/_se[pca_wealth])))

twoway (scatter p17 pca_wealth, sort msize(medsmall) msymbol(circle)) ////
	(lfit p17 pca_wealth [aw=weight], lwidth(medthick) lpattern(solid) lcolor(navy)) ////
	, ////
	ytitle("% Perceived to be rich") ////
	ytitle(, size(medium)) ylabel(0(10)100, grid glwidth(medthin) glpattern(dash)) ////
	xtitle("SES rank") ////
	xtitle(, size(medium)) xlabel(0(10)100, grid glwidth(medthin) glpattern(dash)) ////
	text(50 75 "Coefficient `coef' [`pval']", place(c) size(medium)) ////
	legend(off) graphregion(fcolor(white))
graph export "$graph\Fig2D_Rich.emf", replace font("Times New Roman")





********************************************************************************
************************                                                Figure 3
********************************************************************************

use "$data\Desigualdad_temp.dta", clear
sum p21_gini [aw=weight]
local coef : di %6.3f r(mean)
sum p33_gini [aw=weight]
local coef2 : di %6.3f r(mean)
foreach var in p21 p33 {
forval j=1/6 {
gen `var'_`j'=`var'==`j'
}
}
collapse (mean) p21_1-p21_6 p33_1-p33_6 [aw=weight]
gen varn=1
reshape long p21_ p33_, i(varn) j(tipo)
replace p21_=p21_*100
replace p33_=p33_*100
rename p21_ p21
rename p33_ p33

	
graph bar p21 p33, over(tipo, relabel(1 `" "Gini=0.75," "Top 20%=92," "Bottom 20%=0.5" "' ////
	2 `" "Gini=0.65," "Top 20%=80," "Bottom 20%=1.5" "' ////
	3 `" "Gini=0.53," "Top 20%=65," "Bottom 20%=3" "' ////
	4 `" "Gini=0.38," "Top 20%=45," "Bottom 20%=5" "' ////
	5 `" "Gini=0.20," "Top 20%=30," "Bottom 20%=10" "' ////
	6 `" "Gini=0," "Top 20%=20," "Bottom 20%=20" "' ////
	) label(labsize(vsmall))) ////
	blabel(bar, size(medium) format(%9.1g)) ytitle("% People") ////
	ytitle(, size(medium)) ylabel(0(10)40, grid glwidth(medthin) glpattern(dash)) ////
	text(39 70 "Perceived Gini `coef' ", place(l) size(medium)) ////
	text(35 70 "Desired Gini `coef2' ", place(l) size(medium)) ////
	legend(order(1 "Perceived" 2 "Desired")) graphregion(fcolor(white))
graph export "$graph\Fig3_BoxInequality.emf", replace font("Times New Roman")



********************************************************************************
************************                                                Figure 4
********************************************************************************
use "$data\Desigualdad_temp.dta", clear
foreach var in p34a p34b  p35a p35b p36a p36b {
replace `var'=`var'*10
}
collapse (mean) p27a p27b  p29a p29b p28a p28b p34a p34b  p35a p35b p36a p36b (sd) sd_p27a=p27a sd_p27b=p27b sd_p29a=p29a sd_p29b=p29b sd_p28a=p28a sd_p28b=p28b ////
		sd_p34a=p34a sd_p34b=p34b sd_p35a=p35a sd_p35b=p35b sd_p36a=p36a sd_p36b=p36b (count) n_p27a=p27a n_p27b=p27b n_p29a=p29a n_p29b=p29b n_p28a=p28a n_p28b=p28b ////
		n_p34a=p34a n_p34b=p34b n_p35a=p35a n_p35b=p35b n_p36a=p36a n_p36b=p36b [aw=weight]

local j=1
foreach var in p27a p27b  p29a p29b p28a p28b {
rename `var' ms`j'
rename sd_`var' sd_ms`j'
rename n_`var' n_ms`j'
local j=`j'+1
}
local j=1
foreach var in p34a p34b p36a p36b p35a p35b {
rename `var' msd`j'
rename sd_`var' sd_msd`j'
rename n_`var' n_msd`j'
local j=`j'+1
}

gen varn=1
reshape long ms sd_ms n_ms msd sd_msd n_msd, i(varn) j(tipo)
drop varn
label var tipo "MS"
label def tipo 0 " " 1 "Q1toQ1" 2 "Q1toQ5" 3 "Q3toQ1" 4 "Q3toQ5" 5 "Q5toQ1" 6 "Q5toQ5" 7 " "
label val tipo tipo

gen icp=ms+1.96*sd_ms/sqrt(n_ms)
gen icn=ms-1.96*sd_ms/sqrt(n_ms)

gen icpd=msd+1.96*sd_msd/sqrt(n_msd)
gen icnd=msd-1.96*sd_msd/sqrt(n_msd)

gen tipo1=tipo
replace tipo1=2 if tipo==6
replace tipo1=3 if tipo==2
replace tipo1=6 if tipo==3

label def tipo1 1 "Q1toQ1" 2 "Q5toQ5" 3 "Q1toQ5" 4 "Q3toQ5" 5 "Q5toQ1" 6 "Q3toQ1" 
label val tipo1 tipo1


graph bar ms msd, over(tipo1, relabel(1 "Q1toQ1" 2 "Q5toQ5" 3 "Q1toQ5" 4 "Q3toQ5" 5 "Q5toQ1" 6 "Q3toQ1" ) label(labsize(medsmall))) ////
	blabel(bar, size(medium) format(%9.1g)) ytitle("% People") ////
	ytitle(, size(medium)) ylabel(0(10)80, grid glwidth(medthin) glpattern(dash)) ////
	legend(order(1 "Perceived" 2 "Desired")) graphregion(fcolor(white))
graph export "$graph\Fig4_SocialMobility.emf", replace font("Times New Roman")




********************************************************************************
************************                                                Figure 5
********************************************************************************

use "$data\Desigualdad_temp.dta", clear
foreach var in p34a p34b p35a p35b p36a p36b p37 p38 p39 p40 {
replace `var'=`var'*10
}
pca yrs_educ seguro hh_index padres_ind padres_educbaja ageb_graproes [aw=weight]	//localidad//
predict wealth



foreach var in p33_gini p21_gini edad yrs_educ hh_index index_beliefspoverty index_colectivism wealth ////
	p34a p34b p35a p35b p36a p36b p37 p38 p39 p40 p19 p27a p27b p28a p28b p29a p29b {
quietly sum `var' [aw=weight]
quietly gen std_`var'=(`var'-r(mean))/r(sd)	
}
foreach var in p33_gini p34a p34b p36a p36b p35a p35b p37 {
quietly sum `var' [aw=weight]
di "`var'" "ALL " r(mean) "  "  r(sd)
quietly sum `var' [aw=weight] if T1==0 & T2==0
di "`var'" "CONTROLS " r(mean) "  "  r(sd)
quietly sum std_`var' [aw=weight]
di "STD`var'" "ALL " r(mean) "  "  r(sd)
quietly sum std_`var' [aw=weight] if T1==0 & T2==0
di "STD-`var'" "CONTROLS " r(mean) "  "  r(sd)
}


*****
**High Low Median
*****
quietly sum index_colectivism [aw=weight], det
gen median1=index_colectivism>r(p50)
quietly sum index_beliefspoverty [aw=weight], det
gen median2=index_beliefspoverty>r(p50)


***Plotting T1 / T2
quietly eststo A_p33_gini: reg std_p33_gini T1 T2 std_p21_gini female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight], robust
**Q1-Q1
quietly eststo A_p34a: reg std_p34a T1 T2 std_p27a female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight], robust
**Q1-Q5
quietly eststo A_p34b: reg std_p34b T1 T2 std_p27b female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight], robust
**Q3-Q1
quietly eststo A_p36a: reg std_p36a T1 T2 std_p29a female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight], robust
**Q3-Q5
quietly eststo A_p36b: reg std_p36b T1 T2 std_p29b female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight], robust
**Q5-Q1
quietly eststo A_p35a: reg std_p35a T1 T2 std_p28a female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight], robust
**Q5-Q5
quietly eststo A_p35b: reg std_p35b T1 T2 std_p28b female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight], robust
**Taxes
quietly eststo A_p37: reg std_p37 T1 T2 std_p19 female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight], robust


***Order "Q1toQ1" 2 "Q5toQ5" 3 "Q1toQ5" 4 "Q3toQ5" 5 "Q5toQ1" 6 "Q3toQ1" 
***check https://boris.unibe.ch/116570/1/Jann-2018-grstyle-set.pdf

coefplot (A_p33_gini, keep(T1) \ A_p34a, keep(T1) \ A_p35b, keep(T1)  \ A_p34b, keep(T1) \ A_p36b, keep(T1) \ A_p35a, keep(T1) \ A_p36a, keep(T1) \ A_p37, keep(T1)) || ////
		(A_p33_gini, keep(T2) \ A_p34a, keep(T2) \ A_p35b, keep(T2)  \ A_p34b, keep(T2) \ A_p36b, keep(T2) \ A_p35a, keep(T2) \ A_p36a, keep(T2) \ A_p37, keep(T2)) || ////
		, xline(0) aseq swapnames ////
			coeflabels(A_p33_gini = "Gini" A_p34a = "Q1-Q1" A_p34b = "Q1-Q5" A_p36a = "Q3-Q1" A_p36b = "Q3-Q5" A_p35a = "Q5-Q1" A_p35b="Q5-Q5" A_p37="Taxes") ////
			graphregion(fcolor(white))  ci(95)  bylabels("Treatment 1: Inequality" "Treatment 2: Social Mobility")  ////
			xlabel(-0.30(0.10)0.30, grid glwidth(medthin) glpattern(dash)) ylabel(, nogrid)
graph export "$graph\Fig5_T1T2.emf", replace font("Times New Roman")




********************************************************************************
************************                                                Figure 6
********************************************************************************
use "$data\Desigualdad_temp.dta", clear
pca yrs_educ seguro hh_index padres_ind padres_educbaja ageb_graproes [aw=weight]	//localidad//
predict wealth
xtile pca_wealth=wealth [aw=weight], nq(20)
foreach var in p34a p34b p35a p35b p36a p36b p37 p38 p39 p40 {
replace `var'=`var'*10
}
collapse (mean) p20 p14 p15 p16 p17 p21_gini p33_gini p34a p35b p27a p28b p19 p37 p38 p39 p40 wealth (count) nx=p21_gini (rawsum) weight [aw=weight], by(pca_wealth)
replace pca_wealth=(5*pca_wealth)-2.5

sum p19 [aw=weight]
quietly reg p19 pca_wealth [aw=weight], rob
local coef : di %6.3f _b[pca_wealth]
local pval : di %6.3f  (2 * ttail(e(df_r), abs(_b[pca_wealth]/_se[pca_wealth])))
sum p37 [aw=weight]
quietly reg p37 pca_wealth [aw=weight], rob
local coef2 : di %6.3f _b[pca_wealth]
local pval2 : di %6.3f  (2 * ttail(e(df_r), abs(_b[pca_wealth]/_se[pca_wealth])))


** Figure 6A
twoway (scatter p19 pca_wealth, sort msize(medsmall) msymbol(circle)) ////
	(scatter p37 pca_wealth, sort msize(medsmall) msymbol(square)) ////
	(lfit p19 pca_wealth [aw=weight], lwidth(medthick) lpattern(solid) lcolor(navy)) ////
	(lfit p37 pca_wealth [aw=weight], lwidth(medthick) lpattern(solid) lcolor(maroon)), ////
	ytitle("% Taxes") ////
	ytitle(, size(medium)) ylabel(0(20)70, grid glwidth(medthin) glpattern(dash)) ////
	xtitle("SES rank") ////
	xtitle(, size(medium)) xlabel(0(10)100, grid glwidth(medthin) glpattern(dash)) ////
	text(60 75 "Coefficient `coef' [`pval']", place(c) size(medium)) ////
	text(10 75 "Coefficient `coef2' [`pval2']", place(c) size(medium)) ////
	legend(order(1 "Perceived" 2 "Desired")) graphregion(fcolor(white))
graph export "$graph\Fig6A_Own_Wealth.emf", replace font("Times New Roman")


** Figure 6B: Other taxes
sum p39 [aw=weight]
quietly reg p39 pca_wealth [aw=weight], rob
local coef : di %6.3f _b[pca_wealth]
local pval : di %6.3f  (2 * ttail(e(df_r), abs(_b[pca_wealth]/_se[pca_wealth])))
sum p40 [aw=weight]
quietly reg p40 pca_wealth [aw=weight], rob
local coef2 : di %6.3f _b[pca_wealth]
local pval2 : di %6.3f  (2 * ttail(e(df_r), abs(_b[pca_wealth]/_se[pca_wealth])))
sum p38 [aw=weight]
quietly reg p38 pca_wealth [aw=weight], rob
local coef3 : di %6.3f _b[pca_wealth]
local pval3 : di %6.3f  (2 * ttail(e(df_r), abs(_b[pca_wealth]/_se[pca_wealth])))

twoway (scatter p39 pca_wealth, sort msize(medsmall) msymbol(circle)) ////
	(scatter p40 pca_wealth, sort msize(medsmall) msymbol(square)) ////
	(scatter p38 pca_wealth, sort msize(medsmall) msymbol(diamond) mcolor(gray)) ////
	(lfit p39 pca_wealth [aw=weight], lwidth(medthick) lpattern(solid) lcolor(navy)) ////
	(lfit p40 pca_wealth [aw=weight], lwidth(medthick) lpattern(solid) lcolor(maroon)) ////
	(lfit p38 pca_wealth [aw=weight], lwidth(medthick) lpattern(solid) lcolor(gray)), ////
	ytitle("% Taxes") ////
	ytitle(, size(medium)) ylabel(0(20)70, grid glwidth(medthin) glpattern(dash)) ////
	xtitle("SES rank") ////
	xtitle(, size(medium)) xlabel(0(10)100, grid glwidth(medthin) glpattern(dash)) ////
	text(10 75 "Coefficient `coef' [`pval']", place(c) size(medium)) ////
	text(28 65 "Coefficient `coef2' [`pval2']", place(c) size(medium)) ////
	text(55 75 "Coefficient `coef3' [`pval3']", place(c) size(medium)) ////
	legend(order(1 "Poor" 2 "Median Income" 3 "Rich") rows(1)) graphregion(fcolor(white))
graph export "$graph\Fig6B_Other_Wealth.emf", replace font("Times New Roman")





********************************************************************************
************************                              2/2 SUPPLEMENTARY MATERIAL
********************************************************************************


********************************************************************************
************************                                               Figure A1
********************************************************************************


use "$data\Desigualdad_temp.dta", clear
graph box p15 p17 [aw=weight], showyvars ////
	yvaroptions(relabel(1 `" "% that are poor" "' 2 `" "% that are rich" "') label(labsize(medium))) ////
	box(1, fcolor(navy) lcolor(black)) ////
	box(2, fcolor(maroon) lcolor(black)) ////
	nooutsides ytitle("% People", size(medium))  ////
	ylabel(0(10)100, grid glwidth(medthin) glpattern(dash)) ////
	legend(off) ////
	graphregion(fcolor(white)) note("")
graph export "$graph\FigA1_BoxPerceptions1.emf", replace font("Times New Roman")

********************************************************************************
************************                                               Figure A2
********************************************************************************

sum p14 [aw=weight], det
local coef1 : di %6.0f r(p50)
gen p14_med=p14/r(p50)
sum p16 [aw=weight], det
local coef2 : di %6.0f r(p50)
gen p16_med=p16/r(p50)

graph box p14_med p16_med [aw=weight], showyvars ////
	yvaroptions(relabel(1 `" "Income to be poor" "' 2 `" "Income to be rich" "') label(labsize(medium))) ////
	box(1, fcolor(navy) lcolor(black)) ////
	box(2, fcolor(maroon) lcolor(black)) ////
	nooutsides ytitle("Income relative to median", size(medium))  ////
	ylabel(, grid glwidth(medthin) glpattern(dash)) ////
	legend(off) ////
	text(2.8 27 "Median income $`coef1' ", place(c) size(medsmall)) ////
	text(3.8 75 "Median income $`coef2'", place(c) size(medsmall)) ////
	graphregion(fcolor(white)) note("")
graph export "$graph\FigA2_BoxPercepetions2.emf", replace font("Times New Roman")


********************************************************************************
************************                                               Figure A3
********************************************************************************

use "$data\Desigualdad_temp.dta", clear
recode p31 (2=0) 
recode p32 (2=0)
collapse (mean) p21_gini p21 p33_gini p33 p31 p32 p34a p34b p35a p35b p36a p36b p37 p38 p39 p40 ////
			(sd) sd_p21_gini=p21_gini sd_p21=p21 sd_p33_gini=p33_gini sd_p33=p33 sd_p31=p31 sd_p32=p32 sd_p34a=p34a ////
			sd_p34b=p34b sd_p35a=p35a sd_p35b=p35b sd_p36a=p36a sd_p36b=p36b sd_p37=p37 sd_p38=p38 sd_p39=p39 sd_p40=p40 ////
			(count) n_p21_gini=p21_gini n_p21=p21 n_p33_gini=p33_gini n_p33=p33 n_p31=p31 n_p32=p32 n_p34a=p34a ////
			n_p34b=p34b n_p35a=p35a n_p35b=p35b n_p36a=p36a n_p36b=p36b n_p37=p37 n_p38=p38 n_p39=p39 n_p40=p40 [aw=weight], by(p30)
foreach var in p21_gini p21 p33_gini p33 p31 p32 p34a p34b p35a p35b p36a p36b p37 p38 p39 p40 {
quietly gen icp_`var'=`var'+1.96*(sd_`var'/sqrt(n_`var'))
quietly gen icn_`var'=`var'-1.96*(sd_`var'/sqrt(n_`var'))
drop sd_`var' n_`var'
}
label define p30 0 " " 1 "Control" 2 `" "Treatment 1:" "Inequality" "' 3 `" "Treatment 2:" "Mobility" "' 4 " "
label val p30 p30


gen grupoj1=p30-0.25
gen grupoj2=p30
gen grupoj3=p30+0.25

label define grupoj1 0 " " 1 "Control" 2 `" "Treatment 1:" "Inequality" "' 3 `" "Treatment 2:" "Mobility" "' 4 " "
label val grupoj1 grupoj1

foreach var in p31 p32 {
quietly replace `var'=`var'*100
quietly replace icp_`var'=icp_`var'*100
quietly replace icn_`var'=icn_`var'*100
}

foreach var in p34a p34b p35a p35b p36a p36b p37 p38 p39 p40 {
quietly replace `var'=`var'*10
quietly replace icp_`var'=icp_`var'*10
quietly replace icn_`var'=icn_`var'*10
}

twoway (rcap icp_p33_gini icn_p33_gini p30, sort msize(medium) lcolor(navy)) ////
		(scatter p33_gini p30, sort msize(medsmall) mcolor(navy) msymbol(triangle)), ////
	ytitle("Mean and 95% CI") ////
	ylabel(0.2(0.05)0.4, grid glpattern(dash)) xlabel(0(1)4, labels labsize(small) ////
	valuelabel) xtitle(" ") ////
	graphregion(fcolor(white)) ////
	legend(off)
graph export "$graph\FigA3_Exp_Inequality.emf", replace font("Times New Roman")

********************************************************************************
************************                                               Figure A4
********************************************************************************

***Mobility
*Persistence
twoway (rcap icp_p34a icn_p34a grupoj1, sort msize(medium) lcolor(navy)) (rcap icp_p35b icn_p35b grupoj3, sort msize(medium) lcolor(maroon)) ////
		(scatter p34a grupoj1, sort msize(medium) mcolor(navy) msymbol(triangle)) ////
		(scatter p35b grupoj3, sort msize(medsmall) mcolor(maroon) msymbol(circle)), ////
	ytitle("Mean and 95% CI") ////
	ylabel(0(10)70, grid glpattern(dash)) xlabel(0(1)4, labels labsize(small) ////
	valuelabel) graphregion(fcolor(white)) ////
	legend(order(3 "Persistence for poor (Q1-Q1)" 4 "Persistence for rich (Q5-Q5)") rows(2) region(c(none) margin(medsmall)))
graph export "$graph\FigA4A_Exp_Mob_Persist.emf", replace font("Times New Roman")


gen grupok1=p30-0.40
gen grupok2=p30-0.20
gen grupok3=p30
gen grupok4=p30+0.20

label define grupok1 0 " " 1 "Control" 2 `" "Treatment 1:" "Inequality" "' 3 `" "Treatment 2:" "Mobility" "' 4 " "
label val grupok1 grupok1

*Upward and dowward mobility
twoway (rcap icp_p34b icn_p34b grupok1, sort msize(medium) lcolor(navy)) (rcap icp_p35a icn_p35a grupok2, sort msize(medium) lcolor(maroon)) ////
		(rcap icp_p36a icn_p36a grupok3, sort msize(medium) lcolor(maroon)) (rcap icp_p36b icn_p36b grupok4, sort msize(medium) lcolor(navy)) ////
		(scatter p34b grupok1, sort msize(medium) mcolor(navy) msymbol(triangle)) ////
		(scatter p35a grupok2, sort msize(medsmall) mcolor(maroon) msymbol(circle)) ////
		(scatter p36a grupok3, sort msize(medium) mcolor(maroon) msymbol(square)) ////
		(scatter p36b grupok4, sort msize(medsmall) mcolor(navy) msymbol(+)), ////
	ytitle("Mean and 95% CI") ////
	ylabel(20(10)80, grid glpattern(dash)) xlabel(0(1)4, labels labsize(small) ////
	valuelabel) graphregion(fcolor(white)) ////
	legend(order(5 "Upward for poor (Q1-Q5)" 6 "Downward for rich (Q5-Q1)" ////
	7 "Downward for median (Q3-Q1)" 8 "Upward for median (Q3-Q5)") rows(2) region(c(none) margin(medsmall)))
graph export "$graph\FigA4B_Exp_Mob_DownUp.emf", replace font("Times New Roman")


********************************************************************************
************************                                               Figure A5
********************************************************************************


twoway (rcap icp_p37 icn_p37 grupok1, sort msize(medium) lcolor(navy)) (rcap icp_p39 icn_p39 grupok2, sort msize(medium) lcolor(maroon)) ////
		(rcap icp_p40 icn_p40 grupok3, sort msize(medium) lcolor(brown)) (rcap icp_p38 icn_p38 grupok4, sort msize(medium) lcolor(gray)) ////
		(scatter p37 grupok1, sort msize(medium) mcolor(navy) msymbol(triangle)) ////
		(scatter p39 grupok2, sort msize(medsmall) mcolor(maroon) msymbol(circle)) ////
		(scatter p40 grupok3, sort msize(medium) mcolor(brown) msymbol(square)) ////
		(scatter p38 grupok4, sort msize(medsmall) mcolor(gray) msymbol(+)), ////
	ytitle("Mean and 95% CI") ////
	ylabel(0(10)50, grid glpattern(dash)) xlabel(0(1)4, labels labsize(small) ////
	valuelabel) graphregion(fcolor(white)) ////
	legend(order(5 "Self" 6 "Poor" ////
	7 "Middle" 8 "Rich") rows(2) region(c(none) margin(medsmall)))
graph export "$graph\FigA5_Exp_Taxes.emf", replace font("Times New Roman")


********************************************************************************
************************                                               Figure A6
********************************************************************************


use "$data\Desigualdad_temp.dta", clear
foreach var in p34a p34b p35a p35b p36a p36b p37 p38 p39 p40 {
replace `var'=`var'*10
}
pca yrs_educ seguro hh_index padres_ind padres_educbaja ageb_graproes [aw=weight]	//localidad//
predict wealth

foreach var in p33_gini p21_gini edad yrs_educ hh_index index_beliefspoverty index_colectivism wealth ////
	p34a p34b p35a p35b p36a p36b p37 p38 p39 p40 p19 p27a p27b p28a p28b p29a p29b {
quietly sum `var' [aw=weight]
quietly gen std_`var'=(`var'-r(mean))/r(sd)	
}
foreach var in p33_gini p34a p34b p36a p36b p35a p35b p37 {
quietly sum `var' [aw=weight]
di "`var'" "ALL " r(mean) "  "  r(sd)
quietly sum `var' [aw=weight] if T1==0 & T2==0
di "`var'" "CONTROLS " r(mean) "  "  r(sd)
quietly sum std_`var' [aw=weight]
di "STD`var'" "ALL " r(mean) "  "  r(sd)
quietly sum std_`var' [aw=weight] if T1==0 & T2==0
di "STD-`var'" "CONTROLS " r(mean) "  "  r(sd)
}

*****
**High Low Median
*****
quietly sum index_colectivism [aw=weight], det
gen median1=index_colectivism>r(p50)
quietly sum index_beliefspoverty [aw=weight], det
gen median2=index_beliefspoverty>r(p50)


*Higher than median: collectivism
quietly eststo A_p33_gini: reg std_p33_gini T1 T2 std_p21_gini female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median1==1, robust
**Q1-Q1
quietly eststo A_p34a: reg std_p34a T1 T2 std_p27a female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median1==1, robust
**Q1-Q5
quietly eststo A_p34b: reg std_p34b T1 T2 std_p27b female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median1==1, robust
**Q3-Q1
quietly eststo A_p36a: reg std_p36a T1 T2 std_p29a female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median1==1, robust
**Q3-Q5
quietly eststo A_p36b: reg std_p36b T1 T2 std_p29b female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median1==1, robust
**Q5-Q1
quietly eststo A_p35a: reg std_p35a T1 T2 std_p28a female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median1==1, robust
**Q5-Q5
quietly eststo A_p35b: reg std_p35b T1 T2 std_p28b female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median1==1, robust
**Taxes
quietly eststo A_p37: reg std_p37 T1 T2 std_p19 female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median1==1, robust

coefplot (A_p33_gini, keep(T1) \ A_p34a, keep(T1) \ A_p34b, keep(T1)  \ A_p36a, keep(T1) \ A_p36b, keep(T1) \ A_p35a, keep(T1) \ A_p35b, keep(T1) \ A_p37, keep(T1)) || ////
		(A_p33_gini, keep(T2) \ A_p34a, keep(T2) \ A_p34b, keep(T2)  \ A_p36a, keep(T2) \ A_p36b, keep(T2) \ A_p35a, keep(T2) \ A_p35b, keep(T2) \ A_p37, keep(T2)) || ////
		, xline(0) aseq swapnames ////
			coeflabels(A_p33_gini = "Gini" A_p34a = "Q1-Q1" A_p34b = "Q1-Q5" A_p36a = "Q3-Q1" A_p36b = "Q3-Q5" A_p35a = "Q5-Q1" A_p35b="Q5-Q5" A_p37="Taxes") ////
			graphregion(fcolor(white))  ci(95)  bylabels("Treatment 1: Inequality" "Treatment 2: Social Mobility")  ////
			xlabel(-0.30(0.10)0.30, grid glwidth(medthin) glpattern(dash)) ylabel(, nogrid)
graph export "$graph\A6A_App_Reg1_HighCollec.emf", replace font("Times New Roman")



***
*A6 B Lower than median collectivism
***
quietly eststo A_p33_gini: reg std_p33_gini T1 T2 std_p21_gini female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median1==0, robust
**Q1-Q1
quietly eststo A_p34a: reg std_p34a T1 T2 std_p27a female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median1==0, robust
**Q1-Q5
quietly eststo A_p34b: reg std_p34b T1 T2 std_p27b female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median1==0, robust
**Q3-Q1
quietly eststo A_p36a: reg std_p36a T1 T2 std_p29a female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median1==0, robust
**Q3-Q5
quietly eststo A_p36b: reg std_p36b T1 T2 std_p29b female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median1==0, robust
**Q5-Q1
quietly eststo A_p35a: reg std_p35a T1 T2 std_p28a female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median1==0, robust
**Q5-Q5
quietly eststo A_p35b: reg std_p35b T1 T2 std_p28b female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median1==0, robust
**Taxes
quietly eststo A_p37: reg std_p37 T1 T2 std_p19 female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median1==0, robust

coefplot (A_p33_gini, keep(T1) \ A_p34a, keep(T1) \ A_p34b, keep(T1)  \ A_p36a, keep(T1) \ A_p36b, keep(T1) \ A_p35a, keep(T1) \ A_p35b, keep(T1) \ A_p37, keep(T1)) || ////
		(A_p33_gini, keep(T2) \ A_p34a, keep(T2) \ A_p34b, keep(T2)  \ A_p36a, keep(T2) \ A_p36b, keep(T2) \ A_p35a, keep(T2) \ A_p35b, keep(T2) \ A_p37, keep(T2)) || ////
		, xline(0) aseq swapnames ////
			coeflabels(A_p33_gini = "Gini" A_p34a = "Q1-Q1" A_p34b = "Q1-Q5" A_p36a = "Q3-Q1" A_p36b = "Q3-Q5" A_p35a = "Q5-Q1" A_p35b="Q5-Q5" A_p37="Taxes") ////
			graphregion(fcolor(white))  ci(95)  bylabels("Treatment 1: Inequality" "Treatment 2: Social Mobility")  ////
			xlabel(-0.30(0.10)0.30, grid glwidth(medthin) glpattern(dash)) ylabel(, nogrid)
graph export "$graph\A6B_App_Reg1_LowCollec.emf", replace font("Times New Roman")


************
*A6 C: Index Poverty
**********
quietly eststo A_p33_gini: reg std_p33_gini T1 T2 std_p21_gini female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median2==1, robust
**Q1-Q1
quietly eststo A_p34a: reg std_p34a T1 T2 std_p27a female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median2==1, robust
**Q1-Q5
quietly eststo A_p34b: reg std_p34b T1 T2 std_p27b female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median2==1, robust
**Q3-Q1
quietly eststo A_p36a: reg std_p36a T1 T2 std_p29a female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median2==1, robust
**Q3-Q5
quietly eststo A_p36b: reg std_p36b T1 T2 std_p29b female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median2==1, robust
**Q5-Q1
quietly eststo A_p35a: reg std_p35a T1 T2 std_p28a female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median2==1, robust
**Q5-Q5
quietly eststo A_p35b: reg std_p35b T1 T2 std_p28b female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median2==1, robust
**Taxes
quietly eststo A_p37: reg std_p37 T1 T2 std_p19 female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median2==1, robust

coefplot (A_p33_gini, keep(T1) \ A_p34a, keep(T1) \ A_p34b, keep(T1)  \ A_p36a, keep(T1) \ A_p36b, keep(T1) \ A_p35a, keep(T1) \ A_p35b, keep(T1) \ A_p37, keep(T1)) || ////
		(A_p33_gini, keep(T2) \ A_p34a, keep(T2) \ A_p34b, keep(T2)  \ A_p36a, keep(T2) \ A_p36b, keep(T2) \ A_p35a, keep(T2) \ A_p35b, keep(T2) \ A_p37, keep(T2)) || ////
		, xline(0) aseq swapnames ////
			coeflabels(A_p33_gini = "Gini" A_p34a = "Q1-Q1" A_p34b = "Q1-Q5" A_p36a = "Q3-Q1" A_p36b = "Q3-Q5" A_p35a = "Q5-Q1" A_p35b="Q5-Q5" A_p37="Taxes") ////
			graphregion(fcolor(white))  ci(95)  bylabels("Treatment 1: Inequality" "Treatment 2: Social Mobility")  ////
			xlabel(-0.30(0.10)0.30, grid glwidth(medthin) glpattern(dash)) ylabel(, nogrid)
graph export "$graph\A6C_App_Reg1_HighIndPov.emf", replace font("Times New Roman")


***
*A6 D: Lower than median
***
quietly eststo A_p33_gini: reg std_p33_gini T1 T2 std_p21_gini female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median2==0, robust
**Q1-Q1
quietly eststo A_p34a: reg std_p34a T1 T2 std_p27a female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median2==0, robust
**Q1-Q5
quietly eststo A_p34b: reg std_p34b T1 T2 std_p27b female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median2==0, robust
**Q3-Q1
quietly eststo A_p36a: reg std_p36a T1 T2 std_p29a female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median2==0, robust
**Q3-Q5
quietly eststo A_p36b: reg std_p36b T1 T2 std_p29b female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median2==0, robust
**Q5-Q1
quietly eststo A_p35a: reg std_p35a T1 T2 std_p28a female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median2==0, robust
**Q5-Q5
quietly eststo A_p35b: reg std_p35b T1 T2 std_p28b female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median2==0, robust
**Taxes
quietly eststo A_p37: reg std_p37 T1 T2 std_p19 female std_edad cd_mxcity cd_norte cd_centro married work std_index_* std_wealth [aw=weight] if median2==0, robust

coefplot (A_p33_gini, keep(T1) \ A_p34a, keep(T1) \ A_p34b, keep(T1)  \ A_p36a, keep(T1) \ A_p36b, keep(T1) \ A_p35a, keep(T1) \ A_p35b, keep(T1) \ A_p37, keep(T1)) || ////
		(A_p33_gini, keep(T2) \ A_p34a, keep(T2) \ A_p34b, keep(T2)  \ A_p36a, keep(T2) \ A_p36b, keep(T2) \ A_p35a, keep(T2) \ A_p35b, keep(T2) \ A_p37, keep(T2)) || ////
		, xline(0) aseq swapnames ////
			coeflabels(A_p33_gini = "Gini" A_p34a = "Q1-Q1" A_p34b = "Q1-Q5" A_p36a = "Q3-Q1" A_p36b = "Q3-Q5" A_p35a = "Q5-Q1" A_p35b="Q5-Q5" A_p37="Taxes") ////
			graphregion(fcolor(white))  ci(95)  bylabels("Treatment 1: Inequality" "Treatment 2: Social Mobility")  ////
			xlabel(-0.30(0.10)0.30, grid glwidth(medthin) glpattern(dash)) ylabel(, nogrid)
graph export "$graph\A6D_App_Reg1_LowIndPov.emf", replace font("Times New Roman")

********************************************************************************
************************                                               Figure A7
********************************************************************************

use "$data\Desigualdad_temp.dta", clear
pca yrs_educ seguro hh_index padres_ind padres_educbaja ageb_graproes [aw=weight]	//localidad//
predict wealth
xtile pca_wealth=wealth [aw=weight], nq(20)
foreach var in p34a p34b p35a p35b p36a p36b p37 p38 p39 p40 {
replace `var'=`var'*10
}
collapse (mean) p21_gini p33_gini p34a p34b p35a p35b p36a p36b p27a p27b p28a p28b p29a p29b p19 p37 p38 p39 p40 wealth (count) nx=p21_gini (rawsum) weight [aw=weight], by(pca_wealth)
replace pca_wealth=(5*pca_wealth)-2.5

quietly reg p21_gini pca_wealth [aw=weight], rob
local coef : di %6.3f _b[pca_wealth]
local pval : di %6.3f  (2 * ttail(e(df_r), abs(_b[pca_wealth]/_se[pca_wealth])))
quietly reg p33_gini pca_wealth [aw=weight], rob
local coef2 : di %6.3f _b[pca_wealth]
local pval2 : di %6.3f  (2 * ttail(e(df_r), abs(_b[pca_wealth]/_se[pca_wealth])))


twoway (scatter p21_gini pca_wealth, sort msize(medsmall) msymbol(circle)) ////
	(scatter p33_gini pca_wealth, sort msize(medsmall) msymbol(square)) ////
	(lfit p21_gini pca_wealth [aw=weight], lwidth(medthick) lpattern(solid) lcolor(navy)) ////
	(lfit p33_gini pca_wealth [aw=weight], lwidth(medthick) lpattern(solid) lcolor(maroon)), ////
	ytitle("Gini coefficient") ////
	ytitle(, size(small)) ylabel(0(0.2)1, grid glwidth(medthin) glpattern(dash)) ////
	xtitle("SES rank") ////
	xtitle(, size(small)) xlabel(0(10)100, grid glwidth(medthin) glpattern(dash)) ////
	text(0.7 75 "Coefficient `coef' [`pval']", place(c) size(medium)) ////
	text(0.18 75 "Coefficient `coef2' [`pval2']", place(c) size(medium)) ////
	legend(order(1 "Perceived" 2 "Desired")) graphregion(fcolor(white))
graph export "$graph\FigA7_Wealth.emf", replace font("Times New Roman")


********************************************************************************
************************                                               Figure A8
********************************************************************************

quietly reg p27a pca_wealth [aw=weight], rob
local coef : di %6.3f _b[pca_wealth]
local pval : di %6.3f  (2 * ttail(e(df_r), abs(_b[pca_wealth]/_se[pca_wealth])))
quietly reg p34a pca_wealth [aw=weight], rob
local coef2 : di %6.3f _b[pca_wealth]
local pval2 : di %6.3f  (2 * ttail(e(df_r), abs(_b[pca_wealth]/_se[pca_wealth])))

**Figure A8a
twoway (scatter p27a pca_wealth, sort msize(medsmall) msymbol(circle)) ////
	(scatter p34a pca_wealth, sort msize(medsmall) msymbol(square)) ////
	(lfit p27a pca_wealth [aw=weight], lwidth(medthick) lpattern(solid) lcolor(navy)) ////
	(lfit p34a pca_wealth [aw=weight], lwidth(medthick) lpattern(solid) lcolor(maroon)), ////
	ytitle("Persistence Q1-Q1") ////
	ytitle(, size(small)) ylabel(0(20)100, grid glwidth(medthin) glpattern(dash)) ////
	xtitle("SES rank") ////
	xtitle(, size(small)) xlabel(0(10)100, grid glwidth(medthin) glpattern(dash)) ////
	text(70 75 "Coefficient `coef' [`pval']", place(c) size(medium)) ////
	text(12 75 "Coefficient `coef2' [`pval2']", place(c) size(medium)) ////
	legend(order(1 "Perceived" 2 "Desired")) graphregion(fcolor(white))
graph export "$graph\FigA8A_Q1Q1_Wealth.emf", replace font("Times New Roman")

**Figura A8b
quietly reg p28b pca_wealth [aw=weight], rob
local coef : di %6.3f _b[pca_wealth]
local pval : di %6.3f  (2 * ttail(e(df_r), abs(_b[pca_wealth]/_se[pca_wealth])))
quietly reg p35b pca_wealth [aw=weight], rob
local coef2 : di %6.3f _b[pca_wealth]
local pval2 : di %6.3f  (2 * ttail(e(df_r), abs(_b[pca_wealth]/_se[pca_wealth])))

twoway (scatter p28b pca_wealth, sort msize(medsmall) msymbol(circle)) ////
	(scatter p35b pca_wealth, sort msize(medsmall) msymbol(square)) ////
	(lfit p28b pca_wealth [aw=weight], lwidth(medthick) lpattern(solid) lcolor(navy)) ////
	(lfit p35b pca_wealth [aw=weight], lwidth(medthick) lpattern(solid) lcolor(maroon)), ////
	ytitle("Persistence Q5-Q5") ////
	ytitle(, size(small)) ylabel(0(20)100, grid glwidth(medthin) glpattern(dash)) ////
	xtitle("SES rank") ////
	xtitle(, size(small)) xlabel(0(10)100, grid glwidth(medthin) glpattern(dash)) ////
	text(30 75 "Coefficient `coef' [`pval']", place(c) size(medium)) ////
	text(90 75 "Coefficient `coef2' [`pval2']", place(c) size(medium)) ////
	legend(order(1 "Perceived" 2 "Desired")) graphregion(fcolor(white))
graph export "$graph\FigA8b_Q5Q5_Wealth.emf", replace font("Times New Roman")


**Figura A8c
quietly reg p27b pca_wealth [aw=weight], rob
local coef : di %6.3f _b[pca_wealth]
local pval : di %6.3f  (2 * ttail(e(df_r), abs(_b[pca_wealth]/_se[pca_wealth])))
quietly reg p34b pca_wealth [aw=weight], rob
local coef2 : di %6.3f _b[pca_wealth]
local pval2 : di %6.3f  (2 * ttail(e(df_r), abs(_b[pca_wealth]/_se[pca_wealth])))

twoway (scatter p27b pca_wealth, sort msize(medsmall) msymbol(circle)) ////
	(scatter p34b pca_wealth, sort msize(medsmall) msymbol(square)) ////
	(lfit p27b pca_wealth [aw=weight], lwidth(medthick) lpattern(solid) lcolor(navy)) ////
	(lfit p34b pca_wealth [aw=weight], lwidth(medthick) lpattern(solid) lcolor(maroon)), ////
	ytitle("Upward mobility Q1-Q5") ////
	ytitle(, size(small)) ylabel(0(20)100, grid glwidth(medthin) glpattern(dash)) ////
	xtitle("SES rank") ////
	xtitle(, size(small)) xlabel(0(10)100, grid glwidth(medthin) glpattern(dash)) ////
	text(30 75 "Coefficient `coef' [`pval']", place(c) size(medium)) ////
	text(90 75 "Coefficient `coef2' [`pval2']", place(c) size(medium)) ////
	legend(order(1 "Perceived" 2 "Desired")) graphregion(fcolor(white))
graph export "$graph\FigA8c_Q1Q5_Wealth.emf", replace font("Times New Roman")


**Figura A8d
quietly reg p28a pca_wealth [aw=weight], rob
local coef : di %6.3f _b[pca_wealth]
local pval : di %6.3f  (2 * ttail(e(df_r), abs(_b[pca_wealth]/_se[pca_wealth])))
quietly reg p35a pca_wealth [aw=weight], rob
local coef2 : di %6.3f _b[pca_wealth]
local pval2 : di %6.3f  (2 * ttail(e(df_r), abs(_b[pca_wealth]/_se[pca_wealth])))

twoway (scatter p28a pca_wealth, sort msize(medsmall) msymbol(circle)) ////
	(scatter p35a pca_wealth, sort msize(medsmall) msymbol(square)) ////
	(lfit p28a pca_wealth [aw=weight], lwidth(medthick) lpattern(solid) lcolor(navy)) ////
	(lfit p35a pca_wealth [aw=weight], lwidth(medthick) lpattern(solid) lcolor(maroon)), ////
	ytitle("Downward mobility Q5-Q1") ////
	ytitle(, size(small)) ylabel(0(20)100, grid glwidth(medthin) glpattern(dash)) ////
	xtitle("SES rank") ////
	xtitle(, size(small)) xlabel(0(10)100, grid glwidth(medthin) glpattern(dash)) ////
	text(40 75 "Coefficient `coef' [`pval']", place(c) size(medium)) ////
	text(10 75 "Coefficient `coef2' [`pval2']", place(c) size(medium)) ////
	legend(order(1 "Perceived" 2 "Desired")) graphregion(fcolor(white))
graph export "$graph\FigA8d_Q5Q1_Wealth.emf", replace font("Times New Roman")



**Figura A8e
quietly reg p29a pca_wealth [aw=weight], rob
local coef : di %6.3f _b[pca_wealth]
local pval : di %6.3f  (2 * ttail(e(df_r), abs(_b[pca_wealth]/_se[pca_wealth])))
quietly reg p36a pca_wealth [aw=weight], rob
local coef2 : di %6.3f _b[pca_wealth]
local pval2 : di %6.3f  (2 * ttail(e(df_r), abs(_b[pca_wealth]/_se[pca_wealth])))

twoway (scatter p29a pca_wealth, sort msize(medsmall) msymbol(circle)) ////
	(scatter p36a pca_wealth, sort msize(medsmall) msymbol(square)) ////
	(lfit p29a pca_wealth [aw=weight], lwidth(medthick) lpattern(solid) lcolor(navy)) ////
	(lfit p36a pca_wealth [aw=weight], lwidth(medthick) lpattern(solid) lcolor(maroon)), ////
	ytitle("Downward mobility Q3-Q1") ////
	ytitle(, size(small)) ylabel(0(20)100, grid glwidth(medthin) glpattern(dash)) ////
	xtitle("SES rank") ////
	xtitle(, size(small)) xlabel(0(10)100, grid glwidth(medthin) glpattern(dash)) ////
	text(50 75 "Coefficient `coef' [`pval']", place(c) size(medium)) ////
	text(10 75 "Coefficient `coef2' [`pval2']", place(c) size(medium)) ////
	legend(order(1 "Perceived" 2 "Desired")) graphregion(fcolor(white))
graph export "$graph\FigA8e_Q3Q1_Wealth.emf", replace font("Times New Roman")


**Figura A8f
quietly reg p29b pca_wealth [aw=weight], rob
local coef : di %6.3f _b[pca_wealth]
local pval : di %6.3f  (2 * ttail(e(df_r), abs(_b[pca_wealth]/_se[pca_wealth])))
quietly reg p36b pca_wealth [aw=weight], rob
local coef2 : di %6.3f _b[pca_wealth]
local pval2 : di %6.3f  (2 * ttail(e(df_r), abs(_b[pca_wealth]/_se[pca_wealth])))

twoway (scatter p29b pca_wealth, sort msize(medsmall) msymbol(circle)) ////
	(scatter p36b pca_wealth, sort msize(medsmall) msymbol(square)) ////
	(lfit p29b pca_wealth [aw=weight], lwidth(medthick) lpattern(solid) lcolor(navy)) ////
	(lfit p36b pca_wealth [aw=weight], lwidth(medthick) lpattern(solid) lcolor(maroon)), ////
	ytitle("Upward mobility Q3-Q5") ////
	ytitle(, size(small)) ylabel(0(20)100, grid glwidth(medthin) glpattern(dash)) ////
	xtitle("SES rank") ////
	xtitle(, size(small)) xlabel(0(10)100, grid glwidth(medthin) glpattern(dash)) ////
	text(30 75 "Coefficient `coef' [`pval']", place(c) size(medium)) ////
	text(90 75 "Coefficient `coef2' [`pval2']", place(c) size(medium)) ////
	legend(order(1 "Perceived" 2 "Desired")) graphregion(fcolor(white))
graph export "$graph\FigA8f_Q3Q5_Wealth.emf", replace font("Times New Roman")



********************************************************************************
************************                                                Figure A9
********************************************************************************


use "$data\Desigualdad_temp.dta", clear
pca yrs_educ seguro hh_index padres_ind padres_educbaja ageb_graproes [aw=weight]	//localidad//
predict wealth
xtile pca_wealth=wealth [aw=weight], nq(20)
foreach var in p34a p34b p35a p35b p36a p36b p37 p38 p39 p40 {
replace `var'=`var'*10
}

 
gen diff_gini=p21_gini-p33_gini
gen diff_q1q1=p27a-p34a
gen diff_q1q5=p27b-p34b
gen diff_q3q1=p29a-p36a
gen diff_q3q5=p29b-p36b
gen diff_q5q1=p28a-p35a
gen diff_q5q5=p28b-p35b
gen diff_tax=p19-p37

foreach var in edad index_beliefspoverty index_colectivism wealth ////
		diff_gini diff_q1q1 diff_q1q5 diff_q3q1 diff_q3q5 diff_q5q1 diff_q5q5 diff_tax {
quietly sum `var' [aw=weight]
quietly gen std_`var'=(`var'-r(mean))/r(sd)	
}


quietly eststo A_p33_gini: reg std_diff_gini T1 T2 std_wealth female std_edad cd_mxcity cd_norte cd_centro married work std_index_* [aw=weight], robust
**Q1-Q1
quietly eststo A_p34a: reg std_diff_q1q1 T1 T2 std_wealth female std_edad cd_mxcity cd_norte cd_centro married work std_index_*  [aw=weight], robust
**Q1-Q5
quietly eststo A_p34b: reg std_diff_q1q5 T1 T2 std_wealth female std_edad cd_mxcity cd_norte cd_centro married work std_index_*  [aw=weight], robust
**Q3-Q1
quietly eststo A_p36a: reg std_diff_q3q1 T1 T2 std_wealth female std_edad cd_mxcity cd_norte cd_centro married work std_index_*  [aw=weight], robust
**Q3-Q5
quietly eststo A_p36b: reg std_diff_q3q5 T1 T2 std_wealth female std_edad cd_mxcity cd_norte cd_centro married work std_index_*  [aw=weight], robust
**Q5-Q1
quietly eststo A_p35a: reg std_diff_q5q1 T1 T2 std_wealth female std_edad cd_mxcity cd_norte cd_centro married work std_index_*  [aw=weight], robust
**Q5-Q5
quietly eststo A_p35b: reg std_diff_q5q5 T1 T2 std_wealth female std_edad cd_mxcity cd_norte cd_centro married work std_index_*  [aw=weight], robust
**Taxes
quietly eststo A_p37: reg std_diff_tax T1 T2 std_wealth female std_edad cd_mxcity cd_norte cd_centro married work std_index_*  [aw=weight], robust


coefplot (A_p33_gini, keep(T1) \ A_p34a, keep(T1) \ A_p35b, keep(T1)  \ A_p34b, keep(T1) \ A_p36b, keep(T1) \ A_p35a, keep(T1) \ A_p36a, keep(T1) \ A_p37, keep(T1)) || ////
		(A_p33_gini, keep(T2) \ A_p34a, keep(T2) \ A_p35b, keep(T2)  \ A_p34b, keep(T2) \ A_p36b, keep(T2) \ A_p35a, keep(T2) \ A_p36a, keep(T2) \ A_p37, keep(T2)) || ////
		(A_p33_gini, keep(std_wealth) \ A_p34a, keep(std_wealth) \ A_p35b, keep(std_wealth)  \ A_p34b, keep(std_wealth) \ A_p36b, keep(std_wealth) \ A_p35a, keep(std_wealth) \ A_p36a, keep(std_wealth) \ A_p37, keep(std_wealth)) || ////
		, xline(0) aseq swapnames ////
			coeflabels(A_p33_gini = "Gini" A_p34a = "Q1-Q1" A_p34b = "Q1-Q5" A_p36a = "Q3-Q1" A_p36b = "Q3-Q5" A_p35a = "Q5-Q1" A_p35b="Q5-Q5" A_p37="Taxes") ////
			graphregion(fcolor(white))  ci(95)  bylabels("Treatment 1: Inequality" "Treatment 2: Social Mobility" "Standardized Wealth")  ////
			xlabel(-0.30(0.10)0.30, grid glwidth(medthin) glpattern(dash)) ylabel(, nogrid)
graph export "$graph\FigA9_Reg3_Difference.emf", replace font("Times New Roman")




********************************************************************************
************************                                                Figure A10
********************************************************************************

*Despues del anterior solo sigue esto

sum diff_gini [aw=weight]
*gen std_diff_gini=(diff_gini-r(mean))/r(sd)
twoway (qfit std_diff_gini p21_gini  if T1==1 [aw=weight], lwidth(thick) lpattern(solid)) ////
       (qfit  std_diff_gini p21_gini  if T1==0 & T2==0 [aw=weight], lwidth(thick) lpattern(dash)), ////
graphregion(fcolor(white)) ytitle("Difference") xtitle("Perceived") ////
legend(order(1 "Treatment 1" 2 "Control")) xlabel(, grid glwidth(medthin) glpattern(dash)) ////
ylabel(, grid glwidth(medthin) glpattern(dash))
graph export "$graph\FigA10.emf", replace font("Times New Roman")


********************************************************************************
************************                                                Figure A14
********************************************************************************

use "$data\Desigualdad_temp.dta", clear
pca yrs_educ seguro hh_index padres_ind padres_educbaja ageb_graproes [aw=weight]	//localidad//
predict wealth
xtile pca_wealth=wealth [aw=weight], nq(20)
foreach var in p34a p34b p35a p35b p36a p36b p37 p38 p39 p40 {
replace `var'=`var'*10
}
collapse (mean) p20 p14 p15 p16 p17 p21_gini p33_gini p34a p35b p27a p28b p19 p37 p38 p39 p40 wealth (count) nx=p21_gini (rawsum) weight [aw=weight], by(pca_wealth)
replace pca_wealth=(5*pca_wealth)-2.5


replace p20=100*p20/10000
sum p20 [aw=weight]
quietly reg p20 pca_wealth [aw=weight], rob
local coef : di %6.3f _b[pca_wealth]
local pval : di %6.3f  (2 * ttail(e(df_r), abs(_b[pca_wealth]/_se[pca_wealth])))

twoway (scatter p20 pca_wealth, sort msize(medsmall) msymbol(circle)) ////
	(lfit p20 pca_wealth [aw=weight], lwidth(medthick) lpattern(solid) lcolor(navy)) ////
	, ////
	ytitle("% from own $ to eliminate poverty/inequality") ////
	ytitle(, size(medium)) ylabel(0(5)25, grid glwidth(medthin) glpattern(dash)) ////
	xtitle("SES rank") ////
	xtitle(, size(medium)) xlabel(0(10)100, grid glwidth(medthin) glpattern(dash)) ////
	text(5 80 "Coefficient `coef' [`pval']", place(c) size(medium)) ////
	legend(off) graphregion(fcolor(white))
graph export "$graph\FigA14_Willingtogiveup.emf", replace font("Times New Roman")



******************************
****Supplementary Tables
******************************

********************************************************************************
************************ Table B.1. 
********************************************************************************
use "$data\Desigualdad_temp.dta", clear
pca yrs_educ seguro hh_index padres_ind padres_educbaja ageb_graproes [aw=weight]	//localidad//
predict wealth
xtile pca_wealth=wealth [aw=weight], nq(20)
replace pca_wealth=(5*pca_wealth)-2.5

putexcel set "$tablas\Tables.xls", sheet("tableA1", replace) modify open
putexcel B2= "Variable"
putexcel C2= "N"
putexcel D2= "Mean"
putexcel E2= "Std. Dev."
putexcel F2= "Min"
putexcel G2= "Max"
putexcel B3="A. Key control variables"
local i=4
foreach var in T1 T2 female edad yrs_educ educ_universidad married work seguro wealth pca_wealth hh_index index_beliefspoverty index_colectivism padres_ind padres_educbaja ////
 cd_mxcity cd_norte cd_sur cd_centro {
putexcel B`i'="`var'"
quietly sum `var' [aw=weight], det
matrix A=[r(N), r(mean), r(sd), r(min), r(max)]
putexcel C`i'= matrix(A), nformat(number_d2)
local i=`i'+1
}
display " `i' "
putexcel B`i'="B. Other variables"
local i=`i'+1
foreach var in p12 p13 p14 p15 p16 p17 p18a p18b p18c p18d p18e p18f p19 p20 ////
p21 p21_gini p22a p22b p22c p22d p22e p22f p23a p23b p23c p23d p24a p24b ////
p25a p25b p26a p26b p27a p27b p28a p28b p29a p29b {
putexcel B`i'="`var'"
quietly sum `var' [aw=weight], det
matrix A=[r(N), r(mean), r(sd), r(min), r(max)]
putexcel C`i'= matrix(A), nformat(number_d2)
local i=`i'+1
}

putexcel close

 

********************************************************************************
************************ Table B.2. 
********************************************************************************
use "$data\Desigualdad_temp.dta", clear
pca yrs_educ seguro hh_index padres_ind padres_educbaja ageb_graproes [aw=weight]	//localidad//
predict wealth
xtile pca_wealth=wealth [aw=weight], nq(20)
foreach var in p34a p34b p35a p35b p36a p36b p37 p38 p39 p40 {
replace `var'=`var'*10
}

gen diff_gini=p21_gini-p33_gini
gen diff_q1q1=p27a-p34a
gen diff_q1q5=p27b-p34b
gen diff_q3q1=p29a-p36a
gen diff_q3q5=p29b-p36b
gen diff_q5q1=p28a-p35a
gen diff_q5q5=p28b-p35b
gen diff_tax=p19-p37

label var diff_gini "Gini"
keep diff_gini diff_q1q1 diff_q5q5 diff_q1q5 diff_q3q5 diff_q5q1 diff_q3q1 diff_tax weight 

outreg2  using "$tablas\TableB.2.xls" [aw=weight], replace sum (log) keep (diff_gini diff_q1q1 diff_q5q5 diff_q1q5 diff_q3q5 diff_q5q1 diff_q3q1 diff_tax)   eqkeep(mean sd) 



 

 
