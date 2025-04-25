 
***Protensia Hadunka****
**CHARCOAL AND FAW***
use "C:\Users\hadunka2\Box\HICPS Cleaning 07_12_19\HICPS_unbalanced",clear
clear matrix
clear mata
*set maxvar 10000
replace Armyworms = 0 if Armyworms == . & year == 2018
replace army = Armyworms if year == 2018
replace army = 0 if year == 2016
recode army_aff 4=3 if year==2018
recode army_aff 5=3 if year==2018
replace army_aff=0 if army_aff==.
recode qleft .=0

*generating 2019 FAW 
replace army=1 if year==2019 & army_aff>=0
replace army=0 if year==2019 & army_aff==0

*Genearting the intensitensity for 2019
*replace army_aff=2 if army_aff==2|army_aff==3
*replace army_aff=3 if army_aff==4|army_aff==5

*Merging the district variable
merge m:1 HHID using "C:\Users\hadunka2\Box\HICPS Cleaning 07_12_19\2016 HICPS\2016 HICPS G.dta", keepusing( hh_head_sex hh_head_age hh_head_edu district camp province firewood_av10 ) nogen update
*merge m:1 HHID using "C:\Users\hadunka2\Box\Merging HICPS\2016 HICPS G.dta", keepusing( hh_head_sex hh_head_age hh_head_edu district camp) nogen update
*merge m:1 HHID using "C:\Users\hadunka2\Box\HICPS Cleaning 07_12_19 (Protensia Hadunka)\2016 HICPS\2016 HICPS G.dta", keepusing(province) nogen update
*merge m:1 HHID using "C:\Users\hadunka2\Box\Merging HICPS\2016 HICPS G.dta", keepusing(province) nogen update
joinby HHID year using "C:\Users\hadunka2\Box\Raw rainfall\Deforestation\Deforestation.dta"

joinby HHID year using "C:\Users\hadunka2\Box\Raw rainfall\Deforestation_buffer\Deforestation2.dta"
sort year HHID

*joinby HHID year using "C:\Users\hadunka2\Box\HICPS appending\Rainfall.dta", _merge(merge_rain) unmatched(master) update
joinby HHID year using "C:\Users\hadunka2\Box\Raw rainfall\Total_Rainfall.dta", _merge(merge_rain) unmatched(master) update
sort year HHID
order year HHID totalrain

joinby HHID year using "C:\Users\hadunka2\Box\Temperature data\clean_temp.dta", _merge(merge_temp) unmatched(master) update
sort year HHID
order year HHID temperature

save "C:\Users\hadunka2\Box\HICPS Cleaning 07_12_19\Rain.dta",replace


use "C:\Users\hadunka2\Box\Raw rainfall\Total_Rainfall.dta"
sort year HHID
order year HHID totalrain
keep if year==2015

*use "C:\Users\hadunka2\Box\Raw rainfall\Deforestation_buffer\Deforestation2.dta"
*sort year HHID
*keep if year==2015

*use "C:\Users\hadunka2\Box\Raw rainfall\Deforestation_buffer\Deforestation2.dta"
*sort year HHID
*keep if year==2015


use "C:\Users\hadunka2\Box\Temperature data\clean_temp.dta",clear

sort year HHID
order year HHID temperature
keep if year==2015

append using "C:\Users\hadunka2\Box\HICPS Cleaning 07_12_19\Rain.dta"

*joinby HHID year using "C:\Users\hadunka2\Box\Pollution clean\pollution.dta"

*joinby HHID year using "C:\Users\hadunka2\Box\Raw rainfall\Deforestation_buffer\Deforestation2.dta"

***Occupation***
*drop if mig1_occ>9
*hist mig1_occ, percent barw(.5) color(eltblue)  xlabel(1 "On this or another small farm" 2 "On a commercial farm" 3 "Other industrial work" 4 "Teacher" 5 "Civil Servant" 6 "Clerk" 7 "Shop attendant" 8 "Non-agricultural piecework" 9 "Other", angle(vertical))

*hist solar, percent barw(.5) color(eltblue)  xlabel(, angle(horizontal))

**Source of income***
*gen source_income = 1 if income_smallbusiness > 0
*replace source_income = 2 if income_charcoal> 0
*replace source_income = 3 if income_gardening> 0
*replace source_income = 4 if income_forestproduct> 0
*replace source_income = 5 if income_livestock> 0
*replace source_income = 6 if income_remittance> 0
*replace source_income = 7 if income_other> 0

*hist source_income, percent barw(.5) color(eltblue)  xlabel(1 "smallbusiness" 2 "charcoal" 3 "gardening" 4 "forestproduct" 5 "livestock"  , angle(vertical))
 
*drop if obstacle_coll >7
*drop if obstacle_coll==0
*hist obstacle_coll, percent barw(.6) color(eltblue)  xlabel(1 "Distance to collection area" 2 "Density of available firewood" 3 "Someone with time to do the collection" 4 "Poor quality of available firewood" 5 "Limitations of access/protected area" 6 "Other" 7 "No significant restrictions to finding firewood" , angle(vertical))


/***Perception of the forest***
hist firewood_av10, percent barw(.4) color(eltblue)  xlabel(1 "Increased" 2 "Stayed the same" 3 "Decreased"  , angle(vertical))


hist Charc_business, percent barw(.4) color(eltblue) by(year, col(4)) xlabel(0 "No" 1 "Yes" , angle(vertical))*/

rename totalrain rainfall
gen sqrainfall=rainfall^2

**Log of rainfall
gen log_rain=log(rainfall)
gen sq_rainfall=log_rain^2
*yield variable
*Adding the estimated maize that remained in the field with that 
*Total maize production 
replace qharvested=0 if qharvested==.
replace qleft=0 if qleft==.
gen Total= qharvested+ qleft

*Access to credit
gen Accs_credit=0
replace Accs_credit=1 if borrow500==1| borrow2500==1|borrow10000==1
gen accscredit=log(borrow500+sqrt(borrow500^2+1)) 

// farm size
* I add farm size up across all 5 plantings
egen farmsize_W01 = rowtotal(plot_1 plot_2 plot_3 plot_4 plot_5), miss
label var farmsize "Maize farm size: ha"

replace farmsize_W01=. if farmsize_W01>8 // dropping 4 farms that are too big


gen lnfarmsize = log(farmsize)
label var lnfarmsize "Log of maize farm size"
hist lnfarmsize, norm

gen lnfarmsize_W01 = log(farmsize_W01)
label var lnfarmsize_W01 "Log of maize farm size (winsorized at 1%)"
hist lnfarmsize_W01, norm

*bys village: egen med1_farmland = median(farmland)  // no missings
bys camp: egen med2_farmland = median(farmland)  // no missings

gen farmland_clean = farmland
replace farmland_clean = med2_farmland if farmland > 13

bys year: sum farmland_clean farmsize_W01
bys year: tab farmland_clean, m
bys year: tab farmland_clean

count if farmland_clean < farmsize_W01 // 275 cases
list farmland_clean farmsize_W01 if farmland_clean < farmsize_W01 // 275 cases
bys year: count if farmland_clean < farmsize_W01 & farmsize_W01 != . // cases are in all 4 years

replace farmland_clean = farmsize_W01 if farmland_clean < farmsize_W01 & farmsize_W01 != .

*tw (kdensity farmland_W01)(kdensity farmland_clean)

label var farmland_clean "Total area of farmland: Ha"



*winsor2 yield, cuts (1 99) by(year)

gen subsidy=0
replace subsidy=1 if fisp==1|fisp==2|fisp==3

recode qseed_1 .=0
recode qseed_2 .=0
recode qseed_3 .=0
recode qseed_4 .=0
recode qseed_5 .=0
gen quaseed= qseed_1+ qseed_2+ qseed_3+ qseed_4+ qseed_5
rename quaseed qseed

*Generating the fertilizer variable
recode qbasal_1 .=0
recode qbasal_2 .=0
recode qbasal_3 .=0
recode qbasal_4 .=0
recode qbasal_5 .=0
gen quafert= qbasal_1+ qbasal_2+ qbasal_3+ qbasal_4+ qbasal_5
rename quafert qbasal

recode qtop_1 .=0
recode qtop_2 .=0
recode qtop_3 .=0
recode qtop_4 .=0
recode qtop_5 .=0
gen qfer= qtop_1+ qtop_2+ qtop_3+ qtop_4+ qtop_5
rename qfer qtop

gen fert= qtop + qbasal

encode camp, gen(camp1)
gen camp2 = camp1

winsor2 Total, cuts (1 99) by (year)
winsor2 cultivown_land, cuts (1 99) by (year) 
replace hh_num=1 if hh_num==0|hh_num==.
*gen sq_rain=rainfall^2
ihstrans hh_num 
ihstrans farmland
pca tv radio motorcycle water_pump plough sprayers ox_carts vehicle iron_sheets solar
predict capital
winsor2 capital,cuts (1 99) by (year)
ihstrans capital_w
rename ihs_capital_w hpscapital
rename ihs_farmland hpsfarmland
ihstrans cultivown_land_w qseed fert 
rename ihs_hh_num hpsHHsize
rename ihs_cultivown_land_w totlandng
rename ihs_qseed hpsquaseed
rename ihs_fert hpsfert
recode coop 2=1
*rename ihs_rainfall rainfall2
sort year 
by year: tab army
*gen lyield= ln( yield)
*IV regarding the intensity responses at camp level
gen low=1 if army_aff==1
recode low .=0 
gen med=1 if army_aff==2
recode med .=0 
gen seve=1 if army_aff==3
recode seve .=0 

egen iv_camp = sum(low), by(camp year)
egen iv_camp2 = sum(med), by(camp year)
egen iv_camp3 = sum(seve), by(camp year)

gen hh_iv_low = iv_camp - low
gen hh_iv_med= iv_camp2 - med
gen hh_iv_seve= iv_camp3 - seve

egen num_in_camp1 = sum(low), by (camp year)
egen num_in_camp2 = sum(med), by (camp year)
egen num_in_camp3 = sum(seve), by (camp year)

gen camp_1 = num_in_camp1 - 1
gen camp_2 = num_in_camp2 - 1
gen camp_3 = num_in_camp3 - 1
egen number_in_camp = count(HHID), by(camp year)

gen num_1=(number_in_camp-1)
gen camp_IV1 =  camp_1/(number_in_camp-1)
gen camp_IV2 =  camp_2/(number_in_camp-1)
gen camp_IV3 =  camp_3/(number_in_camp-1)

*Corrected IVs
gen IV1 =(sum(low)-low)/(number_in_camp-1)
gen IV2 =(sum(med)-med)/(number_in_camp-1)
gen IV3 =(sum(seve)-seve)/(number_in_camp-1)

*FAW binary
egen num_in_camp4 = sum(army), by (camp year)
gen camp_4 = num_in_camp4 - 1
gen camp_IV4 =  camp_4/(number_in_camp-1)
gen IV4 =(sum(army)-army)/(number_in_camp-1)

*egen in_camp = mean(army_aff), by(camp year)

egen sum_camp = sum(army_aff), by(camp year)
egen sum_camp2 = sum(army), by(camp year)
gen hh_minus_camp_mean = sum_camp - army_aff
gen hh_minus_camp_mean2 = sum_camp2 - army

ihstrans hh_minus_camp_mean
rename ihs_hh_minus_camp_mean army_diff

*egen number_in_camp = count(HHID), by(camp Times)
*egen number_in_camp = count(army_aff), by(camp year)
egen number_in_camp2 = count(army), by(camp year)
gen camp_one = number_in_camp - 1
gen camp_one2 = number_in_camp2 - 1

gen camIV = hh_minus_camp_mean /camp_one
gen camIV2 = hh_minus_camp_mean2 /camp_one2

gen cam3 =  hh_minus_camp_mean2/(number_in_camp-1)

recode plot_1 .=0
recode plot_2 .=0
recode plot_3 .=0
recode plot_4 .=0
recode plot_5 .=0
gen maize_area= plot_1+ plot_2+ plot_3+ plot_4+ plot_5

sort HHID year
xtset HHID year
 
*gen myield=Total/maize_area
*gen lyield= ln(myield)

*gen fmaize=Total/farmland
*gen fmaize=Total/cultivown_land_w
*gen fmaize=Total/hpsfarmland
gen fmaize=Total_w/totlandng


*winsor2 yield, cuts (1 99) by(year)
*winsor2 fmaize, cuts (1 99) by(year)
gen lyield=ln(fmaize)
gen y2018=1 if year==2018
gen y2017=1 if year==2017
gen y2019=1 if year==2019
recode y2017 y2018 y2019 (.=0)

ihstrans rainfall
ihstrans sqrainfall
ihstrans farmland hpsfarmland
rename ihs_rainfall lrainfall
rename ihs_sqrainfall lsqrainfall
gen Total_income= income_piecework+ income_salary+ income_smallbusiness+ income_charcoal+ income_gardening+ income_forestproduct+ income_livestock+ income_remittance+ income_other
gen non_aginc= income_remittance+ income_other+ income_salary+ income_piecework
gen log_inc=log(Total_income)
gen sq_inc=log_inc^2
*gen non_aginc=Total_income-non_aginc


*sum hh_head_age hh_head_sex hh_head_edu Charc totlandng cultivown_land fmaize Total_income rainfall dist_locat_ct_trees Accs_credit army if year == 2016

gen ag_inc=income_gardening + income_forestproduct + income_livestock
**district by year FE
gen dist_yr=district*year


*Pesticide 
/**Lagged FAW 
egen in_camp = mean(army_aff), by(camp year)
gen FAW_lag=(army_aff-in_camp)
*drop if FAW_lag==0
gen l_FAW= ln(FAW_lag)
drop if l_FAW==0*/

gen l_FAW = L.army_aff
replace l_FAW = 0 if year==2016

*gen l_FAW2=L.army
*replace l_FAW2 = 0 if year==2016

gen l_camIV=L.camIV
gen l_rainfall=L.rainfall
gen s_lrainfall=l_rainfall^2
*Log fertilizer
gen l_fert=log(fert)
gen sq_fert=l_fert^2
gen sq_income= Total_income^2
gen l_inc=L.Total_income
gen l_sqinc=l_inc^2


****Then anyalyze the effects of faw on chrcoal prices***
*gen charc_price = asinh(price_charc)
*reghdfe charc_price l_FAW  , absorb ( i.year i.district) cl(camp1)


*Lagged landholding
gen lag_land=L.farmland

*generating the binary variable
gen spray_binr=1 if spray==1
replace spray_binr=0 if spray==.
*Spray instrument
egen sum_spray = sum(spray), by(camp year)
egen sum_spray2 = sum(spray_binr), by(camp year)
gen hh_minus_spray_mean = sum_spray - spray
gen hh_minus_spray_mean2 = sum_spray2 - spray_binr

gen camIVsp = hh_minus_spray_mean /camp_one
gen camIVsp2 = hh_minus_spray_mean2 /camp_one2

gen l_spray=L.spray
*recode l_spray .=0 
gen l_camIVsp=L.camIVsp

rename totlandng cultivatedland
      

gen spray_FAW=spray*l_FAW

reghdfe Charc l_FAW l_spray spray_FAW farmland   , absorb ( i.district) 



replace Charc=0 if Charc==.
**Camp fixed effects
reghdfe Charc l_FAW hh_head_edu hhsize hh_head_sex Total_income farmland l_rainfall fert , absorb ( i.camp2) cl(camp1)
outreg2 using results, excel replace
reghdfe Charc camIV hh_head_edu hhsize hh_head_sex Total_income farmland l_rainfall fert , absorb ( i.camp2) cl(camp1)
outreg2 using results, excel replace
ivreghdfe Charc hh_head_edu hhsize hh_head_sex Total_income farmland l_rainfall fert (l_FAW=l_camIV) , absorb (i.camp2) first cl(camp1)
outreg2 using results, excel append

***District fixed effects
reghdfe Charc l_FAW hh_head_edu hhsize hh_head_sex Total_income farmland s_lrainfall FISP fert ,absorb (i.district) cl(camp1)
outreg2 using results, excel replace
ivreghdfe Charc hh_head_edu hhsize hh_head_sex Total_income farmland s_lrainfall FISP fert (l_FAW=l_camIV) , absorb (i.district) first cl(camp1)
outreg2 using results, excel append
ivreghdfe Charc l_camIV hh_head_edu hhsize hh_head_sex Total_income farmland s_lrainfall FISP fert , absorb (i.district) 
outreg2 using results, excel append


*****Lets look at the leads test****
reghdfe Charc l_FAW army_aff  hhsize  Total_income farmland  fert l_rainfall s_lrainfall, absorb (i.HHID )
estimates store charc1

coefplot (charc1, asequation(l_FAW) \) ///
, drop(_cons) keep(l_FAW  army_aff) ///
mlabel(cond(@pval<.01, "***", cond(@pval<.05, "**", cond(@pval<.1, "*", "")))) mlabc(red) mlabsize(medium) mlabgap(6pt) mlabp(9) /// 
eqlabel(, labsize(small)) msymbol(circle) mcolor(ebblue) ciopts(recast(rcap) lcolor(ebblue)) ///
 xline(0,lcolor(red)) ylabel(, labsize(vsmall)) omitted baselevels ///  
bylabels( "Food Group Diversification Index (FGDI)" "Household Dietary Diversity (HDDS)") ///
byopts(compact rows(1) note("p-values shown alongside markers" "*** p<.01, ** p<.05, * p<.1" ))



***Let's try the bacon decomposition model****




quietly regress Charc l_FAW army_aff  l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize hh_head_sex
keep if e(sample)
// get means
foreach v in l_FAW army_aff l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize  hh_head_sex {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx 
estimates store charc

coefplot (charc, asequation(l_FAW) \) ///
, drop(_cons) keep(l_FAW  army_aff) ///
mlabel(cond(@pval<.01, "***", cond(@pval<.05, "**", cond(@pval<.1, "*", "")))) mlabc(red) mlabsize(medium) mlabgap(6pt) mlabp(9) /// 
eqlabel(, labsize(small)) msymbol(circle) mcolor(ebblue) ciopts(recast(rcap) lcolor(ebblue)) /// 
 xline(0,lcolor(red)) ylabel(, labsize(vsmall)) omitted baselevels /// 
byopts(compact rows(1) note("p-values shown alongside markers" "*** p<.01, ** p<.05, * p<.1" ))

*Household fixed effec
*drop if year==2015

reghdfe Charc l_FAW l_rainfall s_lrainfall, absorb (i.HHID) 
reghdfe Charc i.l_FAW l_rainfall s_lrainfall i.year dist_yr, absorb (i.HHID) 
outreg2 using results, excel replace
ivreghdfe Charc l_camIV l_rainfall s_lrainfall , absorb (i.HHID) 
outreg2 using results, excel append

reghdfe Charc l_FAW l_rainfall temperature i.year, absorb (i.HHID)

*ihstrans temperature
*rename ihs_temperature hpstemperature

***Weather and FAW
ihstrans temperature 
rename ihs_temperature ltemperature
gen sqtemp=temperature^2
 
ihstrans sqtemp 
rename ihs_sqtemp sqtemperature
gen sq_temp=temperature^2


***FAW and cultivated area***
reghdfe cultivatedland l_FAW
reghdfe cultivatedland l_FAW,absorb(i.year i.HHID)

reghdfe hpsfarmland l_FAW l_rainfall s_lrainfall  ,absorb(i.year i.district) 

reghdfe hpsfarmland camIV l_rainfall s_lrainfall,absorb(i.year i.district)

ivreghdfe hpsfarmland     (l_FAW = camIV) l_rainfall l_rainfall ,absorb(i.year i.district)


***FAW and charcoal prices****
*ivreghdfe charc_price (camIV=l_FAW) l_rainfall s_lrainfall temperature squa_temp cam_year i.year , absorb (i.district) first


*save "C:\Users\hadunka2\Box\01. Zambia Deforestation\Data_Vinni",replace


***Converting the walking distance from minutes to kilometers***
gen dist_forest = time_coll/12

gen army17 = army_aff

gen f_FAW = F.army_aff
gen f_army = F.army

gen FAW_distfore = dist_forest * f_FAW

***obstables to collection***
gen dist_obs = (obstacle_coll == 1)

gen faw_dist = dist_obs*f_FAW


****Area responsible***


hist area_responsible, percent barw(.35) color(eltblue) xlabel(1 "National Government" 2 "Local Government" 3 "Traditional Authority" 4 "Private Individual" 5 "Private Individual" 6 "Myself", angle(vertical))

***Would they behave differently if they owned the land***

gen self_interest = 0 if area_responsible ==4|area_responsible ==6 & area_responsible !=.
replace self_interest = 1 if area_responsible ==1|area_responsible ==2|area_responsible ==3|area_responsible ==5


gen tradition = 1 if area_responsible ==3 & area_responsible !=.
replace tradition = 0 if area_responsible ==1|area_responsible ==2|area_responsible ==4|area_responsible ==5|area_responsible ==6

gen self_FAW  =  f_army*self_interest

 gen self_tradition  =  f_army*tradition


reghdfe Charc  self_interest  cultivown_land hh_head_edu ban_firewood_areas

reghdfe Charc f_army self_interest self_FAW cultivown_land hh_head_edu ban_firewood_areas,absorb( i.district)

reghdfe Charc f_army self_interest self_FAW ,absorb(i.year i.district)

reghdfe Charc f_army self_interest self_FAW cultivown_land hh_head_edu ban_firewood_areas ,absorb(i.year i.district)
outreg2 using results.tex, replace

reghdfe Charc f_army tradition self_tradition cultivown_land hh_head_edu ban_firewood_areas ,absorb( i.district)
outreg2 using results.tex, replace*/


***Distance to trees and charcoal production***
reghdfe deforestation_area_km_squared  dist_forest  cultivown_land hh_head_edu ban_firewood_areas

reghdfe deforestation_area_km_squared  dist_forest FAW_distfore cultivown_land hh_head_edu ban_firewood_areas

reghdfe Charc  dist_obs  cultivown_land hh_head_edu ban_firewood_areas

gen logforest = log(deforestation_area_km_squared)

reghdfe logforest f_army self_interest self_FAW cultivown_land hh_head_edu ban_firewood_areas,absorb( i.district)
outreg2 using results.tex, replace

reghdfe logforest  dist_forest FAW_distfore cultivown_land hh_head_edu ban_firewood_areas,absorb( i.camp2)
outreg2 using results.tex, replace

reghdfe Charc  dist_forest FAW_distfore cultivown_land hh_head_edu ban_firewood_areas,absorb( i.district)
outreg2 using results.tex, append



reghdfe deforestation_area_km_squared  dist_obs  cultivown_land hh_head_edu ban_firewood_areas,absorb( i.district)


reghdfe logforest  dist_obs faw_dist cultivown_land hh_head_edu ban_firewood_areas


reghdfe Charc  dist_forest  cultivown_land hh_head_edu area_responsible

reghdfe Charc  dist_forest FAW_distfore cultivown_land hh_head_edu area_responsible


***Distance as a natural enemy of FAW***

gen hbsfaw = asinh(f_FAW)
gen hbsdist = asinh(dist_forest)

reghdfe hbsfaw  hbsdist , absorb(i.camp2 )
outreg2 using results.tex, replace


quietly regress Charc l_FAW army_aff  l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize hh_head_sex
keep if e(sample)
// get means
foreach v in l_FAW army_aff  l_rainfall  s_lrainfall cultivown_land hh_head_edu hhsize  hh_head_sex {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx 
estimates store charc

coefplot (charc, asequation(l_FAW) \) ///
, drop(_cons) keep(l_FAW  army_aff) ///
mlabel(cond(@pval<.01, "***", cond(@pval<.05, "**", cond(@pval<.1, "*", "")))) mlabc(red) mlabsize(medium) mlabgap(6pt) mlabp(9) /// 
eqlabel(, labsize(small)) msymbol(circle) mcolor(ebblue) ciopts(recast(rcap) lcolor(ebblue)) /// 
 xline(0,lcolor(red)) ylabel(, labsize(vsmall)) omitted baselevels /// 
byopts(compact rows(1) note("p-values shown alongside markers" "*** p<.01, ** p<.05, * p<.1" ))*/

****Charcoal distribution curve**
*drop if Qcharc==0

/*twoway function Charc=normalden( fert),  xtitle("{it: x}") ///
ytitle("Density") title("Standard Normal Distribution")*/

***Charcoal and deforestaion correlation test 
egen Charcoal = sum(Charc), by(camp year)
egen defor = sum(deforestation_area_km_squared), by(camp year)
gen hh_minus_charcoal = Charcoal - Charc
gen hh_minus_defor = defor - deforestation_area_km_squared
corr  Charcoal defor
corr Charc deforestation_area_km_squared 
corr hh_minus_charcoal hh_minus_defor
corr deforestation_area_km_squared hh_minus_defor

reghdfe area_km2_buffer_1 Charc i.year, absorb (i.HHID) 
reghdfe area_km2_buffer_5 Charc i.year, absorb (i.HHID) 
estimates store deforestation 
reghdfe area_km2_buffer_10 Charc i.year, absorb (i.HHID) 
reghdfe area_km2_buffer_15 Charc i.year, absorb (i.HHID)

***Let's look at what is attributed to spraying***
reghdfe area_km2_buffer_10 Charc i.year, absorb (i.HHID)
estimates store defor_charcoal
reghdfe area_km2_buffer_10 spray i.year, absorb (i.HHID) 
estimates store defor_spray

***deforestation***
coefplot (defor_charcoal, asequation(Charcoal)\defor_spray, asequation(Defor_spray)\) ///
, drop(_cons) keep(spray Charc) ///
eqlabel(, labsize(small)) msymbol(circle) mcolor(ebblue) ciopts(recast(rcap) lcolor(ebblue)) xline(0,lcolor(red)) ylabel(, labsize(vsmall)) omitted baselevels /// 
bylabels("Deforestation rate (5 km buffer)") ///
byopts(compact rows(1) ) ///
name (debt_fl, replace) ysize (30) xsize(50)

***Charcoal and deforestation***
coefplot (deforestation, asequation(FE)\) ///
, drop(_cons) keep(l_FAW Charc) ///
eqlabel(, labsize(small)) msymbol(circle) mcolor(ebblue) ciopts(recast(rcap) lcolor(ebblue)) xline(0,lcolor(red)) ylabel(, labsize(vsmall)) omitted baselevels /// 
bylabels("Deforestation rate (5 km buffer)") ///
byopts(compact rows(1) ) ///
name (debt_fl, replace) ysize (30) xsize(50)






***Charcoal and yield IV***
ivreghdfe Charc  (l_FAW=lyield) , absorb (i.HHID) first cl(camp1)




***genetating the square of temp
gen squa_temp=temperature^2

ivreghdfe area_km2_buffer_10 Charc  temperature  squa_temp l_rainfall s_lrainfall i.year ,absorb (i.HHID) first


reghdfe deforestation_area_km_squared l_FAW i.year, absorb (i.HHID) 

reghdfe deforestation_area_km_squared l_FAW i.year, absorb (i.HHID) 



reghdfe area_km2_buffer_1 l_FAW i.year, absorb (i.HHID) 

reghdfe area_km2_buffer_5 l_FAW i.year, absorb (i.HHID) 

reghdfe area_km2_buffer_10 l_FAW i.year, absorb (i.HHID) 

reghdfe area_km2_buffer_15 l_FAW i.year, absorb (i.HHID) 

reghdfe area_km2_buffer_15 l_FAW cultivown_land hh_head_edu hhsize hh_head_sex i.year, absorb (i.HHID) 

gen log_defore=log(area_km2_buffer_5)
gen inv_sine = asinh(area_km2_buffer_5)

gen cam_year=camp2*year

ihstrans area_km2_buffer_5

ihstrans area_km2_buffer_10

ihstrans Charc

ihstrans temperature
ihstrans rainfall




ivreghdfe area_km2_buffer_5 (Charc=l_FAW) l_rainfall s_lrainfall temperature squa_temp cam_year i.year , absorb (i.district) first

ivreghdfe log_defore (Charc=l_FAW) l_rainfall s_lrainfall temperature squa_temp cam_year i.year , absorb (i.district) first

ivreghdfe inv_sine (Charc=l_FAW) l_rainfall s_lrainfall temperature squa_temp cam_year i.year , absorb (i.district) first

ivreghdfe ihs_area_km2_buffer_5 (Charc=l_FAW) l_rainfall s_lrainfall temperature squa_temp    i.year , absorb (i.district) first

gen hh_year = HHID*year

ivreghdfe ihs_area_km2_buffer_5 (Charc=l_FAW) temperature rainfall i.HHID  , absorb (i.year) first 

reghdfe ihs_area_km2_buffer_5 l_FAW temperature rainfall   , absorb (i.year) 

reghdfe ihs_area_km2_buffer_5 ihs_Charc temperature ihs_rainfall i.HHID  , absorb (i.year) 


ivreghdfe ihs_area_km2_buffer_10 (Charc=camIV) l_rainfall s_lrainfall temperature squa_temp   , absorb (i.district) first

gen def = (ihs_area_km2_buffer_5>0)


***Running deforestaion as a binary variable**** 

reghdfe def l_FAW  l_rainfall s_lrainfall temperature squa_temp, absorb (i.district)

clogit def l_FAW l_rainfall s_lrainfall temperature squa_temp, group(district)

*scobit def l_FAW l_rainfall s_lrainfall temperature squa_temp, vce(cluster district)

****generating GDD and KDD *****
egen Tmax = rowmax(temperature)
egen Tmin = rowmin(temperature)
gen Tbase = 10

gen gdd = ( ( (Tmax + Tmin) / 2) - 10 )
label var gdd "Maize growing degree-days: base 10 C"

gen kgdd = gdd + 2
* if the average temperature is less than or equal to Tbase, then GDD is equal to zero
replace gdd = 0 if gdd < 0

hist gdd, normal

* kernel density plot
kdensity gdd


gen kdd=temperature
replace kdd = 0 if kdd < 29

***regression****


**Chacoal and deforestaion
rename  deforestation_area_km_squared  Deforestion
reghdfe Deforestio Charc   , absorb (i.HHID) 
/*reghdfe Deforestion l_FAW cultivown_land hh_head_edu hhsize hh_head_sex rainfall temperature , absorb (i.district) resid
predict def_res, residuals
gen any_army=1 if army==1 & year==2017 | army==1 & year==2018|army==1 & year==2019
replace any_army=0 if any_army==.
collapse mean def_res,by (any_army year)
twoway line def_res year*/
gen campyear=camp2*year
reghdfe Deforestion Charc temperature squa_temp l_rainfall s_lrainfall , absorb (i.HHID i.district i.year i.campyear) 

reghdfe Deforestion Charc kdd gdd l_rainfall s_lrainfall , absorb (i.HHID i.year i.camp2 i.campyear) 


reghdfe Deforestion Charc   , absorb (i.HHID) 


gen lag_defo5=L.area_km2_buffer_5
gen lag_defo10=L.area_km2_buffer_10

reghdfe temperature lag_defo5 l_rainfall farmland i.year , absorb (i.HHID)
outreg2 using results, excel replace
reghdfe temperature area_km2_buffer_10 l_rainfall farmland i.year , absorb (i.HHID)
outreg2 using results, excel append
reghdfe temperature area_km2_buffer_15 l_rainfall farmland i.year , absorb (i.HHID)
outreg2 using results, excel append



reghdfe Deforestion l_FAW cultivown_land hh_head_edu hhsize hh_head_sex l_rainfall s_lrainfall temperature , absorb (i.HHID)
xtreg Deforestion l_FAW cultivown_land hh_head_edu hhsize hh_head_sex rainfall temperature i.year


tobit Deforestion Charc l_rainfall s_lrainfall farmland hh_head_edu hhsize hh_head_sex , vce(cluster HHID)

outreg2 using results, excel replace
ivreghdfe Deforestion (Charc=l_FAW) , absorb (i.HHID) first 
ivreghdfe Deforestion (Charc=l_FAW) , absorb (i.camp1) first 

ivreghdfe Deforestion (Charc=army_aff)  , absorb (i.HHID) first 
ivreghdfe Deforestion (Charc=army), absorb (i.HHID) first
ivreg Deforestion (Charc=l_FAW),first
ivreghdfe Deforestion (Charc=l_FAW) l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize hh_head_sex temperature , absorb (i.camp2) first 
ivreg Deforestion (Charc=l_FAW),first
ivreghdfe Deforestion (Charc=l_FAW) l_rainfall  cultivown_land hh_head_edu hhsize hh_head_sex l_rainfall temperature i.year,absorb (i.district) first
ivreghdfe Deforestion (Charc=l_FAW) rainfall  cultivown_land hh_head_edu hhsize hh_head_sex  temperature i.year,absorb (i.camp2) first
ivreghdfe Deforestion (Charc=l_FAW) rainfall  cultivown_land hh_head_edu hhsize hh_head_sex  temperature ,absorb (i.HHID) 
reghdfe Deforestion Charc rainfall  cultivown_land hh_head_edu hhsize hh_head_sex l_rainfall temperature , absorb (i.camp2)
ivreghdfe Deforestion (Charc=l_FAW) rainfall cultivown_land hh_head_edu hh_head_sex qbasal qtop temperature i.year,absorb (i.camp1) first
ivreghdfe Deforestion (Charc=l_FAW) rainfall lag_land hh_head_edu hh_head_sex  temperature i.year,absorb (i.camp1) first
ivreghdfe Deforestion (Charc=l_FAW) rainfall lag_land hh_head_edu hh_head_sex  temperature  qtop qbasal qseed ,absorb (i.district) first
ivreghdfe Deforestion (Charc=l_FAW) l_rainfall s_lrainfall cultivown_land hh_head_edu hh_head_sex  hpstemperature i.year  ,absorb (i.camp1) first
reghdfe Deforestion l_FAW rainfall lag_land hh_head_edu hh_head_sex  temperature i.year ,absorb (i.HHID) 


/***Trends sample graph**
gen FAW15 = 0
replace FAW15=1 if army==1 &year==2015

gen FAW16 = 0
replace FAW16=1 if army==1 &year==2016

gen FAW17 = 0
replace FAW17=1 if army==1 &year==2017

gen FAW18 = 0
replace FAW18=1 if army==1 &year==2018

gen FAW19 = 0
replace FAW19=1 if army==1 &year==2019
gen arm_lag=L.army*/




outreg2 using results, excel append
reghdfe Deforestion Charc  , absorb (i.district) 
outreg2 using results, excel append
*ivreghdfe Deforestion  (Charc=l_FAW) l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize hh_head_sex temperature sq_temp , absorb (i.HHID) first 
outreg2 using results, excel append






**Pollution
/*rename mean pollution
ivreghdfe pollution (Charc=l_camIV) cultivown_land  hhsize hh_head_sex temperature rainfall i.year , absorb (i.HHID) 

ivreghdfe pollution (Charc=l_camIV) cultivown_land  hhsize hh_head_sex temperature rainfall  , absorb (i.camp1) first*/

***Income low income
gen Ln_income=ln(ag_inc)
gen ag_army=Ln_income*army_aff
ivreghdfe Ln_income army_aff l_rainfall s_lrainfall i.year , absorb (i.HHID) 

*ivreghdfe Charc  l_rainfall  s_lrainfall y2017 y2018 y2019 (l_FAW=l_camIV) , absorb (i.HHID) first 
*outreg2 using results, excel append

**Spray as a robustness check
reghdfe Charc l_spray l_FAW l_rainfall s_lrainfall  ,absorb (i.HHID) cl(camp1)
outreg2 using results, excel replace


**HHFE tries
reghdfe Charc army_aff  farmland log_inc l_rainfall sq_rainfall sq_inc fert vouch  , absorb (i.HHID)  
outreg2 using results, excel replace
ivreghdfe Charc camIV  farmland log_inc l_rainfall sq_rainfall  sq_inc fert vouch  , absorb (i.HHID) 
outreg2 using results, excel append
ivreghdfe Charc  farmland log_inc log_rain l_rainfall sq_rainfall sq_inc fert  vouch   (army_aff=camIV) , absorb (i.HHID) first 
outreg2 using results, excel append



gen inarmy_land=farmland*army_aff

ihstrans inarmy_land
*rename inarmy_land_w hpscapital


***Main results
egen sum_cam = sum(l_FAW), by(camp year)
gen hh_minus_camp_mea = sum_camp - l_FAW


gen camITT = hh_minus_camp_mea /camp_one


egen camp_ave = mean(l_FAW), by(camp year)
*reghdfe Charc l_FAW  l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize hh_head_sex ltemperature sq_temp, absorb ( i.district)
outreg2 using results.text, replace



reghdfe Charc camp_ave  l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize hh_head_sex ltemperature sq_temp i.year, absorb ( i.district)

reghdfe Charc camp_ave  l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize hh_head_sex i.year, absorb ( i.district)

reghdfe Charc camp_ave  l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize hh_head_sex i.year, absorb ( i.district)

ivreghdfe Charc  (l_FAW=lyield)  , absorb (i.HHID) first cl(camp1)
ivreghdfe Charc  (l_FAW=lyield)  l_rainfall s_lrainfall  temperature sqtemp i.year, absorb (i.HHID) first cl(camp1)
ivreghdfe Charc  (l_FAW=lyield)  l_rainfall s_lrainfall  temperature sqtemp i.year, absorb (i.HHID) first cl(camp1)
ivreghdfe Charc  (l_FAW=lyield) cultivown_land hhsize fert qseed l_rainfall s_lrainfall  temperature sqtemp i.year, absorb (i.HHID) first cl(camp1)

/*quietly regress Charc camITT  l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize hh_head_sex
keep if e(sample)
// get means
foreach v in l_FAW  l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize  hh_head_sex {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx*/

/*quietly regress Charc camp_ave  l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize hh_head_sex ltemperature sq_temp
keep if e(sample)
// get means
foreach v in camp_ave  l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize  hh_head_sex ltemperature sq_temp {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx*/
*gen Lead_FAW=F.army_aff
****The current FAW***
*reghdfe  Charc Lead_FAW l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize hh_head_sex ltemperature sq_temp , absorb ( i.district)

reghdfe  Charc l_FAW army_aff l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize hh_head_sex ltemperature sq_temp , absorb ( i.district)



/*quietly regress Charc l_FAW army_aff l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize hh_head_sex ltemperature sq_temp
keep if e(sample)
// get means
foreach v in  army_aff l_FAW l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize  hh_head_sex ltemperature sq_temp {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx*/

/*quietly regress Charc l_FAW Lead_FAW l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize hh_head_sex ltemperature sq_temp
keep if e(sample)
// get means
foreach v in  l_FAW Lead_FAW l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize  hh_head_sex ltemperature sq_temp {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx*/

***Plots fot the main results***

reghdfe Charc l_FAW  l_rainfall s_lrainfall   hhsize  ltemperature sq_temp, absorb(i.HHID i.camp1 i.district) cl(HHID)
estimates store Lag_FAW

reghdfe Charc l_FAW l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize hh_head_sex , absorb(i.HHID) 
*estimates store Ave_campFAW
quietly regress Charc l_FAW l_spray spray_FAW l_rainfall s_lrainfall ltemperature sq_temp 
keep if e(sample)
// get means
foreach v in l_FAW l_spray spray_FAW  l_rainfall s_lrainfall  ltemperature sq_temp {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx
estimates store Spray

*coefplot Lag_FAW Ave_campFAW  Spray , drop (_cons l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize hh_head_sex ltemperature sq_temp ) xline(0) nolabels
 

quietly regress Charc l_FAW  l_rainfall s_lrainfall ltemperature sq_temp
keep if e(sample)
// get means
foreach v in l_FAW ltemperature sq_temp  l_rainfall s_lrainfall {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx
estimates store cre

 
reghdfe Charc camp_ave cultivown_land hh_head_edu hhsize  hh_head_sex  l_rainfall s_lrainfall   hhsize  ltemperature sq_temp, absorb(i.HHID i.camp1 i.district)  cl(HHID)

estimates store Ave_campFAW


coefplot (Lag_FAW, asequation(LPM) \ cre, asequation(IV) \) ///
(charc, asequation(Lag_FAW) \ ) ///
, drop(_cons) keep(l_FAW army_aff) ///
eqlabel(, labsize(small)) msymbol(circle) mcolor(ebblue) ciopts(recast(rcap) lcolor(ebblue)) /// 
 xline(0,lcolor(red)) ylabel(, labsize(vsmall)) omitted baselevels /// 
bylabels( "Main results" "Leads test") ///
byopts(compact rows(1) note("p-values shown alongside markers" "*** p<.01, ** p<.05, * p<.1" ))


coefplot (Lag_FAW, asequation(LPM) \ cre, asequation(IV) \ Spray, asequation(FE) \) ///
(charc, asequation(Lag_FAW) \ ) ///
, drop(_cons) keep(l_FAW ) ///
eqlabel(, labsize(small)) msymbol(circle) mcolor(ebblue) ciopts(recast(rcap) lcolor(ebblue)) /// 
 xline(0,lcolor(red)) ylabel(, labsize(vsmall)) omitted baselevels /// 
bylabels( "Main results" "Leads test") ///
byopts(compact rows(1) note("p-values shown alongside markers" "*** p<.01, ** p<.05, * p<.1" ))



coefplot (Lag_FAW, asequation(LPM) \ cre, asequation(IV) \ Spray, asequation(FE) \) ///
||(charc, asequation(Lag_FAW) \ ) ///
, drop(_cons) keep(l_FAW army_aff) ///
eqlabel(, labsize(small)) msymbol(circle) mcolor(ebblue) ciopts(recast(rcap) lcolor(ebblue)) /// 
 xline(0,lcolor(red)) ylabel(, labsize(vsmall)) omitted baselevels /// 
bylabels( "Main results" "Leads test") ///
byopts(compact rows(1) note("p-values shown alongside markers" "*** p<.01, ** p<.05, * p<.1" ))


***Let's now look at the effect of FAW on quantities of charcoal produced***
**Charc quatitity regression
*keep if year==2018|year==2019
/*gen lCharc=log(Qcharc)
gen lCharc=ln(Qcharc)
gen qchar=Qcharc*50
reghdfe lCharc l_FAW cultivown_land  l_rainfall s_lrainfall   hhsize  ltemperature sq_temp, absorb (i.HHID)

reghdfe qchar l_FAW l_rainfall s_lrainfall ltemperature sq_temp, absorb (i.HHID i.camp2 i.year i.district) cl(HHID)
estimates store charcno
 
reghdfe qchar l_FAW cultivown_land  l_rainfall s_lrainfall   hhsize  ltemperature sq_temp, absorb (i.HHID i.camp2 i.year i.district) cl(HHID)
estimates store qcharcyes

coefplot ( qcharcyes, asequation(FE with controls)) ///
, drop(_cons) keep(l_FAW) ///
eqlabel(, labsize(small)) msymbol(circle) mcolor(ebblue) ciopts(recast(rcap) lcolor(ebblue)) /// 
 xline(0,lcolor(red)) ylabel(, labsize(vsmall)) omitted baselevels /// 
byopts(compact rows(1) note("p-values shown alongside markers" "*** p<.01, ** p<.05, * p<.1" ))


coefplot (qcharcyes, asequation(FE)\) ///
||(deforestation, asequation(FE)\) ///
, drop(_cons) keep(l_FAW Charc) ///
eqlabel(, labsize(small)) msymbol(circle) mcolor(ebblue) ciopts(recast(rcap) lcolor(ebblue)) xline(0,lcolor(red)) ylabel(, labsize(vsmall)) omitted baselevels /// 
bylabels("Charcoal produced (quantities)" "Deforestation rate (5 km buffer)") ///
byopts(compact rows(1) ) ///
name (debt_fl, replace) ysize (30) xsize(50)

reghdfe Deforestion qchar  l_rainfall s_lrainfall, absorb (i.HHID) 

reghdfe Deforestion lCharc l_FAW l_rainfall s_lrainfall, absorb (i.HHID) 
ivreghdfe Deforestion   l_rainfall s_lrainfall i.year (Charc=army_aff) , absorb (i.HHID) first 
reghdfe Charc l_FAW  l_rainfall s_lrainfall year , absorb (i.HHID) 
reghdfe Deforestion Charc   l_rainfall s_lrainfall year , absorb (i.HHID) 
reghdfe qchar l_FAW l_rainfall s_lrainfall  , absorb (i.district)
reghdfe Qcharc l_FAW l_rainfall s_lrainfall i.year  , absorb (i.district)
reghdfe Qcharc  l_FAW y2017 y2018 y2019, absorb (i.district)*/

reghdfe  Charc army_aff l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize hh_head_sex ltemperature sq_temp , absorb ( i.district)

*Robust by running it as a categorical variable
reghdfe Charc i.l_FAW  l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize hh_head_sex , absorb ( i.district)
outreg2 using results.tex, replace

/*quietly regress Charc l_FAW l_spray spray_FAW l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize hh_head_sex
keep if e(sample)
// get means
foreach v in l_FAW l_spray spray_FAW  l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize  hh_head_sex {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx*/

/*quietly regress Charc l_FAW  l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize hh_head_sex
keep if e(sample)
// get means
foreach v in l_FAW  l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize  hh_head_sex {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx*/



/*quietly regress Charc l_FAW  l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize hh_head_sex ltemperature sq_temp
keep if e(sample)
// get means
foreach v in l_FAW  l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize  hh_head_sex ltemperature sq_temp {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx*/


/*quietly regress Charc l_FAW l_spray spray_FAW l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize hh_head_sex ltemperature sq_temp
keep if e(sample)
// get means
foreach v in l_FAW l_spray spray_FAW  l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize  hh_head_sex ltemperature sq_temp {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx*/

/*quietly regress Charc camp_ave  l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize hh_head_sex ltemperature sq_temp
keep if e(sample)
// get means
foreach v in l_FAW  l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize  hh_head_sex ltemperature sq_temp {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx*/

reghdfe army_aff  ltemperature l_rainfall s_lrainfall i.year , absorb ( i.district)

reghdfe army  temperature rainfall , absorb ( i.district)

reghdfe army  temperature , absorb ( i.district)

reghdfe l_FAW  temperature sqtemp l_rainfall s_lrainfall i.year , absorb ( i.district)
outreg2 using results.tex, replace

reghdfe l_FAW  temperature sqtemp l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize  hh_head_sex i.year , absorb ( i.district)
outreg2 using results.tex, replace



reghdfe Charc l_FAW  l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize hh_head_sex temperature sq_temp , absorb ( i.district)

/*
foreach v in l_FAW  l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize  hh_head_sex temperature sqtemp {
    bysort HHID : egen double mean_`v' = mean(`v')
}
xtprobit Charc mean_l_FAW  mean_l_rainfall mean_s_lrainfall mean_cultivown_land mean_hh_head_edu mean_hhsize  mean_hh_head_sex temperature sqtemp
outreg2 using results, excel replace
/*quietly regress Charc l_FAW  l_rainfall s_lrainfall farmland hh_head_edu hhsize hh_head_sex
keep if e(sample)
// get means
foreach v in l_FAW  l_rainfall s_lrainfall farmland hh_head_edu hhsize  hh_head_sex {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx*/
outreg2 using results, replace
*outreg2 using results.tex, replace

reghdfe Charc l_FAW   l_rainfall s_lrainfall farmland hh_head_edu hhsize hh_head_sex , absorb ( i.district) cl(camp1)
*outreg2 using results, excel append
outreg2 using results.tex, append*/




/*quietly regress Charc l_FAW l_spray spray_FAW l_rainfall s_lrainfall farmland hh_head_edu hhsize hh_head_sex
keep if e(sample)
// get means
foreach v in l_FAW l_spray spray_FAW  l_rainfall s_lrainfall farmland hh_head_edu hhsize  hh_head_sex {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx*/
outreg2 using results, excel replace





reghdfe Charc l_FAW  l_spray spray_FAW l_rainfall s_lrainfall farmland hh_head_edu hhsize hh_head_sex , absorb ( i.district) cl(camp1)
outreg2 using results, excel append


**COREELATED RANDOM EFFECTS
/*reghdfe Charc l_FAW   l_rainfall s_lrainfall farmland hh_head_edu hhsize hh_head_sex  , absorb ( i.district) cl(camp1)
outreg2 using results, excel replace

reghdfe Charc l_FAW l_spray spray_FAW  l_rainfall s_lrainfall farmland hh_head_edu hhsize hh_head_sex  , absorb ( i.district) cl(camp1)
outreg2 using results, excel append

reghdfe Charc l_FAW l_spray spray_FAW  l_rainfall s_lrainfall farmland hh_head_edu hhsize hh_head_sex  , absorb ( i.district) cl(camp1)
outreg2 using results, excel append

quietly regress Charc l_FAW l_spray spray_FAW l_rainfall s_lrainfall farmland hh_head_edu hhsize hh_head_sex
keep if e(sample)
// get means
foreach v in l_FAW l_spray spray_FAW  l_rainfall s_lrainfall farmland hh_head_edu hhsize  hh_head_sex {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx
outreg2 using results, excel replace*/




/*quietly regress Charc l_FAW l_rainfall s_lrainfall farmland hh_head_edu hhsize hh_head_sex 
keep if e(sample)
// get means
foreach v in l_FAW l_rainfall s_lrainfall farmland hh_head_edu hhsize hh_head_sex  {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx
outreg2 using results, excel replace




reghdfe Charc l_FAW l_spray spray_FAW  l_rainfall s_lrainfall , absorb ( i.district) cl(camp1)
outreg2 using results, excel append

// decalre panel
xtset HHID year
// cre
xtprobit Charc l_FAW l_spray spray_FAW  l_rainfall s_lrainfall farmland hh_head_edu hhsize hh_head_sex mean*,re
. mfx*/







*by HHID(year) : egen varmean = mean(var)


*Interacting the FAW intensity variable
gen sq_rainf=l_rainfall^2


* Kathy suggested not inclucing veterinary expenses "veterinary_cost_month"
destring clothing_cost_month, generate(clothing_cost_mont) force
destring alcohol_cost_month, generate(alcohol_cost_mont) force
destring charcoal_cost_month, generate(charcoal_cost_mont) force
destring firewood_cost_month, generate(firewood_cost_mont) force
foreach v in food_budget_7day talktime_budget_7day clothing_cost_mont transportation_cost_mont ///
	alcohol_cost_mont firewood_cost_mont charcoal_cost_mont other_cost_month {
	recode `v' (.=0)
	tab `v'
}

** HH expenditure with food expenditure included
egen hhexp1 = rowtotal(food_budget_7day talktime_budget_7day) // weekly exp. items
egen hhexp2 = rowtotal(clothing_cost_mont transportation_cost_month alcohol_cost_mont firewood_cost_mont charcoal_cost_mont other_cost_month) // monthly exp. items

gen hhexp_yr = (52 * hhexp1) + (12 * hhexp2)

label var hhexp_yr "Annual HH expenditure w/ food: Kwachas"

** HH expenditure without food expenditure included
gen hhexp_yr2 = (52 * talktime_budget_7day) + (12 * hhexp2)

label var hhexp_yr2 "Annual HH expenditure w/o food: Kwachas"

drop hhexp1 hhexp2
bys year: tab hhexp_yr, m
bys year: tab hhexp_yr2, m

tw (kdensity hhexp_yr if year==2016)(kdensity hhexp_yr if year==2017) ///
	(kdensity hhexp_yr if year==2018)(kdensity hhexp_yr if year==2019)

tw (kdensity hhexp_yr2 if year==2016)(kdensity hhexp_yr2 if year==2017) ///
	(kdensity hhexp_yr2 if year==2018)(kdensity hhexp_yr2 if year==2019)
	

* generating a variable for food as a proportion total hh expenditure
gen foodexp_share = (52 * food_budget_7day) / hhexp_yr
label var foodexp_share "Percentage of expenditures on food"
bys year: tab foodexp_share, m

tw (kdensity foodexp_share if year==2016)(kdensity foodexp_share if year==2017)


* generating log of annual hh expenditure
** with food
gen lnhhexp_yr = log(hhexp_yr)
label var lnhhexp_yr "Log of annual HH expenditure w/ food"
bys year: tab lnhhexp_yr, m

tw (kdensity lnhhexp_yr if year==2016)(kdensity lnhhexp_yr if year==2017) ///
	(kdensity lnhhexp_yr if year==2018)(kdensity lnhhexp_yr if year==2019)

** without food
gen lnhhexp_yr2 = log(hhexp_yr2)
label var lnhhexp_yr2 "Log of annual HH expenditure w/o food"

tw (kdensity lnhhexp_yr2 if year==2016)(kdensity lnhhexp_yr2 if year==2017) ///
	(kdensity lnhhexp_yr2 if year==2018)(kdensity lnhhexp_yr2 if year==2019)

** Log of Annual food expenditure
gen lnhhexp_yr_food = log(52 * food_budget_7day)
label var lnhhexp_yr_food "Log of annual HH food expenditure"

tw (kdensity lnhhexp_yr_food if year==2016)(kdensity lnhhexp_yr_food if year==2017) ///
	(kdensity lnhhexp_yr_food if year==2018)(kdensity lnhhexp_yr_food if year==2019)

	
** generate household food expenditure
gen hhfoodexp_yr = 52 * food_budget_7day
label var hhfoodexp_yr "Annual HH food expenditure"


** generate log of household food expenditure
gen lnhhfoodexp_yr = log(hhfoodexp_yr)
label var lnhhfoodexp_yr "Log of annual HH food expenditure"

tw (kdensity lnhhfoodexp_yr if year==2016)(kdensity lnhhfoodexp_yr if year==2017) ///
	(kdensity lnhhfoodexp_yr if year==2018)(kdensity lnhhfoodexp_yr if year==2019)

** Principal Component Analysis
* See: https://jbhender.github.io/Stats506/F17/Projects/G18.html

* Step 1
br oxen breeding_bull donkey female_cattle_number goat_sheep_number poultry_number pigs_number asset_phone tv radio bike motorcycle ox_carts vehicle water_pump plough sprayers

encode pigs_number, gen(pigs_count)

pca oxen breeding_bull donkey female_cattle_number goat_sheep_number ///
	poultry_number pigs_count asset_phone tv radio bike motorcycle ox_carts ///
	vehicle water_pump plough sprayers
* Component 1 explains only 25.9% of the variation in the data. Components 1-13 explain 90% of the variation in the data
	
* Step 2
* rotate
predict 	assetind_pca
label var assetind_pca	"Household Asset Index (PCA)"
sum 		assetind_pca
tab 		assetind_pca	
screeplot, ci(asympt level(95)) mean scheme(lean2)
loadingplot
*ttest assetind_pca, by(treated)



* -----------------------------------------------------------
* Generating the Household Asset Index, using ILRI's methos
* -----------------------------------------------------------

foreach v in oxen breeding_bull donkey female_cattle_number goat_sheep_number poultry_number pigs_count asset_phone tv radio bike motorcycle ox_carts vehicle water_pump plough sprayers {
	replace `v' = 0 if `v'==.
}

** Generating the Animal asset index
* Note: The survey did not ask the # of oxen, breeding bull and donkeys. We assumed Yes=1 of each
gen animalind =  10*(oxen + breeding_bull + donkey + female_cattle_number) ///
+ 3*goat_sheep_number + 1*poultry_number + 2*pigs_count
*label var animalind "Aminal asset index"


** Generating the Domestic asset index
gen domesticind = 3*asset_phone + 4*tv + 2*radio
*label var domesticind "Domestic asset index"

** Generating the Transport assets index
* Note: oxen carts are included here in ILRI document i/o in productive assets
gen transportind = 6*bike + 48*motorcycle + 12*ox_carts + 160*vehicle
*label var transportind "Transport asset index"

// Generating the Productive assets index
* Note: water_pump is treddle pump in the survey
* Note: ILRI's document does not contain "sprayers", so I used the same weight as for "Ploughs"
gen productiveind = 6*water_pump + 4*(plough + sprayers)
*label var productiveind "Productive asset index"

** Generating the Household domestic asset index
gen assetind_ilri = domesticind + animalind + transportind + productiveind //  "domesticind" is not included b/c these variables are missing in 2017

gen assetind =  animalind  + productiveind 
label var assetind_ilri "HH asset index (ILRI method)"
bysort year: tab assetind_ilri, m
tw (kdensity assetind_ilri if year==2016)(kdensity assetind_ilri if year==2017)

**Asset index interaction**
gen ass_index1= army_aff*assetind_ilri
gen ass_index = log(ass_index1)
*sort assetind_ilri
sort year HHID
order year HHID assetind_ilri

sort assetind_ilri, stable
*gen l_assetind=L.assetind_ilri
sort assetind_ilri, stable

gen ani_ass=l_FAW*animalind

***District by year FE
*****FS and interactions

/***Non Ag Income
reghdfe Charc l_FAW Ln_income ag_army l_rainfall s_lrainfall i.year, absorb( i.district) cl(camp2)
*outreg2 using results2, excel replace
quietly regress Charc l_FAW Ln_income ag_army l_rainfall s_lrainfall
keep if e(sample)
// get means
foreach v in  l_FAW Ln_income ag_army l_rainfall s_lrainfall {
    bysort HHID : egen double mean_`v' = mean(`v')
}
// decalre panel
xtset HHID year
// cre
xtprobit Charc l_FAW Ln_income ag_army l_rainfall s_lrainfall mean*,re
. mfx*/
gen credit=borrow2500*army_aff
gen credit2=borrow500*army_aff
gen credit3=borrow10000*army_aff
*drop if borrow500==.
***Access to credit
gen army_cult=cultivown_land*army_aff
gen army_cap=capital*army_aff

reghdfe Charc l_FAW  borrow10000 credit l_rainfall s_lrainfall  ltemperature sq_temp, absorb( i.HHID) cl(camp2)
outreg2 using results2, excel replace
estimates store accesstocredit

reghdfe Charc l_FAW army_cult cultivown_land l_rainfall s_lrainfall  ltemperature sq_temp, absorb(i.year ) cl(camp2)
outreg2 using results2, excel append
estimates store cultivated

reghdfe Charc l_FAW  capital army_cap , absorb(i.year i.district) cl(camp2)
outreg2 using results2, excel append
estimates store capital

reghdfe Charc l_FAW Ln_income ag_army i.year, absorb( i.district) cl(camp2)
outreg2 using results2, excel append
estimates store income 

reghdfe Charc l_FAW dist_cha charc i.year, absorb(district)
estimates store distance_forest

coefplot (accesstocredit, asequation(Accscredit) \ ) ///
|| (cultivated, asequation(Landcultivated) \ ) ///
||  (capital, asequation(Capitall) \ ) ///
|| (income, asequation(Income)\ ) ///
|| (distance_forest, asequation(distand)\ ) ///
, drop(_cons) keep(l_FAW  borrow10000 credit army_cult cultivown_land capital army_cap Ln_income ag_army dist_cha charc) ///
eqlabel(, labsize(small)) msymbol(circle) mcolor(ebblue) ciopts(recast(rcap) lcolor(ebblue)) ///
 xline(0,lcolor(red)) ylabel(, labsize(vsmall)) omitted baselevels ///  
bylabels( "FAW" "Charcoal (1 = Yes)" "Charcoal (1 = Yes)" "Charcoal (1 = Yes)" ///
"Charcoal (1 = Yes)") ///
byopts(compact rows(1) note("p-values shown alongside markers" "*** p<.01, ** p<.05, * p<.1" ))



gen ass_FAW= army_aff*assetind_ilri
gen ass_FAW2= log(ass_FAW)
gen log_ass=log(assetind_ilri)
reghdfe Charc l_FAW log_ass ass_FAW  i.year , absorb( i.district) cl(camp2) 
outreg2 using results2, excel append

***increased distance***
gen distfire  = (firewood_av10 ==1)

gen dist_charc= army_aff*firewood_av10

gen distfirefaw= army_aff*distfire


gen charc=log(firewood_av10)
gen dist_firew = asinh(firewood_av10)
gen firewood_FAW=dist_firew*l_FAW
gen dist_cha=log(dist_charc)
reghdfe Charc l_FAW dist_charc firewood_av10 i.year, absorb(district)

reghdfe Charc l_FAW dist_cha charc i.year, absorb(district)

reghdfe Charc l_FAW firewood_FAW dist_firew  i.year, absorb(district)


reghdfe area_km2_buffer_5 l_FAW firewood_FAW dist_firew  i.year, absorb(district)

reghdfe Charc l_FAW distfire distfirefaw  i.year, absorb(i.camp2)


/*quietly regress Charc l_FAW  firewood_FAW dist_firew l_rainfall s_lrainfall
keep if e(sample)
// get means
foreach v in l_FAW  firewood_FAW dist_firew l_rainfall s_lrainfall {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx
*/



*xtprobit Charc mean_l_FAW mean_firewood_FAW mean_dist_firew
outreg2 using results, excel replace




/*quietly regress Charc l_FAW Ln_income ag_army l_rainfall s_lrainfall
keep if e(sample)
// get means
foreach v in  l_FAW borrow500 credit l_rainfall s_lrainfall {
    bysort HHID : egen double mean_`v' = mean(`v')
}
// decalre panel
xtset HHID year
// cre
xtprobit Charc l_FAW borrow500 credit l_rainfall s_lrainfall mean*,re
. mfx
outreg2 using results2, excel append*/

/****Farmland
gen army_farmland=cultivown_land*army_aff
quietly regress Charc l_FAW army_farmland cultivown_land l_rainfall s_lrainfall
keep if e(sample)
// get means
foreach v in  l_FAW cultivown_land army_farmland l_rainfall s_lrainfall {
    bysort HHID : egen double mean_`v' = mean(`v')
}
// decalre panel
xtset HHID year
// cre
xtprobit Charc l_FAW farmland cultivown_land l_rainfall s_lrainfall mean*,re
. mfx
outreg2 using results2, excel append*/

*reghdfe Charc l_FAW army_farmland farmland l_rainfall s_lrainfall  i.year, absorb( i.district) 

*outreg2 using results2, excel append

***Capital

/*quietly regress Charc l_FAW army_cap capital l_rainfall s_lrainfall
keep if e(sample)
// get means
foreach v in l_FAW army_cap capital l_rainfall s_lrainfall {
    bysort HHID : egen double mean_`v' = mean(`v')
}
// decalre panel
xtset HHID year
// cre
xtprobit Charc l_FAW army_cap capital l_rainfall s_lrainfall mean*,re
. mfx
outreg2 using results2, excel append*/


reghdfe Charc l_FAW army_cap capital l_rainfall s_lrainfall  , absorb( i.district) 
outreg2 using results2, excel append

***asset_index
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
*outreg2 using results2, excel append

***Animal

gen animal=log(animalind)
reghdfe Charc l_FAW ani_ass animalind  l_rainfall s_lrainfall, absorb( i.HHID) 


reghdfe qchar l_FAW ani_ass animalind  l_rainfall s_lrainfall ltemperature sq_temp, absorb( i.HHID i.year) 


 **Other coping mechanisms
***Nom-ag income****
gen lnnon_ag = log(non_aginc)
*gen lnonag=log(non_ag_inc)
gen lwork=log(income_piecework)

reghdfe lnnon_ag l_FAW  l_rainfall s_lrainfall , absorb( i.district) 
outreg2 using results2, excel replace
reghdfe lnnon_ag l_FAW  l_rainfall s_lrainfall dist_yr i.year, absorb( i.HHID) 
reghdfe lwork l_FAW  l_rainfall s_lrainfall, absorb( i.HHID) 



***Expenditure on food**
reghdfe lnhhfoodexp_yr l_FAW  l_rainfall s_lrainfall i.year dist_yr, absorb( i.HHID) 
outreg2 using results2, excel replace

***Percentage of hh expenduture on food***
reghdfe lnhhexp_yr l_FAW  l_rainfall s_lrainfall i.year, absorb( i.HHID) 
outreg2 using results2, excel replace

***Access to credit
reghdfe borrow500 l_FAW  l_rainfall s_lrainfall i.year, absorb( i.HHID) 
outreg2 using results2, excel replace




****Number of crops as coping mechanism
// Hectares of other field crops 
gen farmsize_other = farmland_clean - farmsize_W01
label var farmsize_other "Hectares of other field crops planted: Ha"

bys year: sum farmsize_other farmsize_W01
bys year: tab farmsize_other

hist farmsize_other

tw (kdensity farmsize_other) (kdensity farmsize_W01)


gen lnfarmsize_other = log(farmsize_other)
label var lnfarmsize_other "Log Hectares of other field crops planted"

tw (kdensity lnfarmsize_other) (kdensity lnfarmsize_W01)

reghdfe lnfarmsize_other l_FAW  l_rainfall s_lrainfall, absorb( i.HHID) 



// =1 if grew at least one non-maize field crop
gen grewothercrop = (farmland_clean > farmsize_W01)
label var grewothercrop "=1 if grew at least one non-maize field crop"

replace grewothercrop = . if farmland_clean==. & farmsize_W01==.

bys year: tab grewmaize // Did you grow maize during this season?
bys year: tab inter_1 // Did you intercrop?

forval i=1/5 {
	recode inter_`i' (2=0)
	label define inter_`i' 0 "No" 1 "Yes", replace
	label values inter_`i' inter_`i'
	bys year: tab inter_`i'
}


// Generate a variable for Maize share of total area planted
gen maizearea_share = farmsize_W01 / farmland_clean
label var maizearea_share "Maize share of total area planted"

bys year: sum maizearea_share
bys year: tab maizearea_share

hist maizearea_share

***Spraying**
reghdfe Charc l_FAW  l_spray spray_FAW l_rainfall s_lrainfall i.year, absorb ( i.district) cl(camp1)
outreg2 using results2, excel replace
egen camp_spray = mean(spray), by(camp year)
reghdfe l_spray l_FAW l_rainfall s_lrainfall i.year, absorb ( i.district) cl(camp1)
outreg2 using results2, excel replace

***coping mechanisms***
***maize share
reghdfe maizearea_share  l_FAW l_rainfall s_lrainfall temperature sqtemp, absorb( i.district) cl(camp2)
estimates store maizearea_share
gen log_maizeshare = log(maizearea_share)
reghdfe log_maizeshare  l_FAW l_rainfall s_lrainfall temperature sqtemp, absorb( i.HHID) cl(camp2)

quietly regress log_maizeshare  l_FAW l_rainfall s_lrainfall temperature sqtemp
keep if e(sample)
// get means
foreach v in    l_FAW l_rainfall s_lrainfall temperature sqtemp  {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx

***Spray***
reghdfe spray l_FAW  l_rainfall s_lrainfall temperature sqtemp  , absorb( i.HHID) 
quietly regress spray l_FAW  l_rainfall s_lrainfall temperature sqtemp
keep if e(sample)
// get means
foreach v in  spray l_FAW  l_rainfall s_lrainfall temperature sqtemp  {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx
estimates store spray

quietly regress grewothercrop l_FAW  l_rainfall s_lrainfall temperature sqtemp
keep if e(sample)
// get means
foreach v in  grewothercrop l_FAW l_rainfall s_lrainfall temperature sqtemp  {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx
estimates store crop_div
 
 reghdfe grewothercrop l_FAW  l_rainfall s_lrainfall temperature sqtemp  , absorb(i.district ) 
estimates store crop_div2
 
quietly regress mem_left l_FAW  l_rainfall s_lrainfall temperature sqtemp
keep if e(sample)
// get means
foreach v in  mem_left l_FAW  l_rainfall s_lrainfall  temperature sqtemp  {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx 
estimates store migration

**Piecework
 gen piecework = (piecework_members>0)
reghdfe piecework l_FAW  l_rainfall s_lrainfall temperature sqtemp, absorb(i.year i.camp2) 
estimates store piecework

reghdfe lwork l_FAW  l_rainfall s_lrainfall temperature sqtemp, absorb( i.HHID)





coefplot (maizearea_share, asequation(FE) \) ///
||(spray, asequation(Lag_FAW) \ ) ///
||(crop_div, asequation(Lag_FAW) \ ) ///
||(migration, asequation(Lag_FAW) \ ) ///
||(piecework, asequation(Lag_FAW) \ ) ///
, drop(_cons) keep(l_FAW) ///
eqlabel(, labsize(small)) msymbol(circle) mcolor(ebblue) ciopts(recast(rcap) lcolor(ebblue)) xline(0,lcolor(red)) ylabel(, labsize(vsmall)) omitted baselevels /// 
bylabels( "Maize share" "Spraying" "Crop diversification" "Migration" "Off-farm work") ///
byopts(compact rows(1) note("p-values shown alongside markers" "*** p<.01, ** p<.05, * p<.1" )) ///
name (debt_fl, replace) ysize (30) xsize(50)


egen camp_maize = mean(maizearea_share), by(camp year)
gen maize_FAW = maizearea_share*l_FAW


reghdfe mem_left l_FAW  l_rainfall s_lrainfall temperature sqtemp  


*outreg2 using results2, excel append

*reghdfe camp_maize l_FAW  l_rainfall s_lrainfall , absorb( i.district) 
*outreg2 using results2, excel append

*reghdfe Charc maizearea_share maize_FAW   l_rainfall s_lrainfall , absorb( i.district) 

/*quietly regress Charc  maizearea_share maize_FAW  l_rainfall s_lrainfall
keep if e(sample)
// get means
foreach v in  maizearea_share maize_FAW  {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx*/

/**First stage regressions
reghdfe spray l_FAW  l_rainfall s_lrainfall temperature sqtemp  , absorb( i.HHID) 
quietly regress spray l_FAW  l_rainfall s_lrainfall temperature sqtemp
keep if e(sample)
// get means
foreach v in  spray l_FAW  l_rainfall s_lrainfall temperature sqtemp  {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx*/
/***Intercropping
reghdfe inter_1 l_FAW  l_rainfall s_lrainfall temperature sqtemp , absorb( i.HHID) 
quietly regress inter_1 l_FAW  l_rainfall s_lrainfall temperature sqtemp
keep if e(sample)
// get means
foreach v in  inter_1 l_FAW l_rainfall s_lrainfall temperature sqtemp  {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx*/
egen camp_inter_1 = mean(inter_1), by(camp year)
reghdfe camp_inter_1 l_FAW  l_rainfall s_lrainfall ltemperature sqtemp , absorb( i.camp2) cl(camp1)
outreg2 using results2, excel append

egen grewothercrop1 = mean(grewothercrop), by(camp year)
reghdfe grewothercrop1 l_FAW  l_rainfall s_lrainfall ltemperature sqtemp , absorb( i.camp2) cl(camp1)
outreg2 using results2, excel append

**Growing other crops
reghdfe grewothercrop l_FAW  l_rainfall s_lrainfall i.year, absorb( i.district)
outreg2 using results2, excel append 
gen grewother_FAW = grewothercrop*l_FAW
egen camp_grewothercrop = mean(grewothercrop), by(camp year)
reghdfe camp_grewothercrop l_FAW  l_rainfall s_lrainfall , absorb( i.district)

/*quietly regress grewothercrop l_FAW  l_rainfall s_lrainfall temperature sqtemp
keep if e(sample)
// get means
foreach v in  grewothercrop l_FAW l_rainfall s_lrainfall temperature sqtemp  {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx*/

*outreg2 using results2, excel append

**Piecework
drop piecework
gen piecework=0 if piecework_members==0
replace piecework=1 if piecework_members>=1
reghdfe  piecework l_FAW l_rainfall s_lrainfall ltemperature sqtemp, absorb ( i.camp2 i.year) cl(camp1)
outreg2 using results2, excel append
gen piecework_FAW=piecework*l_FAW
egen camp_piece = mean(piecework), by(camp year)


***Fresh results***
egen camp_maize = mean(maizearea_share), by(camp year)
gen maize_arm = camp_ave*camp_maize

egen camp_crop = mean(grewothercrop), by(camp year)
gen crop_army = camp_ave*camp_crop

gen piece_army = camp_ave*camp_piece

egen mig_crop = mean(mem_left), by(camp year)
gen mig_army = camp_ave*mig_crop

egen spray_crop = mean(spray), by(camp year)
gen spray_arm = camp_ave*spray_crop

***camp level availability of coping strategies***
reghdfe Charc camp_maize maize_arm camp_crop crop_army   mig_crop  mig_army camp_piece piece_army  camp_spray spray_army l_rainfall s_lrainfall temperature sqtemp i.year , absorb( i.camp_1) cl(camp1)
estimates store coping

coefplot (coping, asequation(LPM) \) ///
, drop(_cons) keep(camp_maize maize_arm camp_crop crop_army   mig_crop  mig_army camp_piece piece_army  camp_spray spray_army) ///
eqlabel(, labsize(small)) msymbol(circle) mcolor(ebblue) ciopts(recast(rcap) lcolor(ebblue)) /// 
 xline(0,lcolor(red)) ylabel(, labsize(vsmall)) omitted baselevels /// 
bylabels( "Food Group Diversification Index (FGDI)" "Household Dietary Diversity (HDDS)") ///
byopts(compact rows(1) note("p-values shown alongside markers" "*** p<.01, ** p<.05, * p<.1" ))



reghdfe Charc camp_maize maize_arm camp_crop crop_army   mig_crop  mig_army camp_piece piece_army  spray_crop spray_arm l_rainfall s_lrainfall temperature sqtemp  , absorb(i.year i.camp_1)

quietly regress piecework l_FAW  l_rainfall s_lrainfall ltemperature sqtemp
keep if e(sample)
// get means
foreach v in  piecework l_FAW l_rainfall s_lrainfall ltemperature sqtemp  {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx



reghdfe piecework l_FAW  l_rainfall s_lrainfall temperature sqtemp, absorb(i.district ) 


***Livestock
gen livestock=log(animalind)
reghdfe livestock l_FAW  l_rainfall s_lrainfall, absorb( i.HHID) 

**Migration
reghdfe mem_left l_FAW  l_rainfall s_lrainfall temperature sqtemp  
outreg2 using results2, excel append
gen mig_FAW=mem_left*l_FAW



/*quietly regress mem_left l_FAW  l_rainfall s_lrainfall
keep if e(sample)
// get means
foreach v in  mem_left l_FAW l_rainfall s_lrainfall  {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx*/

egen camp_mig = mean(mem_left), by(camp year)
*gen camp_mig = num_in_camp - 1
reghdfe camp_mig l_FAW  l_rainfall s_lrainfall  , absorb( i.district) 
**outreg2 using results2, excel append

reghdfe Charc maizearea_share maize_FAW grewothercrop grewother_FAW mem_left mig_FAW piecework piecework_FAW l_spray  spray_FAW l_rainfall s_lrainfall temperature sqtemp 




reghdfe Qcharc maizearea_share maize_FAW grewothercrop grewother_FAW mem_left mig_FAW piecework piecework_FAW l_spray  spray_FAW l_rainfall s_lrainfall ltemperature sqtemp, absorb(i.year i.district)

reghdfe Qcharc maizearea_share maize_FAW grewothercrop grewother_FAW mem_left mig_FAW piecework piecework_FAW l_spray  spray_FAW l_rainfall s_lrainfall ltemperature sqtemp, absorb(i.district i.camp2)

reghdfe Qcharc maizearea_share maize_FAW grewothercrop grewother_FAW mem_left mig_FAW piecework piecework_FAW l_spray  spray_FAW l_rainfall s_lrainfall ltemperature sqtemp, absorb(i.district ) 


reghdfe Qcharc maizearea_share maize_FAW grewothercrop grewother_FAW mem_left mig_FAW piecework piecework_FAW l_spray  spray_FAW l_rainfall s_lrainfall temperature sqtemp, absorb(  i.camp2)
outreg2 using results.tex, replace


reghdfe Charc maizearea_share maize_FAW grewothercrop grewother_FAW mem_left mig_FAW piecework piecework_FAW l_spray  spray_FAW l_rainfall s_lrainfall temperature sqtemp, absorb( i.district )
outreg2 using results.tex, replace  


**** How do households that located further away ****
gen increased = (firewood_av10==1)
gen constant = (firewood_av10==2)
gen decreased = (firewood_av10==3)

***Interactions***
gen increasedFAW = increased*l_FAW
gen constantFAW = constant*l_FAW
gen decreasedFAW = decreased*l_FAW

gen increasedarmy = increased*army_aff
gen constantarmy = constant*army_aff
gen decreasedarmy = decreased*army_aff

 
reghdfe Charc l_FAW increased increasedFAW constant constantFAW decreased decreasedFAW,absorb(i.camp2 i.year) 

reghdfe Charc l_FAW increased increasedFAW constant constantFAW decreased decreasedFAW l_rainfall s_lrainfall ltemperature sqtemp,absorb(i.camp2 i.year) 

reghdfe Charc l_FAW increased increasedFAW constant constantFAW decreased decreasedFAW l_rainfall s_lrainfall ltemperature sqtemp,absorb( i.year) 

reghdfe Charc l_FAW increased increasedFAW constant constantFAW decreased decreasedFAW l_rainfall s_lrainfall ltemperature sqtemp,absorb( i.year)
outreg2 using results.tex, replace  
 

reghdfe Charc l_FAW increased increasedarmy constant constantarmy decreased decreasedarmy l_rainfall s_lrainfall ltemperature sqtemp,absorb(i.camp2 i.year) 
outreg2 using results.tex, replace  


reghdfe Charc l_FAW increasedarmy increased  constant constantarmy decreasedarmy decreased  l_rainfall s_lrainfall ltemperature sqtemp,absorb(i.district i.year) 


reghdfe Charc i.firewood_av10

reghdfe hbsfaw  dist_firew , absorb(i.camp2 )
outreg2 using results.tex, replace
estimates store FAW_Dist

reghdfe Charc l_FAW firewood_FAW dist_firew   i.year, absorb(i.camp2)
estimates store Dist

reghdfe Charc l_FAW   decreased increased constantarmy constant   decreasedarmy increasedarmy  s_lrainfall temperature 
estimates store aware

reghdfe Charc l_FAW   firewood_av10 army_aff, absorb(i.year)
estimates store author



coefplot (FAW_Dist, asequation(FAW_Dist) \ ) ///
|| (Dist, asequation(ist) \ ) ///
||  (aware, asequation(forest) \ ) ///
|| (author, asequation(auth)\ ) ///
, drop(_cons) keep(l_FAW  hbsdist firewood_FAW dist_firew increased increasedarmy  constant constantarmy decreased decreasedarmy firewood_av10 army_aff) ///
eqlabel(, labsize(small)) msymbol(circle) mcolor(ebblue) ciopts(recast(rcap) lcolor(ebblue)) ///
 xline(0,lcolor(red)) ylabel(, labsize(vsmall)) omitted baselevels ///  
bylabels( "FAW" "Charcoal (1 = Yes)" "Charcoal (1 = Yes)" "Charcoal (1 = Yes)") ///
byopts(compact rows(1) note("p-values shown alongside markers" "*** p<.01, ** p<.05, * p<.1" ))

reghdfe Charc l_FAW increased increasedFAW constant constantFAW decreased decreasedFAW l_rainfall s_lrainfall ltemperature sqtemp,absorb(i.year i.camp2)

reghdfe Charc l_FAW firewood_FAW dist_firew  i.year, absorb(camp2)

reghdfe Charc l_FAW increased increasedarmy constant constantarmy decreased decreasedarmy l_rainfall s_lrainfall ltemperature sqtemp,absorb(i.district)




/*reghdfe lwork l_FAW  l_rainfall s_lrainfall , absorb( i.district) 
outreg2 using results2, excel replace

reghdfe Charc inter_1 maizearea_share grewothercrop animalind lnhhexp_yr  l_rainfall s_lrainfall  , absorb( i.HHID) 

reghdfe Charc camp_spray camp_inter_1 camp_maize  camp_mig   l_rainfall s_lrainfall i.year , absorb( i.district) 
outreg2 using results2, excel replace

reghdfe Charc camp_spray   camp_grewothercrop camp_mig   l_rainfall s_lrainfall i.year , absorb( i.district) 
outreg2 using results2, excel replace*/



reghdfe Charc camp_spray  camp_grewothercrop   camp_maize  camp_piece camp_mig l_rainfall s_lrainfall i.year , absorb( i.district) 
outreg2 using results2, excel replace


***coping mechanism on charcoal
gen maize_army2=camp_maize*army_aff
gen inter_army2=camp_inter_1*army_aff
gen grew_army=grewothercrop*army_aff
gen migr_army2=mem_left*army_aff
gen pie_army2= piecework*army_aff
gen spray_army2=spray_FAW*army_aff

reghdfe Charc maizearea_share maize_army  grewothercrop1  grew_army2 mem_left  migr_army2 piecework l_spray  camp_spray spray_army l_rainfall s_lrainfall  , absorb( i.district)

***coping mechanism on charcoal
gen maize_army=maizearea_share*army_aff
gen inter_army=inter_1*l_FAW
gen grew3_army=grewothercrop*l_FAW
gen migr_army=mem_left*l_FAW
gen pie_army= piecework*l_FAW
gen spray_army=l_spray*l_FAW
gen  campie_army=camp_piece*l_FAW


reghdfe Charc maize_army camp_maize grew_army camp_grewothercrop   camp_mig  migr_army piecework pie_army  camp_spray spray_army l_rainfall s_lrainfall temperature sqtemp i.year , absorb( i.HHID i.camp2) 

estimates store final

outreg2 using results2, excel replace

coefplot (final, asequation(LPM) \) ///
, drop(_cons) keep(maize_army camp_maize grew_army camp_grewothercrop   camp_mig  migr_army piecework pie_army  camp_spray spray_army) ///
eqlabel(, labsize(small)) msymbol(circle) mcolor(ebblue) ciopts(recast(rcap) lcolor(ebblue)) /// 
 xline(0,lcolor(red)) ylabel(, labsize(vsmall)) omitted baselevels /// 
bylabels( "Food Group Diversification Index (FGDI)" "Household Dietary Diversity (HDDS)") ///
byopts(compact rows(1) note("p-values shown alongside markers" "*** p<.01, ** p<.05, * p<.1" ))

/*quietly regress Charc camp_maize maize_army  camp_grewothercrop  grew_army camp_mig  migr_army piecework pie_army  l_spray spray_FAW  l_rainfall s_lrainfall  l_rainfall s_lrainfall temperature sqtemp
keep if e(sample)
// get means
foreach v in  camp_maize maize_army  camp_grewothercrop  grew_army camp_mig  migr_army piecework pie_army  l_spray spray_FAW l_rainfall s_lrainfall ltemperature sqtemp  {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx*/

gen inv_maize_army=(maize_army)
gen in_camp_maize=(camp_maize)

quietly regress Charc in_camp_maize inv_maize_army  camp_grewothercrop  grew_army camp_mig  migr_army  camp_piece campie_army pie_army l_spray  spray_FAW  l_rainfall s_lrainfall   temperature sqtemp
keep if e(sample)
// get means
foreach v in  in_camp_maize inv_maize_army  camp_grewothercrop  grew_army camp_mig  migr_army camp_piece campie_army  camp_spray spray_FAW l_rainfall s_lrainfall temperature sqtemp  {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx
outreg2 using results.tex, replace



/*quietly regress Charc camp_maize maize_army  camp_grewothercrop  grew_army camp_mig  migr_army piecework pie_army  l_spray spray_FAW  l_rainfall s_lrainfall  l_rainfall s_lrainfall temperature sqtemp
keep if e(sample)
// get means
foreach v in  camp_maize maize_army  camp_grewothercrop  grew_army camp_mig  migr_army piecework pie_army  l_spray spray_FAW l_rainfall s_lrainfall ltemperature sqtemp  {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx*/


reghdfe Charc maizearea_share maize_army  , absorb( i.district)
outreg2 using results2, excel replace
reghdfe Charc inter_1 inter_army i.year , absorb( i.district)
outreg2 using results2, excel append
reghdfe Charc grewothercrop grew3_army i.year , absorb( i.district)
outreg2 using results2, excel append
reghdfe Charc mem_left migr_army i.year , absorb( i.district)
outreg2 using results2, excel append
reghdfe Charc piecework pie_army  i.year , absorb( i.district)
outreg2 using results2, excel append
reghdfe Charc spray spray_army i.year , absorb( i.district)
outreg2 using results2, excel append

reghdfe Charc inter_1 inter_army  grewothercrop grew3_army   mem_left  migr_army piecework pie_army spray spray_army l_rainfall s_lrainfall  i.year , absorb( i.district)
outreg2 using results2, excel replace

***IV approach**
ivreghdfe Charc ( maizearea_share maize_FAW =  camp_maize maize_army2 ) l_rainfall s_lrainfall, absorb (i.district) first

ivreghdfe Charc ( grewothercrop grew3_army =  camp_grewothercrop grew_army ) l_rainfall s_lrainfall, absorb (i.district) first

ivreghdfe Charc ( mem_left  migr_army2 =  piecework pie_army ) l_rainfall s_lrainfall, absorb (i.district) first

ivreghdfe Charc ( l_spray spray_FAW =  camp_spray spray_army) l_rainfall s_lrainfall, absorb (i.district) first



***Charcoal and deforestation 
ivreghdfe Deforestion  (Charc=l_FAW), absorb (i.HHID) first 
outreg2 using results, excel append

reghdfe area_km2_buffer_10 l_FAW l_rainfall s_lrainfall  temperature sqtemp  grewothercrop mem_left piecework  spray i.year , absorb (i.HHID) 
ivreghdfe area_km2_buffer_10 (Charc=l_FAW) l_rainfall s_lrainfall  temperature sqtemp  grewothercrop mem_left piecework  spray i.year , absorb (i.HHID)  
reghdfe area_km2_buffer_10 Charc l_rainfall s_lrainfall  temperature sqtemp grewothercrop mem_left piecework  spray i.year, absorb (i.HHID) 

reghdfe area_km2_buffer_10  temperature i.year, absorb (i.HHID) 
reghdfe temperature  area_km2_buffer_10 i.year, absorb (i.HHID) 




reghdfe Deforestion l_FAW l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize hh_head_sex temperature sqtemp, absorb (i.camp1)
ivreghdfe Deforestion (Charc=l_FAW)  temperature   i.year  ,absorb (i.district) first

***Charcoal and yield IV***
ivreghdfe Charc  (l_FAW=lyield)  , absorb (i.HHID) first cl(camp1)
ivreghdfe Charc  (l_FAW=lyield)  l_rainfall s_lrainfall  temperature sqtemp i.year, absorb (i.HHID) first cl(camp1)
ivreghdfe Charc  (l_FAW=lyield)  l_rainfall s_lrainfall  temperature sqtemp i.year, absorb (i.HHID) first cl(camp1)
ivreghdfe Charc  (l_FAW=lyield) cultivown_land hhsize fert qseed l_rainfall s_lrainfall  temperature sqtemp i.year, absorb (i.HHID) first cl(camp1)





/*quietly regress Charc inter_1 inter_army  grewothercrop grew3_army   mem_left  migr_army piecework pie_army spray spray_army l_rainfall s_lrainfall
keep if e(sample)
// get means
foreach v in inter_1 inter_army  grewothercrop grew3_army   mem_left  migr_army piecework pie_army spray spray_army l_rainfall s_lrainfall {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx*/
// decalre panel

/*xtprobit Charc inter_1 inter_army  grewothercrop grew3_army   mem_left  migr_army piecework pie_army spray spray_army l_rainfall s_lrainfall


/*quietly regress Charc camp_maize maize_army  l_rainfall s_lrainfall
keep if e(sample)
// get means
foreach v in camp_maize maize_army  l_rainfall s_lrainfall {
    bysort HHID : egen double mean_`v' = mean(`v')
}
// decalre panel
xtset HHID year
// cre
xtprobit Charc camp_maize maize_army  l_rainfall s_lrainfall mean*,re
. mfx*/
 
reghdfe Charc camp_inter_1 inter_army  l_rainfall s_lrainfall  , absorb( i.district) 
/*quietly regress  Charc camp_inter_1 inter_army  l_rainfall s_lrainfall
keep if e(sample)
// get means
foreach v in  Charc camp_inter_1 inter_army l_rainfall s_lrainfall {
    bysort HHID : egen double mean_`v' = mean(`v')
}
// decalre panel
xtset HHID year
// cre
xtprobit Charc  Charc camp_inter_1 inter_army  l_rainfall s_lrainfall mean*,re
. mfx*/
/*reghdfe Charc camp_grewothercrop grew_army  l_rainfall s_lrainfall i.year , absorb( i.district) 
quietly regress  Charc  camp_grewothercrop grew_army  l_rainfall s_lrainfall
keep if e(sample)
// get means
foreach v in   camp_grewothercrop grew_army l_rainfall s_lrainfall {
    bysort HHID : egen double mean_`v' = mean(`v')
}
// decalre panel
xtset HHID year
// cre

/*xtprobit  Charc  camp_grewothercrop grew_army  l_rainfall s_lrainfall mean*,re
. mfx*/
reghdfe Charc  camp_mig  l_rainfall s_lrainfall  , absorb( i.district) 
quietly regress  Charc migr_army camp_mig  l_rainfall s_lrainfall
keep if e(sample)
// get means
foreach v in   migr_army camp_mig  l_rainfall s_lrainfall {
    bysort HHID : egen double mean_`v' = mean(`v')
}
// decalre panel
xtset HHID year
// cre
xtprobit  Charc  migr_army camp_mig  l_rainfall s_lrainfall mean*,re
. mfx*/












forval i=1/5 {
	bys year: tab company_`i'
}
	
forval i=1/5 {
	recode company_`i' (9 = 11)
}	

/* renaming this variable to match our existing cleaning code
forval i=1/5 {
	rename seed_co_`i' seedco_`i' 
}	

	*/
**********Generating seed maturity_`i' classifications per maize planting
forval i=1/5 {
	gen SeedVarietyName_`i'=""
}

*label list company_1

/* These are the customized labels that I imposed to the Companies, in order to standardize them across all 4 years :)
	 201 SeedCo
	 202 MRI
	 203 Pioneer
	 204 Pannar
	 205 Zamseed
	 206 Dekalb
	 207 Kamano
	 208 Klein Caroo
	 209 Local Maize
	 210 Golden Valley
	 211 Progene
	 213 Other Hybrid
	 214 Unknown Local
*/


* Kamano Seeds: they provide drought tolerant seeds, mostly in the medium maturity_`i' range
*> http://dtma.cimmyt.org/index.php/seed/companies-stocking-dt-seeds


forval i=1/5 {
	recode company_`i' (213=209) // reclassifying "Unknow local" as "Local maize"
}

forval i=1/5 {
	replace SeedVarietyName_`i'= "SC303" if company_`i'==201 & seedco_`i'==1
	replace SeedVarietyName_`i'= "SC403" if company_`i'==201 & seedco_`i'==2
	replace SeedVarietyName_`i'= "SC411" if company_`i'==201 & seedco_`i'==3
	replace SeedVarietyName_`i'= "SC506" if company_`i'==201 & seedco_`i'==4
	replace SeedVarietyName_`i'= "SC513" if company_`i'==201 & seedco_`i'==5
	replace SeedVarietyName_`i'= "SC525" if company_`i'==201 & seedco_`i'==6
	replace SeedVarietyName_`i'= "SC602" if company_`i'==201 & seedco_`i'==7
	replace SeedVarietyName_`i'= "SC608" if company_`i'==201 & seedco_`i'==8
	replace SeedVarietyName_`i'= "SC621" if company_`i'==201 & seedco_`i'==9
	replace SeedVarietyName_`i'= "SC627" if company_`i'==201 & seedco_`i'==10
	replace SeedVarietyName_`i'= "SC633" if company_`i'==201 & seedco_`i'==11
	replace SeedVarietyName_`i'= "SC637" if company_`i'==201 & seedco_`i'==12
	replace SeedVarietyName_`i'= "SC647" if company_`i'==201 & seedco_`i'==13
	replace SeedVarietyName_`i'= "SC727" if company_`i'==201 & seedco_`i'==17
	replace SeedVarietyName_`i'= "SCOther" if company_`i'==201 & seedco_`i'==18
* Jordan missed these ones
	replace SeedVarietyName_`i'= "SC701" if company_`i'==201 & seedco_`i'==14
	replace SeedVarietyName_`i'= "SC709" if company_`i'==201 & seedco_`i'==15
	replace SeedVarietyName_`i'= "SC719" if company_`i'==201 & seedco_`i'==16

	replace SeedVarietyName_`i'= "MRI514" if company_`i'==202 & mri_`i'==1
	replace SeedVarietyName_`i'= "MRI594" if company_`i'==202 & mri_`i'==2
	replace SeedVarietyName_`i'= "MRI614" if company_`i'==202 & mri_`i'==3
	replace SeedVarietyName_`i'= "MRI624" if company_`i'==202 & mri_`i'==4
	replace SeedVarietyName_`i'= "SYN5944" if company_`i'==202 & mri_`i'==8
	replace SeedVarietyName_`i'= "OtherMRI" if company_`i'==202 & mri_`i'==9
* Jordan missed these ones
	replace SeedVarietyName_`i'= "MRI651" if company_`i'==202 & mri_`i'==5
	replace SeedVarietyName_`i'= "MRI724" if company_`i'==202 & mri_`i'==6
	replace SeedVarietyName_`i'= "MRI744" if company_`i'==202 & mri_`i'==7
	replace SeedVarietyName_`i'= "MRI455" if company_`i'==202 & mri_`i'==10
	replace SeedVarietyName_`i'= "MRI634" if company_`i'==202 & mri_`i'==11
	replace SeedVarietyName_`i'= "MRI644" if company_`i'==202 & mri_`i'==12
	replace SeedVarietyName_`i'= "MRI654" if company_`i'==202 & mri_`i'==13
	replace SeedVarietyName_`i'= "MRI694" if company_`i'==202 & mri_`i'==14
	replace SeedVarietyName_`i'= "MRI704" if company_`i'==202 & mri_`i'==15
	replace SeedVarietyName_`i'= "MRI714" if company_`i'==202 & mri_`i'==16
	replace SeedVarietyName_`i'= "MRI734" if company_`i'==202 & mri_`i'==17
	replace SeedVarietyName_`i'= "MRI711" if company_`i'==202 & mri_`i'==18
	replace SeedVarietyName_`i'= "OtherMRI" if company_`i'==202 & mri_`i'==19

	replace SeedVarietyName_`i'= "PBB30G19" if company_`i'==203 & pioneer_`i'==1
	replace SeedVarietyName_`i'= "PBB3253" if company_`i'==203 & pioneer_`i'==2
	replace SeedVarietyName_`i'= "PBB3812W" if company_`i'==203 & pioneer_`i'==3
	replace SeedVarietyName_`i'= "PBBOther" if company_`i'==203 & pioneer_`i'==7
* Jordan missed these ones
	replace SeedVarietyName_`i'= "P2859W" if company_`i'==203 & pioneer_`i'==4
	replace SeedVarietyName_`i'= "P3506W" if company_`i'==203 & pioneer_`i'==5
	replace SeedVarietyName_`i'= "PHB30B50" if company_`i'==203 & pioneer_`i'==6

	replace SeedVarietyName_`i'= "PAN413" if company_`i'==204 & pannar_`i'==1
	replace SeedVarietyName_`i'= "PAN53" if company_`i'==204 & pannar_`i'==2
	replace SeedVarietyName_`i'= "PAN691" if company_`i'==204 & pannar_`i'==3
	replace SeedVarietyName_`i'= "PAN4M21" if company_`i'==204 & pannar_`i'==4
	replace SeedVarietyName_`i'= "PANOther" if company_`i'==204 & pannar_`i'==9
* Jordan missed these ones
	replace SeedVarietyName_`i'= "PAN4M23" if company_`i'==204 & pannar_`i'==5
	replace SeedVarietyName_`i'= "PAN7M81" if company_`i'==204 & pannar_`i'==6
	replace SeedVarietyName_`i'= "PAN7M83" if company_`i'==204 & pannar_`i'==7
	replace SeedVarietyName_`i'= "PAN6777" if company_`i'==204 & pannar_`i'==8

	replace SeedVarietyName_`i'= "ZMS402" if company_`i'==205 & zamseed_`i'==1
	replace SeedVarietyName_`i'= "ZMS528" if company_`i'==205 & zamseed_`i'==4
	replace SeedVarietyName_`i'= "ZMS606" if company_`i'==205 & zamseed_`i'==5
	replace SeedVarietyName_`i'= "ZMS616" if company_`i'==205 & zamseed_`i'==7
	replace SeedVarietyName_`i'= "ZMS620" if company_`i'==205 & zamseed_`i'==8
	replace SeedVarietyName_`i'= "ZMS638" if company_`i'==205 & zamseed_`i'==10
	replace SeedVarietyName_`i'= "ZMS702" if company_`i'==205 & zamseed_`i'==12
	replace SeedVarietyName_`i'= "ZMSOther" if company_`i'==205 & zamseed_`i'==15
* Jordan missed these ones
	replace SeedVarietyName_`i'= "ZMS405" if company_`i'==205 & zamseed_`i'==2
	replace SeedVarietyName_`i'= "ZMS510" if company_`i'==205 & zamseed_`i'==3
	replace SeedVarietyName_`i'= "ZMS607Y" if company_`i'==205 & (zamseed_`i'==6 |  zamseed_`i'==18)
	replace SeedVarietyName_`i'= "ZMS623" if company_`i'==205 & zamseed_`i'==9
	replace SeedVarietyName_`i'= "ZMS652" if company_`i'==205 & zamseed_`i'==11
	replace SeedVarietyName_`i'= "ZMS720" if company_`i'==205 & zamseed_`i'==13
	replace SeedVarietyName_`i'= "ZMS721" if company_`i'==205 & zamseed_`i'==14
	replace SeedVarietyName_`i'= "ZMS502" if company_`i'==205 & zamseed_`i'==16
	replace SeedVarietyName_`i'= "ZMS717" if company_`i'==205 & zamseed_`i'==17

	replace SeedVarietyName_`i'= "DK8021" if company_`i'==206 & dekalb_`i'==1
	replace SeedVarietyName_`i'= "DK8033" if company_`i'==206 & dekalb_`i'==2
	replace SeedVarietyName_`i'= "DK9053" if company_`i'==206 & dekalb_`i'==4
	replace SeedVarietyName_`i'= "DK9089" if company_`i'==206 & dekalb_`i'==5
	replace SeedVarietyName_`i'= "DKOther" if company_`i'==206 & dekalb_`i'==7
	replace SeedVarietyName_`i'= "Local" if company_`i'==209
*	replace SeedVarietyName_`i'= "Local" if company_`i'==213 // we really should clean this though- some hybrids in here
* Jordan missed these ones
	replace SeedVarietyName_`i'= "DKC8073" if company_`i'==206 & dekalb_`i'==3
	replace SeedVarietyName_`i'= "DKC777" if company_`i'==206 & dekalb_`i'==6
}


bys year: tab1 SeedVarietyName_*, sort


** The other maize varieties are referred to in "oth_variety_*". Trying to identify
tab1 oth_variety_1 oth_variety_2 oth_variety_3 oth_variety_4 oth_variety_5

forval i=1/5 {
	gen temp_oth_variety_`i' = oth_variety_`i'
	replace temp_oth_variety_`i' = strtrim(oth_variety_`i') // removes leading and trailing blanks
	replace temp_oth_variety_`i' = stritrim(oth_variety_`i') // removes internal blanks
	replace temp_oth_variety_`i' = upper(oth_variety_`i') // makes this uppercase
}

* levelsof SeedVarietyName_`i', local(mylevels) // storing all seed codes in this local
/*
local mylevels "DK8021" "DK8033" "DK9053" "DK9089" "DKC777" "DKC8073" "DKOther" ///
		"MRI455" "MRI514" "MRI594" "MRI614" "MRI624" "MRI634" "MRI644" "MRI651" ///
		"MRI654" "MRI704" "MRI711" "MRI714" "MRI724" "MRI744" "OtherMRI" "P2859W" ///
		"P3506W" "PAN413" "PAN4M21" "PAN4M23" "PAN53" "PAN6777" "PAN691" "PAN7M81" ///
		"PAN7M83" "PANOther" "PBB30G19" "PBB3253" "PBB3812W" "PBBOther" "PHB30B50" ///
		"SC303" "SC403" "SC411" "SC506" "SC513" "SC525" "SC602" "SC608" "SC621" ///
		"SC627" "SC633" "SC637" "SC647" "SC701" "SC709" "SC719" "SC727" "SCOther" ///
		"SYN5944" "ZMS402" "ZMS405" "ZMS502" "ZMS510" "ZMS528" "ZMS606" "ZMS607Y" ///
		"ZMS616" "ZMS620" "ZMS623" "ZMS638" "ZMS652" "ZMS702" "ZMS720" "ZMS721" "ZMSOther"

local mylevels "DK8021" "DK8033" "DK9053" "DK9089" "DKC777" "DKC8073" "DKOther" "MRI455" "MRI514" "MRI594" "MRI614" "MRI624" "MRI634" "MRI644" "MRI651" "MRI654" "MRI704" "MRI711" "MRI714" "MRI724" "MRI744" "OtherMRI" "P2859W" "P3506W" "PAN413" "PAN4M21" "PAN4M23" "PAN53" "PAN6777" "PAN691" "PAN7M81" "PAN7M83" "PANOther" "PBB30G19" "PBB3253" "PBB3812W" "PBBOther" "PHB30B50" "SC303" "SC403" "SC411" "SC506" "SC513" "SC525" "SC602" "SC608" "SC621" "SC627" "SC633" "SC637" "SC647" "SC701" "SC709" "SC719" "SC727" "SCOther" "SYN5944" "ZMS402" "ZMS405" "ZMS502" "ZMS510" "ZMS528" "ZMS606" "ZMS607Y" "ZMS616" "ZMS620" "ZMS623" "ZMS638" "ZMS652" "ZMS702" "ZMS720" "ZMS721" "ZMSOther"

forval i=1/5 {
	foreach v of local mylevels {
		replace SeedVarietyName_`i'= `v' if strpos(temp_oth_variety_`i', `v')>0
	}
}



forval i=1/5 {
	foreach v in "DK8021" "DK8033" "DK9053" "DK9089" "DKC777" "DKC8073" "DKOther" ///
		"MRI455" "MRI514" "MRI594" "MRI614" "MRI624" "MRI634" "MRI644" "MRI651" ///
		"MRI654" "MRI704" "MRI711" "MRI714" "MRI724" "MRI744" "OtherMRI" "P2859W" ///
		"P3506W" "PAN413" "PAN4M21" "PAN4M23" "PAN53" "PAN6777" "PAN691" "PAN7M81" ///
		"PAN7M83" "PANOther" "PBB30G19" "PBB3253" "PBB3812W" "PBBOther" "PHB30B50" ///
		"SC303" "SC403" "SC411" "SC506" "SC513" "SC525" "SC602" "SC608" "SC621" ///
		"SC627" "SC633" "SC637" "SC647" "SC701" "SC709" "SC719" "SC727" "SCOther" ///
		"SYN5944" "ZMS402" "ZMS405" "ZMS502" "ZMS510" "ZMS528" "ZMS606" "ZMS607Y" ///
		"ZMS616" "ZMS620" "ZMS623" "ZMS638" "ZMS652" "ZMS702" "ZMS720" "ZMS721" "ZMSOther" {
	replace SeedVarietyName_`i'= `v' if strpos(temp_oth_variety_`i', `v')>0
	}
}
*/

	
	drop temp_oth_variety_*


forval i=1/5 {
	replace SeedVarietyName_`i'= "SC303" if strpos(oth_variety_`i', "SC303")>0 | strpos(oth_variety_`i', "SC 303")>0
	replace SeedVarietyName_`i'= "SC403" if strpos(oth_variety_`i', "SC403")>0 | strpos(oth_variety_`i', "SC 403")>0
	replace SeedVarietyName_`i'= "SC411" if strpos(oth_variety_`i', "SC411")>0 | strpos(oth_variety_`i', "SC 411")>0
	replace SeedVarietyName_`i'= "SC506" if strpos(oth_variety_`i', "SC506")>0 | strpos(oth_variety_`i', "SC 506")>0
	replace SeedVarietyName_`i'= "SC513" if strpos(oth_variety_`i', "SC513")>0 | strpos(oth_variety_`i', "SC 513")>0
	replace SeedVarietyName_`i'= "SC525" if strpos(oth_variety_`i', "SC525")>0 | strpos(oth_variety_`i', "SC 525")>0
	replace SeedVarietyName_`i'= "SC602" if strpos(oth_variety_`i', "SC602")>0 | strpos(oth_variety_`i', "SC 602")>0
	replace SeedVarietyName_`i'= "SC608" if strpos(oth_variety_`i', "SC608")>0 | strpos(oth_variety_`i', "SC 608")>0
	replace SeedVarietyName_`i'= "SC621" if strpos(oth_variety_`i', "SC621")>0 | strpos(oth_variety_`i', "SC 621")>0
	replace SeedVarietyName_`i'= "SC627" if strpos(oth_variety_`i', "SC627")>0 | strpos(oth_variety_`i', "SC 627")>0
	replace SeedVarietyName_`i'= "SC633" if strpos(oth_variety_`i', "SC633")>0 | strpos(oth_variety_`i', "SC 633")>0
	replace SeedVarietyName_`i'= "SC637" if strpos(oth_variety_`i', "SC637")>0 | strpos(oth_variety_`i', "SC 637")>0
	replace SeedVarietyName_`i'= "SC647" if strpos(oth_variety_`i', "SC647")>0 | strpos(oth_variety_`i', "SC 647")>0
	replace SeedVarietyName_`i'= "SC727" if strpos(oth_variety_`i', "SC727")>0 | strpos(oth_variety_`i', "SC 727")>0
* Jordan missed these ones
	replace SeedVarietyName_`i'= "SC701" if strpos(oth_variety_`i', "SC701")>0 | strpos(oth_variety_`i', "SC 701")>0
	replace SeedVarietyName_`i'= "SC709" if strpos(oth_variety_`i', "SC709")>0 | strpos(oth_variety_`i', "SC 709")>0
	replace SeedVarietyName_`i'= "SC719" if strpos(oth_variety_`i', "SC719")>0 | strpos(oth_variety_`i', "SC 719")>0

	replace SeedVarietyName_`i'= "MRI514" if strpos(oth_variety_`i', "MRI514")>0 | strpos(oth_variety_`i', "MRI 514")>0
	replace SeedVarietyName_`i'= "MRI594" if strpos(oth_variety_`i', "MRI594")>0 | strpos(oth_variety_`i', "MRI 594")>0
	replace SeedVarietyName_`i'= "MRI614" if strpos(oth_variety_`i', "MRI614")>0 | strpos(oth_variety_`i', "MRI 614")>0
	replace SeedVarietyName_`i'= "MRI624" if strpos(oth_variety_`i', "MRI624")>0 | strpos(oth_variety_`i', "MRI 624")>0
	replace SeedVarietyName_`i'= "SYN5944" if strpos(oth_variety_`i', "SYN5944")>0 | strpos(oth_variety_`i', "SYN 5944")>0
* Jordan missed these ones
	replace SeedVarietyName_`i'= "MRI651" if strpos(oth_variety_`i', "MRI651")>0 | strpos(oth_variety_`i', "MRI 651")>0
	replace SeedVarietyName_`i'= "MRI724" if strpos(oth_variety_`i', "MRI724")>0 | strpos(oth_variety_`i', "MRI 724")>0
	replace SeedVarietyName_`i'= "MRI744" if strpos(oth_variety_`i', "MRI744")>0 | strpos(oth_variety_`i', "MRI 744")>0
	replace SeedVarietyName_`i'= "MRI455" if strpos(oth_variety_`i', "MRI455")>0 | strpos(oth_variety_`i', "MRI 455")>0
	replace SeedVarietyName_`i'= "MRI634" if strpos(oth_variety_`i', "MRI634")>0 | strpos(oth_variety_`i', "MRI 634")>0
	replace SeedVarietyName_`i'= "MRI644" if strpos(oth_variety_`i', "MRI644")>0 | strpos(oth_variety_`i', "MRI 644")>0
	replace SeedVarietyName_`i'= "MRI654" if strpos(oth_variety_`i', "MRI654")>0 | strpos(oth_variety_`i', "MRI 654")>0
	replace SeedVarietyName_`i'= "MRI694" if strpos(oth_variety_`i', "MRI694")>0 | strpos(oth_variety_`i', "MRI 694")>0
	replace SeedVarietyName_`i'= "MRI704" if strpos(oth_variety_`i', "MRI704")>0 | strpos(oth_variety_`i', "MRI 704")>0
	replace SeedVarietyName_`i'= "MRI714" if strpos(oth_variety_`i', "MRI714")>0 | strpos(oth_variety_`i', "MRI 714")>0
	replace SeedVarietyName_`i'= "MRI734" if strpos(oth_variety_`i', "MRI734")>0 | strpos(oth_variety_`i', "MRI 734")>0
	replace SeedVarietyName_`i'= "MRI711" if strpos(oth_variety_`i', "MRI711")>0 | strpos(oth_variety_`i', "MRI 711")>0

	replace SeedVarietyName_`i'= "PBB30G19" if strpos(oth_variety_`i', "PBB30G19")>0 | strpos(oth_variety_`i', "PBB 30G19")>0 | strpos(oth_variety_`i', "PBB 30 G19")>0
	replace SeedVarietyName_`i'= "PBB3253" if strpos(oth_variety_`i', "PBB3253")>0 | strpos(oth_variety_`i', "PBB 3253")>0
	replace SeedVarietyName_`i'= "PBB3812W" if strpos(oth_variety_`i', "PBB3812W")>0 | strpos(oth_variety_`i', "PBB 3812W")>0 | strpos(oth_variety_`i', "PBB 3812 W")>0
* Jordan missed these ones
	replace SeedVarietyName_`i'= "P2859W" if strpos(oth_variety_`i', "P2859W")>0 | strpos(oth_variety_`i', "P 2859W")>0 | strpos(oth_variety_`i', "P 2859 W")>0
	replace SeedVarietyName_`i'= "P3506W" if strpos(oth_variety_`i', "P3506W")>0 | strpos(oth_variety_`i', "P 3506W")>0 | strpos(oth_variety_`i', "P 3506 W")>0
	replace SeedVarietyName_`i'= "PHB30B50" if strpos(oth_variety_`i', "PHB30B50")>0 | strpos(oth_variety_`i', "PHB 30B50")>0 | strpos(oth_variety_`i', "PHB 30 B50")>0 | strpos(oth_variety_`i', "PHB 30B50")>0 | strpos(oth_variety_`i', "PHB 30 B 50")>0

	replace SeedVarietyName_`i'= "PAN413" if strpos(oth_variety_`i', "PAN413")>0 | strpos(oth_variety_`i', "PAN 413")>0
	replace SeedVarietyName_`i'= "PAN53" if strpos(oth_variety_`i', "PAN53")>0 | strpos(oth_variety_`i', "PAN 53")>0
	replace SeedVarietyName_`i'= "PAN691" if strpos(oth_variety_`i', "PAN691")>0 | strpos(oth_variety_`i', "PAN 691")>0
	replace SeedVarietyName_`i'= "PAN4M21" if strpos(oth_variety_`i', "PAN4M21")>0 | strpos(oth_variety_`i', "PAN 4M21")>0 | strpos(oth_variety_`i', "PAN 4M 21")>0
* Jordan missed these ones
	replace SeedVarietyName_`i'= "PAN4M23" if strpos(oth_variety_`i', "PAN4M23")>0 | strpos(oth_variety_`i', "PAN 4M23")>0 | strpos(oth_variety_`i', "PAN 4M 23")>0
	replace SeedVarietyName_`i'= "PAN7M81" if strpos(oth_variety_`i', "PAN7M81")>0 | strpos(oth_variety_`i', "PAN 7M81")>0 | strpos(oth_variety_`i', "PAN 7M 81")>0
	replace SeedVarietyName_`i'= "PAN7M83" if strpos(oth_variety_`i', "PAN7M83")>0 | strpos(oth_variety_`i', "PAN 7M83")>0 | strpos(oth_variety_`i', "PAN 7M 83")>0
	replace SeedVarietyName_`i'= "PAN6777" if strpos(oth_variety_`i', "PAN6777")>0 | strpos(oth_variety_`i', "PAN 6777")>0 | strpos(oth_variety_`i', "PAN 6777")>0

	replace SeedVarietyName_`i'= "ZMS402" if strpos(oth_variety_`i', "ZMS402")>0 | strpos(oth_variety_`i', "ZMS 402")>0
	replace SeedVarietyName_`i'= "ZMS528" if strpos(oth_variety_`i', "ZMS 528")>0 | strpos(oth_variety_`i', "ZMS 528")>0
	replace SeedVarietyName_`i'= "ZMS606" if strpos(oth_variety_`i', "ZMS606")>0 | strpos(oth_variety_`i', "ZMS 606")>0
	replace SeedVarietyName_`i'= "ZMS616" if strpos(oth_variety_`i', "ZMS616")>0 | strpos(oth_variety_`i', "ZMS 616")>0
	replace SeedVarietyName_`i'= "ZMS620" if strpos(oth_variety_`i', "ZMS620")>0 | strpos(oth_variety_`i', "ZMS 620")>0
	replace SeedVarietyName_`i'= "ZMS638" if strpos(oth_variety_`i', "ZMS638")>0 | strpos(oth_variety_`i', "ZMS 638")>0
	replace SeedVarietyName_`i'= "ZMS702" if strpos(oth_variety_`i', "ZMS702")>0 | strpos(oth_variety_`i', "ZMS 702")>0
* Jordan missed these ones
	replace SeedVarietyName_`i'= "ZMS405" if strpos(oth_variety_`i', "ZMS405")>0 | strpos(oth_variety_`i', "ZMS 405")>0
	replace SeedVarietyName_`i'= "ZMS510" if strpos(oth_variety_`i', "ZMS510")>0 | strpos(oth_variety_`i', "ZMS 510")>0
	replace SeedVarietyName_`i'= "ZMS607Y" if strpos(oth_variety_`i', "ZMS607Y")>0 | strpos(oth_variety_`i', "ZMS 607Y")>0 | strpos(oth_variety_`i', "ZMS 607 Y")>0
	replace SeedVarietyName_`i'= "ZMS623" if strpos(oth_variety_`i', "ZMS623")>0 | strpos(oth_variety_`i', "ZMS 623")>0
	replace SeedVarietyName_`i'= "ZMS652" if strpos(oth_variety_`i', "ZMS652")>0 | strpos(oth_variety_`i', "ZMS 652")>0
	replace SeedVarietyName_`i'= "ZMS720" if strpos(oth_variety_`i', "ZMS720")>0 | strpos(oth_variety_`i', "ZMS 720")>0
	replace SeedVarietyName_`i'= "ZMS721" if strpos(oth_variety_`i', "ZMS721")>0 | strpos(oth_variety_`i', "ZMS 721")>0
	replace SeedVarietyName_`i'= "ZMS502" if strpos(oth_variety_`i', "ZMS502")>0 | strpos(oth_variety_`i', "ZMS 502")>0
	replace SeedVarietyName_`i'= "ZMS717" if strpos(oth_variety_`i', "ZMS717")>0 | strpos(oth_variety_`i', "ZMS 717")>0

	replace SeedVarietyName_`i'= "DK8021" if strpos(oth_variety_`i', "DK8021")>0 | strpos(oth_variety_`i', "DK 8021")>0
	replace SeedVarietyName_`i'= "DK8033" if strpos(oth_variety_`i', "DK8033")>0 | strpos(oth_variety_`i', "DK 8033")>0
	replace SeedVarietyName_`i'= "DK9053" if strpos(oth_variety_`i', "DK9053")>0 | strpos(oth_variety_`i', "DK 9053")>0
	replace SeedVarietyName_`i'= "DK9089" if strpos(oth_variety_`i', "DK9089")>0 | strpos(oth_variety_`i', "DK 9089")>0
* Jordan missed these ones
	replace SeedVarietyName_`i'= "DKC8073" if strpos(oth_variety_`i', "DKC8073")>0 | strpos(oth_variety_`i', "DKC 8073")>0
	replace SeedVarietyName_`i'= "DKC777" if strpos(oth_variety_`i', "DKC777")>0 | strpos(oth_variety_`i', "DKC 777")>0
}


* See seed variety classification: https://static1.squarespace.com/static/54e39dcfe4b033c7e0e77c20/t/59da6ed7d7bdcef13181c3b6/1507487449769/Waldman_SeedChoice_2017.pdf
forval i=1/5 {
	gen maturity_`i' ="" // Cretaes "maturity" variable
	
	
//very early (0)
	replace maturity_`i' ="0" if SeedVarietyName_`i'== "SC403"
	replace maturity_`i' ="0" if SeedVarietyName_`i'== "SC411" 
	replace maturity_`i' ="0" if SeedVarietyName_`i'== "PAN413" 
	replace maturity_`i' ="0" if SeedVarietyName_`i'== "DK8031"
	replace maturity_`i' ="0" if SeedVarietyName_`i'=="ZMS402" 

	//early (should equal 1)
	replace maturity_`i' ="1" if SeedVarietyName_`i'== "SC506" // https://www.seedcogroup.com/products/maize
	replace maturity_`i' ="1" if SeedVarietyName_`i'== "SC513" // https://www.seedcogroup.com/products/maize
	replace maturity_`i' ="1" if SeedVarietyName_`i'== "SC525" // https://www.seedcogroup.com/zm/products/maize/early-maturing-hybrid/sc-525
	replace maturity_`i' ="1" if SeedVarietyName_`i'== "PAN53"
	replace maturity_`i' ="1" if SeedVarietyName_`i'== "DK8033" 
	replace maturity_`i' ="1" if SeedVarietyName_`i'== "DK9089"  //early to medium
	replace maturity_`i' ="1" if SeedVarietyName_`i'== "ZMS405" // early: https://www.zamseed.co.zm/index.php/agricultural-seed/maize-seed-
	//unconfirmed early
	replace maturity_`i' ="1" if SeedVarietyName_`i'=="ZMS520" 
	replace maturity_`i' ="1" if SeedVarietyName_`i'=="ZMS528" 
	replace maturity_`i' ="1" if SeedVarietyName_`i'=="PAN4M19"
	replace maturity_`i' ="1" if SeedVarietyName_`i'=="PBB2859W"

	//medium (should equal 2)
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "MRI624"
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "MRI594"
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "SC602" // https://www.seedcogroup.com/zm/products/maize/medium-maturing-hybrids
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "SC608" // https://www.seedcogroup.com/zm/products/maize/medium-maturing-hybrids
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "SC621" // https://www.seedcogroup.com/zm/products/maize/medium-maturing-hybrids
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "SC627" // https://www.seedcogroup.com/zm/products/maize/medium-maturing-hybrids
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "SC633" // https://www.seedcogroup.com/zm/products/maize/medium-maturing-hybrids
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "SC637" // https://www.seedcogroup.com/zm/products/maize/medium-maturing-hybrids
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "SC647" // https://www.seedcogroup.com/zm/products/maize/medium-maturing-hybrids
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "PBB30G19-6"

	//unconfirmed medium
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "ZMS602"
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "ZMS606"
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "ZMS608"
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "ZMS616"
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "ZMS620"
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "ZMS638"

	replace maturity_`i' ="2" if SeedVarietyName_`i'== "P2859W" // based on Waldman et al.'s paper
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "P3506W" // http://cereals.co.ke/pdf/July-Sept-2019.pdf

	* MRI Seed Company’s promoted varieties mature just a few days short of the ///
		*growing period of 150 days for Southern Zambia. See: https://iicbe.org/upload/9829C414017.pdf
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "MRI634" // https://iicbe.org/upload/9829C414017.pdf
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "MRI651"
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "MRI724"
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "MRI744"
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "MRI455"
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "MRI644" // https://iicbe.org/upload/9829C414017.pdf
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "MRI654"
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "MRI694" // https://iicbe.org/upload/9829C414017.pdf
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "MRI704"
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "MRI714"
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "MRI711"
	replace maturity_`i' ="" if SeedVarietyName_`i'== "OtherMRI"

	replace maturity_`i' ="2" if SeedVarietyName_`i'== "PHB30B50"
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "PAN4M23"
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "PAN7M81"
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "PAN7M83"
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "ZMS510" // Medium: https://www.zamseed.co.zm/index.php/agricultural-seed/maize-seed-
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "ZMS623" // medium: https://www.zamseed.co.zm/index.php/agricultural-seed/maize-seed-
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "ZMS652" // medium: https://www.zamseed.co.zm/index.php/agricultural-seed/maize-seed-
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "ZMS502" // medium: https://griffinzambia.com/product/zamseed-maize-seed-502-5kg/
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "ZMS607Y" // medium: https://static1.squarespace.com/static/54e39dcfe4b033c7e0e77c20/t/59da6ed7d7bdcef13181c3b6/1507487449769/Waldman_SeedChoice_2017.pdf
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "ZMS721" // medium: https://www.zamseed.co.zm/index.php/agricultural-seed/maize-seed-/variety-description
	replace maturity_`i' ="2" if SeedVarietyName_`i'== "DKC777" // medium: http://ratin.net/site/news_article/4139

		
	//Late
	replace maturity_`i' ="3" if SeedVarietyName_`i'== "SC701" // https://www.seedcogroup.com/zm/products/maize/late-maturing-hybrid/sc-701
	replace maturity_`i' ="3" if SeedVarietyName_`i'== "SC709" // https://www.seedcogroup.com/zm/products/maize/late-maturing-hybrid/sc-709
	replace maturity_`i' ="3" if SeedVarietyName_`i'== "SC719" // https://www.seedcogroup.com/zm/products/maize/late-maturing-hybrid/sc-719
	replace maturity_`i' ="3" if SeedVarietyName_`i'== "SC727" // https://www.seedcogroup.com/zm/products/maize/late-maturing-hybrid/sc-727
	replace maturity_`i' ="3" if SeedVarietyName_`i'== "ZMS702"
	replace maturity_`i' ="3" if SeedVarietyName_`i'== "PAN6777" // http://www.pannar.com/assets/countries/pannar_sa_PRODUCTCAT_eng.pdf
	replace maturity_`i' ="3" if SeedVarietyName_`i'== "ZMS720" // late: https://www.zamseed.co.zm/index.php/agricultural-seed/maize-seed-
	replace maturity_`i' ="3" if SeedVarietyName_`i'== "ZMS717" // late: https://www.zamseed.co.zm/index.php/agricultural-seed/maize-seed-/variety-description
	replace maturity_`i' ="3" if SeedVarietyName_`i'== "DKC8073" // late: http://www.monsantoafrica.com/_pdfs/dekalb_dkc80-73.pdf
}


//local varieites all tagged as late maturing though not sure this is true

forval i=1/5 {
	tab1 SeedVarietyName_`i' maturity_`i'

	replace maturity_`i' ="4" if SeedVarietyName_`i'== "Local"
*	replace maturity_`i' ="" if maturity_`i'=="a"  //only about 320 should remain unclassified but still more cleaning needed
	destring maturity_`i', replace

	tab maturity_`i' year
*	tab maturity_`i' year, m
}

/*
** Progene seed are intermediate maturing drought tolerant: http://dtma.cimmyt.org/index.php/seed/companies-stocking-dt-seeds
* Making all Progene, Kamano & Other Hybrid seed "medium" maturing
	forval i=1/5 {
		replace maturity_`i' = 2 if company_`i'==11 // for Progene
		replace maturity_`i' = 2 if company_`i'==7 // for Kamano
		replace maturity_`i' = 2 if company_`i'==13 // for Other Hybrid
	}
*/


forval i=1/5 {
	
	// regrouping very early and early into a single category
	recode maturity_`i' (0 = 1)
	tab maturity_`i' year

	// Making "local variety" the default option ==> "0"
	recode maturity_`i' (4 = 0)
	tab maturity_`i' year

	// labeling the maturity_`i' variable
	label var maturity_`i' "Maize seed maturity classification: planting `i'"

	label define maturity_`i' 0 "Local" 1 "Early" 2 "Medium" 3 "Late" 
	label values maturity_`i' maturity_`i'

	bys year: tab maturity_`i'
}



// creating dummy variables for each maturity_`i' group

gen localmaize = 0
label var localmaize "HH grew local maize variety"
forval i=1/5 {
	replace localmaize=1 if maturity_`i'==0
	replace localmaize=. if maturity_1==. & maturity_2==. & maturity_3==. & maturity_4==. & maturity_5==.
}

gen earlymaize = 0
label var earlymaize "HH grew early maize variety"
forval i=1/5 {
	replace earlymaize =1 if maturity_`i'==1
	replace earlymaize = . if maturity_1==. & maturity_2==. & maturity_3==. & maturity_4==. & maturity_5==.
}

gen mediummaize = 0
label var mediummaize "HH grew medium maize variety"
forval i=1/5 {
	replace mediummaize = 1 if maturity_`i'==2
	replace mediummaize = . if maturity_1==. & maturity_2==. & maturity_3==. & maturity_4==. & maturity_5==.
}

gen latemaize = 0
label var latemaize "HH grew late maize variety"
forval i=1/5 {
	replace latemaize = 1 if maturity_`i'==3
	replace latemaize = . if maturity_1==. & maturity_2==. & maturity_3==. & maturity_4==. & maturity_5==.
}

foreach var of varlist localmaize earlymaize mediummaize latemaize {
	label define `var' 0 "No" 1 "Yes", replace
	label values `var' `var'
}

bys year: sum localmaize earlymaize mediummaize latemaize




*---------------------------------------------------------
* 	Use of recycled Maize Seed
*---------------------------------------------------------

* recoding Yes/No answers to questions "8.1.5 Was this recycled seed?"
* Relevant variables: recycl_1 recycl_2 recycl_3 recycl_4 recycl_5
bys year: tab1 recycl_*

egen recycl_num = rowtotal(recycl_1 recycl_2 recycl_3 recycl_4 recycl_5), m
label var recycl_num "Number of plantings with recycled maize seed"



*---------------------------------------------------------
* 	Number of Hybrid Seed Varieties Planted
*---------------------------------------------------------
gen hybseedcount = 0

foreach v in "DK8021" "DK8033" "DK9053" "DK9089" "DKC777" "DKC8073" "DKOther" ///
	"MRI455" "MRI514" "MRI594" "MRI614" "MRI624" "MRI634" "MRI644" "MRI651" ///
	"MRI654" "MRI704" "MRI711" "MRI714" "MRI724" "MRI744" "OtherMRI" "P2859W" ///
	"P3506W" "PAN413" "PAN4M21" "PAN4M23" "PAN53" "PAN6777" "PAN691" "PAN7M81" ///
	"PAN7M83" "PANOther" "PBB30G19" "PBB3253" "PBB3812W" "PBBOther" "PHB30B50" ///
	"SC303" "SC403" "SC411" "SC506" "SC513" "SC525" "SC602" "SC608" "SC621" ///
	"SC627" "SC633" "SC637" "SC647" "SC701" "SC709" "SC719" "SC727" "SCOther" ///
	"SYN5944" "ZMS402" "ZMS405" "ZMS502" "ZMS510" "ZMS528" "ZMS606" "ZMS607Y" ///
	"ZMS616" "ZMS620" "ZMS623" "ZMS638" "ZMS652" "ZMS702" "ZMS720" "ZMS721" "ZMSOther" {
	
	forval i=1/5 {
		replace hybseedcount = hybseedcount + 1 if SeedVarietyName_`i'=="`v'"
	}
}

replace hybseedcount=. if SeedVarietyName_1=="" & SeedVarietyName_2=="" & SeedVarietyName_3=="" & SeedVarietyName_4=="" & SeedVarietyName_5=="" // if SeedVarietyName_`i' is empty

label var hybseedcount "Number of Hybrid Seed Varieties Planted"

bys year: tab hybseedcount, m
bys year: tab hybseedcount





*---------------------------------------------------------
* 	Variables Discussed with Nicky
*---------------------------------------------------------

// Generating a 0/1 dummy for (=1 if grew F1 hybrid maize)

gen grewf1maize = 0
label var grewf1maize "Grew F1 hybrid maize: 1=Yes, 0=No"

foreach v in "DK8021" "DK8033" "DK9053" "DK9089" "DKC777" "DKC8073" "DKOther" ///
	"MRI455" "MRI514" "MRI594" "MRI614" "MRI624" "MRI634" "MRI644" "MRI651" ///
	"MRI654" "MRI704" "MRI711" "MRI714" "MRI724" "MRI744" "OtherMRI" "P2859W" ///
	"P3506W" "PAN413" "PAN4M21" "PAN4M23" "PAN53" "PAN6777" "PAN691" "PAN7M81" ///
	"PAN7M83" "PANOther" "PBB30G19" "PBB3253" "PBB3812W" "PBBOther" "PHB30B50" ///
	"SC303" "SC403" "SC411" "SC506" "SC513" "SC525" "SC602" "SC608" "SC621" ///
	"SC627" "SC633" "SC637" "SC647" "SC701" "SC709" "SC719" "SC727" "SCOther" ///
	"SYN5944" "ZMS402" "ZMS405" "ZMS502" "ZMS510" "ZMS528" "ZMS606" "ZMS607Y" ///
	"ZMS616" "ZMS620" "ZMS623" "ZMS638" "ZMS652" "ZMS702" "ZMS720" "ZMS721" "ZMSOther" {

	forval i=1/5 {
		replace grewf1maize = 1 if SeedVarietyName_`i'=="`v'" & recycl_`i' == 0 // only for non-recycled F1 hybrid seeds
	}
}

replace grewf1maize=. if SeedVarietyName_1=="" & SeedVarietyName_2=="" & SeedVarietyName_3=="" & SeedVarietyName_4=="" & SeedVarietyName_5=="" // if SeedVarietyName_`i' is empty

label define grewf1maize 1 "Yes" 0 "No", replace
label values grewf1maize grewf1maize

bys year: tab grewf1maize, m

*reghdfe grewf1maize l_FAW  l_rainfall s_lrainfall, absorb( i.HHID) 

// Generating a 0/1 dummy for (Hectares of F1 hybrid maize planted)
gen f1maize_ha = 0
label var f1maize_ha "Hectares of F1 hybrid maize planted"

foreach v in "DK8021" "DK8033" "DK9053" "DK9089" "DKC777" "DKC8073" "DKOther" ///
	"MRI455" "MRI514" "MRI594" "MRI614" "MRI624" "MRI634" "MRI644" "MRI651" ///
	"MRI654" "MRI704" "MRI711" "MRI714" "MRI724" "MRI744" "OtherMRI" "P2859W" ///
	"P3506W" "PAN413" "PAN4M21" "PAN4M23" "PAN53" "PAN6777" "PAN691" "PAN7M81" ///
	"PAN7M83" "PANOther" "PBB30G19" "PBB3253" "PBB3812W" "PBBOther" "PHB30B50" ///
	"SC303" "SC403" "SC411" "SC506" "SC513" "SC525" "SC602" "SC608" "SC621" ///
	"SC627" "SC633" "SC637" "SC647" "SC701" "SC709" "SC719" "SC727" "SCOther" ///
	"SYN5944" "ZMS402" "ZMS405" "ZMS502" "ZMS510" "ZMS528" "ZMS606" "ZMS607Y" ///
	"ZMS616" "ZMS620" "ZMS623" "ZMS638" "ZMS652" "ZMS702" "ZMS720" "ZMS721" "ZMSOther" {

	forval i=1/5 { // plantings
		replace f1maize_ha = f1maize_ha + plot_`i' if SeedVarietyName_`i'=="`v'" & recycl_`i' == 0 // only for non-recycled F1 hybrid seeds
	}
}

replace f1maize_ha=. if grewf1maize==.

bys year: tab f1maize_ha, m



gen lnf1maize_ha = log(f1maize_ha)
label var lnf1maize_ha "Log of Hectares of F1 hybrid maize planted"

sum Charc if year==2016
sum Charc if year==2018
sum Charc if year==2019




***Descrpitives t-test
***Demographics
*keep if year==2016
ttest hh_head_age,by(Charc)
ttest hh_head_edu,by(Charc)
ttest hh_head_sex,by(Charc)

***Production and productivity
ttest Total,by(Charc)
ttest fmaize,by(Charc)

***Land ownership and acquisition
ttest farmsize_W01,by(Charc)
ttest cultivown_land,by(Charc)

***Wealth
ttest assetind_ilri,by(Charc)
ttest borrow500,by(Charc)
ttest animalind,by(Charc)


***Demographs by FAW***
*keep if year==2018
ttest hh_head_age,by(Charc)
ttest hh_head_edu,by(Charc)
ttest hh_head_sex,by(Charc)

***Production and productivity
ttest Total,by(Charc)
ttest fmaize,by(Charc)

***Land ownership and acquisition
ttest farmsize_W01,by(Charc)
ttest cultivown_land,by(Charc)

***Wealth
ttest assetind_ilri,by(Charc)
ttest borrow500,by(Charc)
ttest animalind,by(Charc)


**Charc quatitity regression
keep if year==2018|year==2019
*gen lCharc=log(Qcharc)
gen lCharc=ln(Qcharc)
gen qchar=Qcharc*50
reghdfe lCharc l_FAW l_rainfall s_lrainfall, absorb (i.HHID) 
reghdfe Deforestion lCharc l_FAW l_rainfall s_lrainfall, absorb (i.HHID) 
ivreghdfe Deforestion   l_rainfall s_lrainfall i.year (Charc=army_aff) , absorb (i.HHID) first 
reghdfe Charc l_FAW  l_rainfall s_lrainfall year , absorb (i.HHID) 
reghdfe Deforestion Charc   l_rainfall s_lrainfall year , absorb (i.HHID) 
reghdfe qchar l_FAW l_rainfall s_lrainfall  , absorb (i.district)
reghdfe Qcharc l_FAW l_rainfall s_lrainfall i.year  , absorb (i.district)
reghdfe Qcharc  l_FAW y2017 y2018 y2019, absorb (i.district)


 

tobit Deforestion Qcharc  l_rainfall s_lrainfall farmland hh_head_edu hhsize hh_head_sex , vce(cluster HHID)
reghdfe Deforestion lCharc  l_rainfall s_lrainfall farmland hh_head_edu hhsize hh_head_sex  i.year , absorb (i.district) 

*ivreghdfe Deforestion   l_rainfall s_lrainfall  (Charc=army_aff) ,  vce(cluster HHID)
tobit  qchar l_FAW i.year , vce(cluster district)
outreg2 using results2, excel replace
tobit  qchar l_FAW farmland hh_head_edu hhsize hh_head_sex  l_rainfall s_lrainfall i.year , vce(cluster district)
outreg2 using results2, excel append

tobit Deforestion  lCharc l_rainfall s_lrainfall  , vce(cluster HHID)
tobit Deforestion  Qcharc l_rainfall s_lrainfall  , vce(cluster HHID)

drop _merge
save temp.dta, replace

/****Balanced table i the baseline
keep HHID year army 
reshape wide army, i(HHID) j(year)
gen treat1=1 if army2017 == 1
replace treat1 = 0 if treat1 ==.
/*gen treat2=1 if army_aff2017 == 2
replace treat2 = 0 if treat2 ==.
gen treat3=1 if army_aff2017 == 3
replace treat3 = 0 if treat3 ==.*/

merge 1:m HHID using temp.dta, nogen

gen c_grp=1 if army2017==0 
replace c_grp = 0 if c_grp==.
*gen control = treat== 1 


reg Charc c_grp if year==2016, cl(camp1)
outreg2 using results.tex, replace

foreach x in  hh_head_age hh_head_edu hh_head_sex farmland cultivown_land fmaize Total_income rainfall borrow500 Total_w  {
	
	reg `x'  c_grp if year==2016, cl(camp1)
	outreg2 using results.tex, append

	}

ttest hh_head_age if year==2016, by (treat1) 
ttest hh_head_edu if  year==2016, by (treat1) 
ttest hh_head_sex if  year==2016, by (treat1) 
ttest farmland if  year==2016, by (treat1) 
ttest cultivown_land if  year==2016, by (treat1) 
ttest fmaize if  year==2016, by (treat1)
ttest Total_income if  year==2016, by (treat1)
ttest rainfall, by (army) if year==2016
ttest borrow500, by (army) if year==2016
ttest Total_w, by (army) if year==2016
ttest assetind_ilri, by (army) if year==2016



foreach x in Charc hh_head_age hh_head_edu hh_head_sex farmland cultivown_land fmaize Total_income rainfall borrow500 Total_w assetind_ilri {
	
	reg `x'  treat1 if year==2016, cl(camp1)

	est store reg`i'
	local i=`i'+1
	pause
	}
	
outreg2 using results, excel append

foreach x in Charc hh_head_age hh_head_edu hh_head_sex farmland cultivown_land fmaize Total_income rainfall borrow500 Total_w assetind_ilri {
	
	reg `x'  treat2 if year==2016, cl(camp1)

	est store reg`i'
	local i=`i'+1
	pause
	}
	
outreg2 using results, excel append
	
foreach x in Charc hh_head_age hh_head_edu hh_head_sex farmland cultivown_land fmaize Total_income rainfall borrow500  assetind_ilri {
	
	reg `x'  treat3 if year==2016, cl(camp1)

	est store reg`i'
	local i=`i'+1
	pause
	}	
outreg2 using results, excel append*/

/*keep if year==2017
gen FRA3=kgs_soldfra>0
collapse FRA3,by (district)
	
*save "Charcoal.csv", replace
// Basal and top dressing together
/* Added on Jan 12, 2020: per feedback from Nicky
egen qbasal_top = rowtotal(qbasal qtop), m
gen qbasal_top_ha = qbasal_top / farmsize
label var qbasal_top_ha "Total basal and top dressing fertilizer: kg/ha"



// generating a binary variable (=1 if HH used fertilizer)

** Procedure 2: I utilize the total quantities declared by HH's for Top and Basal fertilizer. I generate my 0/1 dummy (=1 if HH used fertilizer) if qtop or qbasal > 0
	*Note: This method works better
gen fert_used = 0 // this is the desired 0/1 dummy
label var   fert_used "If HH used top or basal fertilizer: 1=Yes

replace fert_used = 1 if (qbasal > 0 | qtop > 0)

label define fert_used 1 "Yes" 0 "No", replace
label values fert_used fert_used

replace fert_used=. if qbasal==. & qtop==.

bys year: tab fert_used, m



// Distance to agro-dealer
gen lnagrodealer_dist = log(agrodealer_dist)
label var lnagrodealer_dist "Log of walking distance to agro-dealer in minutes"



// Creating field crop categories, based on Nicky Mason's classification

/* Legumes & oilseeds: Common Beans (14), Cowpeas (15), Groundnuts (8), Soyabeans (9), Sunflower (7)
replace combeans = 1 if inlist(crop,"14") & combeans == .
replace cowpeas = 1 if inlist(crop,"15") & cowpeas == .
replace groundnuts = 1 if inlist(crop,"8") & groundnuts == .
replace soybean = 1 if inlist(crop,"9") & soybean == .
replace sunflower = 1 if inlist(crop,"7") & sunflower == .

replace combeans = 1 if hec_combeans!=. & year==2019
replace cowpeas = 1 if hec_cowpeas!=. & year==2019
replace groundnuts = 1 if hec_groundnuts!=. & year==2019
replace soybean = 1 if hec_soyabeans!=. & year==2019
replace sunflower = 1 if hec_sunflower!=. & year==2019

gen othercrop_group1 = 0
replace othercrop_group1=. if crop==""
replace othercrop_group1 = 1 if combeans==1 | cowpeas==1 | groundnuts==1 | soybean==1 | sunflower==1
label var othercrop_group1 "HH grew Legumes & oilseeds"

* Cash crops: cotton (10), tobacco (12), coffee, cashew, paprika
** We don't have "coffee, cashew, paprika" listed as crops in our survey
replace cotton = 1 if inlist(crop,"10") & cotton == .
replace tobacco = 1 if inlist(crop,"12") & tobacco == .

replace cotton = 1 if hec_cotton!=. & year==2019
replace tobacco = 1 if hec_tobacco!=. & year==2019

gen othercrop_group2 = 0
replace othercrop_group2=. if crop==""
replace othercrop_group2 = 1 if cotton==1 | tobacco==1
label var othercrop_group2 "HH grew Cash crops"

* Roots & tubers: Cassava (18), Irish Potato (11), Sweet Potato (17)
** We don't have "coffee, cashew, paprika" listed as crops in our survey
replace cassava = 1 if inlist(crop,"18") & cassava == .
replace ipotato = 1 if inlist(crop,"11") & ipotato == .
replace spotato = 1 if inlist(crop,"17") & spotato == .

replace cassava = 1 if hec_cassava!=. & year==2019
replace ipotato = 1 if hec_irishpots!=. & year==2019
replace spotato = 1 if hec_sweetpot!=. & year==2019

gen othercrop_group3 = 0
replace othercrop_group3=. if crop==""
replace othercrop_group3 = 1 if cassava==1 | ipotato==1 | spotato==1
label var othercrop_group3 "HH grew Roots & tubers"

* Other cereals (non-maize): Millet (6), Rice (5), Sorghum (4)
replace millet = 1 if inlist(crop,"6") & millet == .
replace rice = 1 if inlist(crop,"5") & rice == .
replace sorghum = 1 if inlist(crop,"4") & sorghum == .

replace millet = 1 if hec_millet!=. & year==2019
replace rice = 1 if hec_rice!=. & year==2019
replace sorghum = 1 if hec_sorghum!=. & year==2019

gen othercrop_group4 = 0
replace othercrop_group4=. if crop==""
replace othercrop_group4 = 1 if millet==1 | rice==1 | sorghum==1
label var othercrop_group4 "HH grew Other cereals"


egen othercrop_all_groups = rowtotal(othercrop_group1 othercrop_group2 othercrop_group3 othercrop_group4), m
replace othercrop_all_groups=1 if othercrop_all_groups>1 & othercrop_all_groups!=.
tab othercrop_all_groups
label var othercrop_all_groups "HH grew Other crops"

foreach v in othercrop_group1 othercrop_group2 othercrop_group3 othercrop_group4 othercrop_all_groups {
	label define `v' 0 "No" 1 "Yes", replace
	label values `v' `v'
}



// Number of field crops grown, based on Nicky Mason's classification
egen othercrop_count = rowtotal(othercrop_group1 othercrop_group2 othercrop_group3 othercrop_group4), m
label var othercrop_count "Number of field crops grown"

bys year: tab othercrop_count


// Alternative measure of "grewothercrop"
gen grewothercrop_2 = (othercrop_count >= 1)
label var grewothercrop_2 "=1 if grew at least one non-maize field crop (alt.)"
order grewothercrop_2, after(grewothercrop)




