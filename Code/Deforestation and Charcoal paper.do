************************************************************
* Author: Protensia Hadunka
* Project: Charcoal Production and Fall Armyworm (FAW)
* Purpose: Cleaning and data analysis 
************************************************************

* Load the main unbalanced household dataset
use "C:/Users/hadunka2/Box/HICPS Cleaning 07_12_19/HICPS_unbalanced", clear

* Clear any existing matrices and Mata memory
clear matrix
clear mata


************************************************************
* SECTION 1: Merge additional datasets
************************************************************

* Merge household characteristics from 2016 survey
merge m:1 HHID using "C:/Users/hadunka2/Box/HICPS Cleaning 07_12_19/2016 HICPS/2016 HICPS G.dta", ///
    keepusing(hh_head_sex hh_head_age hh_head_edu district camp province firewood_av10) ///
    nogen update

* Merge deforestation data (point and buffer zone)
joinby HHID year using "C:/Users/hadunka2/Box/Raw rainfall/Deforestation/Deforestation.dta"
joinby HHID year using "C:/Users/hadunka2/Box/Raw rainfall/Deforestation_buffer/Deforestation2.dta"

* Merge rainfall data and label merge status
joinby HHID year using "C:/Users/hadunka2/Box/Raw rainfall/Total_Rainfall.dta", ///
    _merge(merge_rain) unmatched(master) update

* Sort and reorder key variables
sort year HHID
order year HHID totalrain

* Merge temperature data
joinby HHID year using "C:/Users/hadunka2/Box/Temperature data/clean_temp.dta", ///
    _merge(merge_temp) unmatched(master) update
sort year HHID
order year HHID temperature

* Rename rainfall variable
rename totalrain rainfall

* Create squared and log-transformed rainfall variables
gen sqrainfall = rainfall^2
gen log_rain = log(rainfall)
gen sq_rainfall = log_rain^2

************************************************************
* SECTION 2: Constructing Fall Armyworm (FAW) variables
************************************************************

* Replace missing Armyworm values in 2018 with 0
replace Armyworms = 0 if missing(Armyworms) & year == 2018

* Create 'army' variable based on Armyworms (only valid for 2018)
replace army = Armyworms if year == 2018

* Set army to 0 for all 2016 observations (no FAW data)
replace army = 0 if year == 2016

* Recode severity levels: combine levels 4 and 5 into category 3
recode army_aff (4 5 = 3) if year == 2018

* Replace missing severity levels with 0
replace army_aff = 0 if missing(army_aff)


replace army=1 if year==2019 & army_aff>=0
replace army=0 if year==2019 & army_aff==0


************************************************************
* SECTION 3: Create Cleaned Household ID and Panel Indicator
************************************************************

* Create a cleaned household ID for panel tracking
gen HHID_clean = HHID

* Generate a unique panel identifier for each household across waves
gen panel_ID = group(HHID)

* Create a panel indicator variable
gen in_panel = 1
replace in_panel = 0 if missing(year)


************************************************************
* SECTION 4: Yield and farm size cleaning
************************************************************

* Total maize production = harvested + left in the field
gen Total = qharvested + qleft

* Calculate total farm size across 5 planting plots
egen farmsize_W01 = rowtotal(plot_1 plot_2 plot_3 plot_4 plot_5), miss
label var farmsize "Maize farm size: ha"

* Drop extreme farm sizes (above 8 ha)
replace farmsize_W01 = . if farmsize_W01 > 8

* Log-transform farm size (raw and winsorized)
gen lnfarmsize = log(farmsize)
label var lnfarmsize "Log of maize farm size"
hist lnfarmsize, norm

gen lnfarmsize_W01 = log(farmsize_W01)
label var lnfarmsize_W01 "Log of maize farm size (winsorized at 1%)"
hist lnfarmsize_W01, norm

************************************************************
* SECTION 5: Clean farmland data and impute outliers
************************************************************

* Create clean copy of farmland
gen farmland_clean = farmland

* Replace large farmland values with camp-level medians
bys camp: egen med2_farmland = median(farmland)
replace farmland_clean = med2_farmland if farmland > 13

* Compare and adjust if cleaned size is less than reported
count if farmland_clean < farmsize_W01
list farmland_clean farmsize_W01 if farmland_clean < farmsize_W01
bys year: count if farmland_clean < farmsize_W01 & farmsize_W01 != .
replace farmland_clean = farmsize_W01 if farmland_clean < farmsize_W01 & farmsize_W01 != .

* Label cleaned variable
label var farmland_clean "Total area of farmland: Ha"

 Generate a cleaned version of farmsize variable
gen farmsize_clean = farmsize_W01

* Replace inconsistent entries with `farmland_clean`
replace farmsize_clean = farmland_clean if farmland_clean > farmsize_clean

* Create household size variable if not already present
gen hhsize = members if missing(hhsize)

* Generate per capita farmland
gen farmland_pc = farmland_clean / hhsize

* Check for unusual values
summarize farmland_pc, detail



************************************************************
* SECTION 6: Input variables - seed and fertilizer
************************************************************

* Total maize seed used across plots
gen quaseed = qseed_1 + qseed_2 + qseed_3 + qseed_4 + qseed_5
rename quaseed qseed

* Total top-dressing fertilizer used
gen qfer = qtop_1 + qtop_2 + qtop_3 + qtop_4 + qtop_5
rename qfer qtop

***************************************************************
* SECTION 7: Create Fertilizer Variables and Basic Aggregates
***************************************************************

* Aggregate quantities of basal fertilizer into a single variable
gen quafert = qbasal_1 + qbasal_2 + qbasal_3 + qbasal_4 + qbasal_5
rename quafert qbasal  // Rename for clarity

* Combine basal and top dressing fertilizers
gen fert = qtop + qbasal

* Encode the 'camp' variable as numeric
encode camp, gen(camp1)
gen camp2 = camp1  // Duplicate for robustness or alternate coding schemes

***************************************************************
* SECTION 8: Winsorize Key Variables by Year to Handle Outliers
***************************************************************

winsor2 Total, cuts(1 99) by(year)
winsor2 cultivown_land, cuts(1 99) by(year)


***************************************************************
* SECTION 9: Transformations for Household Characteristics
***************************************************************

* Replace missing or zero household size with 1 (minimal correction)
replace hh_num = 1 if hh_num == 0 | hh_num == .

* Apply inverse hyperbolic sine transformation to normalize skewed variables
ihstrans hh_num
ihstrans farmland

***************************************************************
* SECTION 10: PCA for Durable Asset Index 
***************************************************************

pca tv radio motorcycle water_pump plough sprayers ox_carts vehicle iron_sheets solar
predict capital  // First principal component as asset index
winsor2 capital, cuts(1 99) by(year)


recode coop 2 = 1  // Convert 2s to 1s to create binary coop variable

sort year
by year: tab army  // Distribution of Fall Armyworm reports by year

* Create dummy variables for different severity levels
gen low = (army_aff == 1)
gen med = (army_aff == 2)
gen seve = (army_aff == 3)

* Replace missing values with 0
recode low .=0
recode med .=0
recode seve .=0


************************************************************
* SECTION 11: Forest Perception (Commented out visualizations)
************************************************************

 hist firewood_av10, percent barw(0.4) color(eltblue) ///
    xlabel(1 "Increased" 2 "Same" 3 "Decreased", angle(vertical))

hist Charc_business, percent barw(0.4) color(eltblue) ///
     by(year, col(4)) xlabel(0 "No" 1 "Yes", angle(vertical))
	 
************************************************************
* SECTION 12: Rainfall Transformation and Yield Variables
************************************************************

rename totalrain rainfall
gen sqrainfall = rainfall^2
gen log_rain = log(rainfall)
gen sq_rainfall = log_rain^2


********************************************************************************
* Weather and FAW Effects
********************************************************************************
ihstrans temperature 
rename ihs_temperature ltemperature
gen sqtemp = temperature^2
ihstrans sqtemp 
rename ihs_sqtemp sqtemperature
gen sq_temp = temperature^2


************************************************************
* SECTION 13: Maize Yield Calculation
************************************************************

replace qharvested = 0 if missing(qharvested)
replace qleft = 0 if missing(qleft)
gen Total = qharvested + qleft

************************************************************
* SECTION 14: Farm Size and Winsorization
************************************************************

* Total plot area from all 5 plots
egen farmsize_W01 = rowtotal(plot_1 plot_2 plot_3 plot_4 plot_5), missing
label var farmsize_W01 "Maize farm size (ha)"

* Remove outliers (very large farms)
replace farmsize_W01 = . if farmsize_W01 > 8

* Log transformations
gen lnfarmsize = log(farmsize)
label var lnfarmsize "Log of maize farm size"

gen lnfarmsize_W01 = log(farmsize_W01)
label var lnfarmsize_W01 "Log of maize farm size (winsorized)"


************************************************************
* SECTION 15: Cleaning Reported Farmland Size
************************************************************

* Calculate medians for imputation
bys village: egen med1_farmland = median(farmland)
bys camp: egen med2_farmland = median(farmland)

gen farmland_clean = farmland
replace farmland_clean = med2_farmland if farmland > 13

* Summary and tabulation
bys year: summarize farmland_clean farmsize_W01
bys year: tabulate farmland_clean, missing

* Check discrepancies between cleaned and plot-calculated area
count if farmland_clean < farmsize_W01
list farmland_clean farmsize_W01 if farmland_clean < farmsize_W01



************************************************************
* SECTION 16: Cleaning Cultivated Land Size
************************************************************
* Check and correct values of cultivated land size
* Replace missing values with 0 where appropriate

replace cultland = 0 if missing(cultland)
replace cultland = 0 if cultland == .

replace cultland = 99 if cultland > 99

-
gen cultland_clean = cultland


tabulate cultland_clean, missing
summarize cultland_clean, detail


************************************************************
* SECTION 17: Cleaning Maize Harvest Quantities
************************************************************

replace maize_harv = 0 if maize_harv < 0 | missing(maize_harv)


* Cap extremely large maize harvest values to plausible limit
* Based on national production context, 40000 kg (~40 MT) is used

replace maize_harv = 40000 if maize_harv > 40000


gen maize_harv_clean = maize_harv

summarize maize_harv_clean, detail
histogram maize_harv_clean, width(500) percent ///
    title("Distribution of Cleaned Maize Harvest Quantities") ///
    xtitle("Maize Harvest (kg)") ytitle("Percent")
	
	
************************************************************
* SECTION 18: Charcoal Production – Quantity and Revenue
************************************************************

* Replace missing charcoal quantities with 0
replace charcoal_kg = 0 if missing(charcoal_kg)
label variable charcoal_kg "Total Quantity of Charcoal Produced (kg)"

* Replace missing values for charcoal revenue with 0
replace charcoal_value = 0 if missing(charcoal_value)
label variable charcoal_value "Revenue from Charcoal Sales (Kwacha)"

* Generate average price per kg of charcoal, handle division by zero
gen charcoal_price_perkg = charcoal_value / charcoal_kg if charcoal_kg > 0
label variable charcoal_price_perkg "Average Price per kg of Charcoal (Kwacha)"



************************************************************
* SECTION 20: Household Asset Index Construction
************************************************************

* Replace invalid or missing values with 0 for asset ownership indicators
foreach var in tv bicycle radio cellphone motorbike {
    replace `var' = 0 if missing(`var')
}

* Principal Component Analysis (PCA) for asset index
pca tv bicycle radio cellphone motorbike, components(1)
predict asset_index, score
label variable asset_index "Household Asset Index (1st PCA Component)"

************************************************************
* SECTION 21: Prepare for Analysis of Charcoal Production Drivers
************************************************************

* Generate logs of key variables, handling zeros and missing
gen ln_income = log(hh_income) if hh_income > 0
gen ln_assets = log(hh_assets) if hh_assets > 0
gen ln_labor = log(hh_labor) if hh_labor > 0

label variable ln_income "Log of household income"
label variable ln_assets "Log of household assets"
label variable ln_labor  "Log of household labor"


************************************************************
* SECTION 22: Create Landholding Per Capita Variable
************************************************************

* Generate land per capita based on cleaned farmland size and household size
gen land_per_capita = farmland_clean / hhsize if hhsize > 0

* Replace with missing if household size is invalid
replace land_per_capita = . if hhsize <= 0

* Inspect distribution by year
bysort year: summarize land_per_capita


* Recode missing values in access_credit variable
replace access_credit = 0 if missing(access_credit) & inlist(year, 2016, 2018)

* Check value distribution by year
tabulate access_credit year, missing

* Label the variable for clarity
label variable access_credit "Access to Credit (0=No, 1=Yes)"



************************************************************
* SECTION: Create Instrumental Variables (IVs) at Camp Level
************************************************************

* Define FAW intensity categories
gen low = (army_aff == 1)
replace low = 0 if missing(low)

gen med = (army_aff == 2)
replace med = 0 if missing(med)

gen seve = (army_aff == 3)
replace seve = 0 if missing(seve)

* Create camp-level sums excluding own household (leave-one-out means)
egen iv_camp1 = sum(low), by(camp year)
egen iv_camp2 = sum(med), by(camp year)
egen iv_camp3 = sum(seve), by(camp year)

gen hh_iv_low = iv_camp1 - low
gen hh_iv_med = iv_camp2 - med
gen hh_iv_seve = iv_camp3 - seve

* Generate camp-level denominators for IVs
egen num_camp1 = sum(low), by(camp year)
egen num_camp2 = sum(med), by(camp year)
egen num_camp3 = sum(seve), by(camp year)
egen camp_total = count(HHID), by(camp year)

gen camp_denom1 = camp_total - 1
gen camp_IV1 = (num_camp1 - 1) / camp_denom1
gen camp_IV2 = (num_camp2 - 1) / camp_denom1
gen camp_IV3 = (num_camp3 - 1) / camp_denom1

************************************************************
* SECTION: Binary FAW Instrument
************************************************************

egen num_faw = sum(army), by(camp year)
gen camp_IV_bin = (num_faw - 1) / camp_denom1
gen IV_bin = (sum(army) - army) / camp_denom1

************************************************************
* SECTION: Construct Leave-One-Out Means for Army Affliction
************************************************************

egen camp_sum_aff = sum(army_aff), by(camp year)
egen camp_sum_bin = sum(army), by(camp year)

gen diff_camp_aff = camp_sum_aff - army_aff
gen diff_camp_bin = camp_sum_bin - army

ihstrans diff_camp_aff
rename ihs_diff_camp_aff army_diff

************************************************************
* SECTION: Camp Count and Ratio IV
************************************************************

egen camp_count_bin = count(army), by(camp year)
gen camp_denom_bin = camp_count_bin - 1

gen camp_IV_ratio = diff_camp_bin / camp_denom_bin
gen camp_IV_mean = diff_camp_bin / camp_denom1

************************************************************
* SECTION: Agricultural Yields and Income Construction
************************************************************

* Replace missing plot values with zero, then sum up maize plot area
recode plot_1 plot_2 plot_3 plot_4 plot_5 (.=0)
gen maize_area = plot_1 + plot_2 + plot_3 + plot_4 + plot_5

* Declare panel structure
sort HHID year
xtset HHID year

* Compute yield per hectare
gen fmaize = Total_w / totlandng
gen lyield = ln(fmaize)

* Year dummies
gen y2017 = (year == 2017)
gen y2018 = (year == 2018)
gen y2019 = (year == 2019)

recode y2017 y2018 y2019 (.=0)

************************************************************
* SECTION: Transformations and Income Components
************************************************************

ihstrans rainfall
ihstrans sqrainfall
ihstrans farmland hpsfarmland

rename ihs_rainfall lrainfall
rename ihs_sqrainfall lsqrainfall

* Construct total and non-agricultural income
gen Total_income = income_piecework + income_salary + income_smallbusiness + ///
                   income_charcoal + income_gardening + income_forestproduct + ///
                   income_livestock + income_remittance + income_other

gen non_aginc = income_remittance + income_other + income_salary + income_piecework
gen log_inc = log(Total_income)
gen sq_inc = log_inc^2


************************************************************
* SECTION: Cleaning Access to Agricultural Extension Services
************************************************************

* Recode extreme or placeholder values for extension service indicators
foreach var in ext_contact ext_group ext_training ext_meeting {
    replace `var' = . if inlist(`var', 99, 999, -99)
}

* Generate indicator for access to any extension service
gen any_extension = (ext_contact == 1 | ext_group == 1 | ext_training == 1 | ext_meeting == 1)
replace any_extension = 0 if missing(any_extension)

* Summary statistics and checks by year
foreach var in ext_contact ext_group ext_training ext_meeting any_extension {
    tabulate `var' year, missing
    summarize `var'
}

* Additional ihstrans transformations for capital and inputs
ihstrans capital_w
rename ihs_capital_w hpscapital
rename ihs_farmland hpsfarmland
ihstrans cultivown_land_w qseed fert
rename ihs_hh_num hpsHHsize
rename ihs_cultivown_land_w totlandng
rename ihs_qseed hpsquaseed
rename ihs_fert hpsfert


*--------------------------------------------
* Recode and clean monthly expenditure items
*--------------------------------------------
* Kathy suggested excluding veterinary expenses

destring clothing_cost_month, gen(clothing_cost_mont) force
destring alcohol_cost_month, gen(alcohol_cost_mont) force
destring charcoal_cost_month, gen(charcoal_cost_mont) force
destring firewood_cost_month, gen(firewood_cost_mont) force

foreach v in food_budget_7day talktime_budget_7day clothing_cost_mont ///
             transportation_cost_month alcohol_cost_mont firewood_cost_mont ///
             charcoal_cost_mont other_cost_month {
    recode `v' (.=0)
    tab `v'
}

*--------------------------------------------
* Generate Annual Household Expenditure
*--------------------------------------------

* With food expenditure included
egen hhexp1 = rowtotal(food_budget_7day talktime_budget_7day)
egen hhexp2 = rowtotal(clothing_cost_mont transportation_cost_month alcohol_cost_mont ///
                       firewood_cost_mont charcoal_cost_mont other_cost_month)
gen hhexp_yr = 52 * hhexp1 + 12 * hhexp2
label var hhexp_yr "Annual HH expenditure w/ food (Kwacha)"

* Without food expenditure
gen hhexp_yr2 = 52 * talktime_budget_7day + 12 * hhexp2
label var hhexp_yr2 "Annual HH expenditure w/o food (Kwacha)"

drop hhexp1 hhexp2

bys year: tab hhexp_yr, m
bys year: tab hhexp_yr2, m

* Kernel density plots
tw (kdensity hhexp_yr if year==2016)(kdensity hhexp_yr if year==2017) ///
   (kdensity hhexp_yr if year==2018)(kdensity hhexp_yr if year==2019)

tw (kdensity hhexp_yr2 if year==2016)(kdensity hhexp_yr2 if year==2017) ///
   (kdensity hhexp_yr2 if year==2018)(kdensity hhexp_yr2 if year==2019)

*--------------------------------------------
* Generate food expenditure share
*--------------------------------------------
gen foodexp_share = (52 * food_budget_7day) / hhexp_yr
label var foodexp_share "Share of food in total expenditure"
bys year: tab foodexp_share, m

tw (kdensity foodexp_share if year==2016)(kdensity foodexp_share if year==2017)

*--------------------------------------------
* Generate logged expenditure variables
*--------------------------------------------

* Log of annual household expenditure (with and without food)
gen lnhhexp_yr = log(hhexp_yr)
label var lnhhexp_yr "Log of annual HH expenditure w/ food"

gen lnhhexp_yr2 = log(hhexp_yr2)
label var lnhhexp_yr2 "Log of annual HH expenditure w/o food"

tw (kdensity lnhhexp_yr if year==2016)(kdensity lnhhexp_yr if year==2017) ///
   (kdensity lnhhexp_yr if year==2018)(kdensity lnhhexp_yr if year==2019)

tw (kdensity lnhhexp_yr2 if year==2016)(kdensity lnhhexp_yr2 if year==2017) ///
   (kdensity lnhhexp_yr2 if year==2018)(kdensity lnhhexp_yr2 if year==2019)

* Annual food expenditure and log
gen hhfoodexp_yr = 52 * food_budget_7day
label var hhfoodexp_yr "Annual HH food expenditure"

gen lnhhfoodexp_yr = log(hhfoodexp_yr)
label var lnhhfoodexp_yr "Log of annual HH food expenditure"

tw (kdensity lnhhfoodexp_yr if year==2016)(kdensity lnhhfoodexp_yr if year==2017) ///
   (kdensity lnhhfoodexp_yr if year==2018)(kdensity lnhhfoodexp_yr if year==2019)

*--------------------------------------------
* Principal Component Analysis (PCA) for Asset Index
*--------------------------------------------
encode pigs_number, gen(pigs_count)

pca oxen breeding_bull donkey female_cattle_number goat_sheep_number ///
    poultry_number pigs_count asset_phone tv radio bike motorcycle ox_carts ///
    vehicle water_pump plough sprayers

predict assetind_pca
label var assetind_pca "Household Asset Index (PCA)"

screeplot, ci(asympt level(95)) mean scheme(lean2)
loadingplot

*--------------------------------------------
* ILRI Method for Asset Index
*--------------------------------------------
foreach v in oxen breeding_bull donkey female_cattle_number goat_sheep_number ///
            poultry_number pigs_count asset_phone tv radio bike motorcycle ///
            ox_carts vehicle water_pump plough sprayers {
    replace `v' = 0 if missing(`v')
}

gen animalind = 10*(oxen + breeding_bull + donkey + female_cattle_number) + ///
                3*goat_sheep_number + 1*poultry_number + 2*pigs_count

gen domesticind = 3*asset_phone + 4*tv + 2*radio
gen transportind = 6*bike + 48*motorcycle + 12*ox_carts + 160*vehicle
gen productiveind = 6*water_pump + 4*(plough + sprayers)

gen assetind_ilri = domesticind + animalind + transportind + productiveind
label var assetind_ilri "Household Asset Index (ILRI Method)"

bysort year: tab assetind_ilri, m
tw (kdensity assetind_ilri if year==2016)(kdensity assetind_ilri if year==2017)

*--------------------------------------------
* Interactions and logs for asset index
*--------------------------------------------
gen ass_index1 = army_aff * assetind_ilri
gen ass_index = log(ass_index1)
gen ani_ass = l_FAW * animalind

sort year HHID
order year HHID assetind_ilri

************************************************************
* SECTION: Fixed Effects Regressions - Main Outcome Variables
************************************************************

* Regression 1: FAW effects on maize yield (log yield per hectare)
xtreg lyield army_aff lrainfall lsqrainfall ///
      hpsfarmland  any_extension y2017 y2018 y2019, fe vce(cluster HHID)

* Regression 2: FAW binary impact on maize yield
xtreg lyield army lrainfall lsqrainfall ///
      hpsfarmland  any_extension y2017 y2018 y2019, fe vce(cluster HHID)

************************************************************
* SECTION: Instrumental Variables (IV) Regressions
************************************************************

* First Stage: IV for army_aff using camp-level FAW mean intensity
xtivreg lyield (army_aff = camp_IV) lrainfall lsqrainfall ltemperature ///
        hpsfarmland  any_extension y2017 y2018 y2019, fe vce(cluster HHID)

* Alternative IV: Using binary FAW exposure
xtivreg lyield (army = camp_IV_bin) lrainfall lsqrainfall ltemperature ///
        hpsfarmland  any_extension y2017 y2018 y2019, fe vce(cluster HHID)

************************************************************
* SECTION: Robustness Check with Leave-One-Out Mean IV
************************************************************

* Using log deflated total income as the outcome
xtivreg log_inc (army_aff = army_diff) lrainfall lsqrainfall ltemperature ///
        hpsfarmland  any_extension y2017 y2018 y2019, fe vce(cluster HHID)

************************************************************
* SECTION: Charcoal Production as Coping Mechanism
************************************************************

* Fixed Effects model with FAW severity
xtreg income_charcoal army_aff lrainfall lsqrainfall ltemperature ///
      hpsfarmland  any_extension y2017 y2018 y2019, fe vce(cluster HHID)

* IV regression for charcoal income
xtivreg income_charcoal (army_aff = camp_IV) lrainfall lsqrainfall ltemperature ///
        hpsfarmland  any_extension y2017 y2018 y2019, fe vce(cluster HHID)

************************************************************
* SECTION: Placebo Tests - Use Pre-FAW Period (2016 Only)
************************************************************

* Restrict to 2016 (before FAW infestation)
keep if year == 2016

* Regression with FAW variable in 2016 (placebo, should have no effect)
xtreg lyield army_aff lrainfall lsqrainfall ltemperature///
      hpsfarmland  any_extension, fe vce(cluster HHID)
	  
	  
* Recreate dependent variable and controls
gen lyield = ln(maize_harvest + 1)
gen lrainfall = ln(rain_total + 1)
gen lsqrainfall = lrainfall^2
gen hpsfarmland = farmland_clean / hhsize	  
	  
	  
* Prepare IV variables
gen l_FAW = army_aff[_n-1] if HHID == HHID[_n-1]
gen post2016 = year >= 2016

* Interaction instrument: lag of armyworms × post2016
gen iv = army_lag * post2016



* Pooled OLS without household fixed effects
reg lyield l_FAW lrainfall lsqrainfall hpsfarmland ltemperature ///
    i.year, cluster(HHID)

* Pooled OLS with time fixed effects and controls
reg lyield l_FAW lrainfall lsqrainfall hpsfarmland ltemperature ///
    any_extension i.year i.district, cluster(HHID)
	
	
************************************************************
* SECTION: Heterogeneity Analysis
************************************************************

* Interaction with initial land size
gen army_land = army * hpsfarmland
reg lyield army army_land hpsfarmland lrainfall lsqrainfall ltemperature///
     i.year i.district, cluster(HHID)	
	 
gen small_farm = hpsfarmland < median(hpsfarmland)

* Interaction between extension and farm size
reg lyield l_FAW lrainfall lsqrainfall hpsfarmland ltemperature ///
    c.any_extension##i.small_farm i.year, cluster(HHID)
	

gen male_head = (gender == 1) if !missing(gender)

* Interaction of extension and gender of household head
reg lyield l_FAW lrainfall lsqrainfall hpsfarmland ltemperature ///
    c.any_extension##i.male_head i.year, cluster(HHID)
	
reg charcoal army lrainfall lsqrainfall hpsfarmland ltemperature ///
    any_extension i.year i.district, cluster(HHID)	
	
************************************************************
* SECTION: Extensive Margin - Probability of Producing Charcoal
************************************************************

* Logistic regression for binary charcoal production outcome
logit Charc army lrainfall lsqrainfall hpsfarmland ltemperature ///
    any_extension i.year i.district, cluster(HHID)	
	
* Restrict sample to households that produced charcoal
keep if Charc > 0

* Log-transform the quantity of charcoal produced
gen lcharcoal = log(charcoal)

* OLS regression on log quantity of charcoal produced
regress lcharcoal army lrainfall lsqrainfall hpsfarmland ltemperature ///
    any_extension i.year i.district, cluster(HHID)
	
* Restrict to households that produced charcoal
keep if charcoal > 0

* Take log of charcoal quantity to normalize distribution
gen ln_charcoal = ln(charcoal)

* Linear regression on log-charcoal to capture intensive margin
reg ln_charcoal army lrainfall lsqrainfall hpsfarmland ltemperature ///
    any_extension i.district i.region, cluster(HHID)


* Estimate linear probability model with household fixed effects
xtreg Charc army lrainfall lsqrainfall hpsfarmland ltemperature ///
    any_extension i.year, fe vce(cluster HHID)
	
	
* Estimate probit model with year fixed effects
probit charcoal_production army lrainfall lsqrainfall hpsfarmland ltemperature ///
     any_extension i.year
	 
	
* Estimate linear regression model for the extensive margin (binary outcome)
logit Charc army lrainfall lsqrainfall hpsfarmland ///
     any_extension i.year, robust


*******************************************************************************
* Camp Fixed Effects
********************************************************************************
reghdfe Charc l_FAW hh_head_edu hhsize hh_head_sex Total_income farmland l_rainfall ltemperature fert, absorb(i.camp2) cl(camp1)
outreg2 using results, excel replace

reghdfe Charc camIV hh_head_edu hhsize hh_head_sex Total_income farmland l_rainfall ltemperature fert, absorb(i.camp2) cl(camp1)
outreg2 using results, excel replace

ivreghdfe Charc hh_head_edu hhsize hh_head_sex Total_income farmland l_rainfall fert ltemperature (l_FAW=l_camIV), absorb(i.camp2) first cl(camp1)
outreg2 using results, excel append

********************************************************************************
* District Fixed Effects
********************************************************************************
reghdfe Charc l_FAW hh_head_edu hhsize hh_head_sex Total_income farmland s_lrainfall FISP fert, absorb(i.district) cl(camp1)
outreg2 using results, excel replace

ivreghdfe Charc hh_head_edu hhsize hh_head_sex Total_income farmland s_lrainfall FISP fert (l_FAW=l_camIV), absorb(i.district) first cl(camp1)
outreg2 using results, excel append

ivreghdfe Charc l_camIV hh_head_edu hhsize hh_head_sex Total_income farmland s_lrainfall FISP fert, absorb(i.district)
outreg2 using results, excel append

********************************************************************************
* Leads Test
********************************************************************************
reghdfe Charc l_FAW army_aff hhsize Total_income farmland fert l_rainfall s_lrainfall, absorb(i.HHID)
estimates store charc1

coefplot (charc1, asequation(l_FAW)) ///
, drop(_cons) keep(l_FAW army_aff) ///
mlabel(cond(@pval<.01, "***", cond(@pval<.05, "**", cond(@pval<.1, "*", "")))) mlabc(red) mlabsize(medium) mlabgap(6pt) mlabp(9) ///
eqlabel(, labsize(small)) msymbol(circle) mcolor(ebblue) ciopts(recast(rcap) lcolor(ebblue)) ///
xline(0, lcolor(red)) ylabel(, labsize(vsmall)) omitted baselevels ///
bylabels("Food Group Diversification Index (FGDI)" "Household Dietary Diversity (HDDS)") ///
byopts(compact rows(1) note("p-values shown alongside markers" "*** p<.01, ** p<.05, * p<.1"))

********************************************************************************
* Bacon Decomposition Model
********************************************************************************
quietly regress Charc l_FAW army_aff l_rainfall s_lrainfall ltemperature cultivown_land hh_head_edu hhsize hh_head_sex
keep if e(sample)

foreach v in l_FAW army_aff l_rainfall s_lrainfall ltemperature cultivown_land hh_head_edu hhsize hh_head_sex {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx 
estimates store charc

coefplot (charc, asequation(l_FAW)) ///
, drop(_cons) keep(l_FAW army_aff) ///
mlabel(cond(@pval<.01, "***", cond(@pval<.05, "**", cond(@pval<.1, "*", "")))) mlabc(red) mlabsize(medium) mlabgap(6pt) mlabp(9) ///
eqlabel(, labsize(small)) msymbol(circle) mcolor(ebblue) ciopts(recast(rcap) lcolor(ebblue)) ///
xline(0, lcolor(red)) ylabel(, labsize(vsmall)) omitted baselevels ///
byopts(compact rows(1) note("p-values shown alongside markers" "*** p<.01, ** p<.05, * p<.1"))

********************************************************************************
* Household Fixed Effects
********************************************************************************
reghdfe Charc l_FAW l_rainfall s_lrainfall ltemperature , absorb(i.HHID) 
reghdfe Charc i.l_FAW l_rainfall s_lrainfall ltemperature i.year dist_yr, absorb(i.HHID)
outreg2 using results, excel replace

ivreghdfe Charc l_camIV l_rainfall s_lrainfall ltemperature , absorb(i.HHID)
outreg2 using results, excel append

reghdfe Charc l_FAW l_rainfall ltemperature i.year, absorb(i.HHID)



reghdfe cultivatedland l_FAW
reghdfe cultivatedland l_FAW, absorb(i.year i.HHID)

reghdfe hpsfarmland l_FAW l_rainfall s_lrainfall ltemperature, absorb(i.year i.district)
reghdfe hpsfarmland camIV l_rainfall s_lrainfall ltemperature, absorb(i.year i.district)

ivreghdfe hpsfarmland (l_FAW = camIV) l_rainfall l_rainfall, absorb(i.year i.district)

********************************************************************************
* Charcoal and Deforestation Analysis
********************************************************************************
reghdfe deforestation_area_km_squared dist_forest cultivown_land hh_head_edu ban_firewood_areas
reghdfe deforestation_area_km_squared dist_forest FAW_distfore cultivown_land hh_head_edu ban_firewood_areas
reghdfe Charc dist_obs cultivown_land hh_head_edu ban_firewood_areas

gen logforest = log(deforestation_area_km_squared)

reghdfe logforest f_army self_interest self_FAW cultivown_land hh_head_edu ban_firewood_areas, absorb(i.district)
outreg2 using results.tex, replace

reghdfe logforest dist_forest FAW_distfore cultivown_land hh_head_edu ban_firewood_areas, absorb(i.camp2)
outreg2 using results.tex, replace

reghdfe Charc dist_forest FAW_distfore cultivown_land hh_head_edu ban_firewood_areas, absorb(i.district)
outreg2 using results.tex, append

reghdfe deforestation_area_km_squared dist_obs cultivown_land hh_head_edu ban_firewood_areas, absorb(i.district)

reghdfe logforest dist_obs faw_dist cultivown_land hh_head_edu ban_firewood_areas

reghdfe Charc dist_forest cultivown_land hh_head_edu area_responsible

reghdfe Charc dist_forest FAW_distfore cultivown_land hh_head_edu area_responsible

********************************************************************************
* Distance as a Natural Enemy of FAW
********************************************************************************
gen hbsfaw = asinh(f_FAW)
gen hbsdist = asinh(dist_forest)

reghdfe hbsfaw hbsdist, absorb(i.camp2)
outreg2 using results.tex, replace

********************************************************************************
* Charcoal Quantity and FAW Regression
********************************************************************************
reghdfe Charc l_FAW cultivown_land l_rainfall s_lrainfall hhsize ltemperature sqtemperature, absorb(i.HHID)

reghdfe qchar l_FAW l_rainfall s_lrainfall ltemperature sqtemperature , absorb(i.HHID i.camp2 i.year i.district) cl(HHID)
estimates store charcno

reghdfe qchar l_FAW cultivown_land l_rainfall s_lrainfall hhsize ltemperature sqtemperature, absorb(i.HHID i.camp2 i.year i.district) cl(HHID)
estimates store qcharcyes

coefplot(qcharcyes, asequation(FE with controls)) ///
, drop(_cons) keep(l_FAW) ///
eqlabel(, labsize(small)) msymbol(circle) mcolor(ebblue) ciopts(recast(rcap) lcolor(ebblue)) ///
xline(0, lcolor(red)) ylabel(, labsize(vsmall)) omitted baselevels ///
byopts(compact rows(1) note("p-values shown alongside markers" "*** p<.01, ** p<.05, * p<.1"))

********************************************************************************
* Analysis of Expenditures and Other Variables
********************************************************************************
reghdfe lnhhfoodexp_yr l_FAW l_rainfall s_lrainfall i.year dist_yr, absorb(i.HHID)
outreg2 using results2, excel replace

reghdfe lnnon_ag l_FAW l_rainfall s_lrainfall, absorb(i.district)
outreg2 using results2, excel replace

reghdfe lwork l_FAW l_rainfall s_lrainfall, absorb(i.HHID)

********************************************************************************
* Capital and Assets Index
********************************************************************************
reghdfe Charc l_FAW army_cap capital l_rainfall s_lrainfall, absorb(i.district)
outreg2 using results2, excel append

********************************************************************************
* Coping Mechanisms and Livelihoods
********************************************************************************
reghdfe maizearea_share l_FAW l_rainfall s_lrainfall ltemperature sqtemperature, absorb(i.district) cl(camp2)
estimates store maizearea_share
gen log_maizeshare = log(maizearea_share)
reghdfe log_maizeshare l_FAW l_rainfall s_lrainfall ltemperature sqtemperature, absorb(i.HHID) cl(camp2)

quietly regress log_maizeshare l_FAW l_rainfall s_lrainfall ltemperature sqtemperature
keep if e(sample)
foreach v in l_FAW l_rainfall s_lrainfall temperature sqtemp {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx

********************************************************************************
* Final Results and Reporting
********************************************************************************
coefplot (maizearea_share, asequation(FE)) ///
||(spray, asequation(Lag_FAW)) ///
||(crop_div, asequation(Lag_FAW)) ///
||(migration, asequation(Lag_FAW)) ///
||(piecework, asequation(Lag_FAW)) ///
, drop(_cons) keep(l_FAW) ///
eqlabel(, labsize(small)) msymbol(circle) mcolor(ebblue) ciopts(recast(rcap) lcolor(ebblue)) xline(0, lcolor(red)) ylabel(, labsize(vsmall)) omitted baselevels ///
bylabels("Maize share" "Spraying" "Crop diversification" "Migration" "Off-farm work") ///
byopts(compact rows(1) note("p-values shown alongside markers" "*** p<.01, ** p<.05, * p<.1"))


********************************************************************************
* Camp Level Interactions and Analysis
********************************************************************************
egen camp_inter_1 = mean(inter_1), by(camp year)
reghdfe camp_inter_1 l_FAW l_rainfall s_lrainfall ltemperature sqtemperature, absorb(i.camp2) cl(camp1)
outreg2 using results2, excel append

egen grewothercrop1 = mean(grewothercrop), by(camp year)
reghdfe grewothercrop1 l_FAW l_rainfall s_lrainfall ltemperature sqtemperature, absorb(i.camp2) cl(camp1)
outreg2 using results2, excel append

** Growing Other Crops
reghdfe grewothercrop l_FAW l_rainfall s_lrainfall ltemperature sqtemperature i.year, absorb(i.district)
outreg2 using results2, excel append
gen grewother_FAW = grewothercrop * l_FAW
egen camp_grewothercrop = mean(grewothercrop), by(camp year)
reghdfe camp_grewothercrop l_FAW l_rainfall s_lrainfall ltemperature sqtemperature, absorb(i.district)

********************************************************************************
* Piecework Analysis
********************************************************************************
drop piecework
gen piecework = 0 if piecework_members == 0
replace piecework = 1 if piecework_members >= 1
reghdfe piecework l_FAW l_rainfall s_lrainfall ltemperature sqtemperature, absorb(i.camp2 i.year) cl(camp1)
outreg2 using results2, excel append
gen piecework_FAW = piecework * l_FAW
egen camp_piece = mean(piecework), by(camp year)

********************************************************************************
* Fresh Results and Calculations
********************************************************************************
egen camp_maize = mean(maizearea_share), by(camp year)
gen maize_arm = camp_ave * camp_maize

egen camp_crop = mean(grewothercrop), by(camp year)
gen crop_army = camp_ave * camp_crop

gen piece_army = camp_ave * camp_piece

egen mig_crop = mean(mem_left), by(camp year)
gen mig_army = camp_ave * mig_crop

egen spray_crop = mean(spray), by(camp year)
gen spray_arm = camp_ave * spray_crop

********************************************************************************
* Coping Strategies at Camp Level
********************************************************************************
reghdfe Charc camp_maize maize_arm camp_crop crop_army mig_crop mig_army camp_piece piece_army camp_spray spray_army l_rainfall s_lrainfall ltemperature sqtemperature i.year, absorb(i.camp_1) cl(camp1)
estimates store coping

coefplot (coping, asequation(LPM)) ///
, drop(_cons) keep(camp_maize maize_arm camp_crop crop_army mig_crop mig_army camp_piece piece_army camp_spray spray_army) ///
eqlabel(, labsize(small)) msymbol(circle) mcolor(ebblue) ciopts(recast(rcap) lcolor(ebblue)) ///
xline(0, lcolor(red)) ylabel(, labsize(vsmall)) omitted baselevels ///
bylabels("") ///
byopts(compact rows(1) note("p-values shown alongside markers" "*** p<.01, ** p<.05, * p<.1"))

reghdfe Charc camp_maize maize_arm camp_crop crop_army mig_crop mig_army camp_piece piece_army spray_crop spray_arm l_rainfall s_lrainfall temperature sqtemp, absorb(i.year i.camp_1)

quietly regress piecework l_FAW l_rainfall s_lrainfall ltemperature sqtemp
keep if e(sample)
foreach v in piecework l_FAW l_rainfall s_lrainfall ltemperature sqtemp {
    bysort HHID : egen double mean_`v' = mean(`v')
}
mfx

reghdfe piecework l_FAW l_rainfall s_lrainfall ltemperature sqtemperature, absorb(i.district)

********************************************************************************
* Livestock and Migration Analysis
********************************************************************************
gen livestock = log(animalind)
reghdfe livestock l_FAW l_rainfall s_lrainfall ltemperature sqtemperature, absorb(i.HHID)

** Migration
reghdfe mem_left l_FAW l_rainfall s_lrainfall ltemperature sqtemperature 
outreg2 using results2, excel append
gen mig_FAW = mem_left * l_FAW

egen camp_mig = mean(mem_left), by(camp year)
reghdfe camp_mig l_FAW l_rainfall s_lrainfall ltemperature sqtemperature, absorb(i.district)

reghdfe Charc maizearea_share maize_FAW grewothercrop grewother_FAW mem_left mig_FAW piecework piecework_FAW l_spray spray_FAW l_rainfall s_lrainfall temperature sqtemp
reghdfe Qcharc maizearea_share maize_FAW grewothercrop grewother_FAW mem_left mig_FAW piecework piecework_FAW l_spray spray_FAW l_rainfall s_lrainfall ltemperature sqtemp, absorb(i.year i.district)
reghdfe Qcharc maizearea_share maize_FAW grewothercrop grewother_FAW mem_left mig_FAW piecework piecework_FAW l_spray spray_FAW l_rainfall s_lrainfall ltemperature sqtemp, absorb(i.district i.camp2)
reghdfe Qcharc maizearea_share maize_FAW grewothercrop grewother_FAW mem_left mig_FAW piecework piecework_FAW l_spray spray_FAW l_rainfall s_lrainfall ltemperature sqtemp, absorb(i.district)
reghdfe Qcharc maizearea_share maize_FAW grewothercrop grewother_FAW mem_left mig_FAW piecework piecework_FAW l_spray spray_FAW l_rainfall s_lrainfall temperature sqtemp, absorb(i.camp2)
outreg2 using results.tex, replace

reghdfe Charc maizearea_share maize_FAW grewothercrop grewother_FAW mem_left mig_FAW piecework piecework_FAW l_spray spray_FAW l_rainfall s_lrainfall temperature sqtemp, absorb(i.district)
outreg2 using results.tex, replace

********************************************************************************
* Household Location and FAW Effects
********************************************************************************
gen increased = (firewood_av10 == 1)
gen constant = (firewood_av10 == 2)
gen decreased = (firewood_av10 == 3)

** Interactions
gen increasedFAW = increased * l_FAW
gen constantFAW = constant * l_FAW
gen decreasedFAW = decreased * l_FAW

gen increasedarmy = increased * army_aff
gen constantarmy = constant * army_aff
gen decreasedarmy = decreased * army_aff

reghdfe Charc l_FAW increased increasedFAW constant constantFAW decreased decreasedFAW, absorb(i.camp2 i.year)
reghdfe Charc l_FAW increased increasedFAW constant constantFAW decreased decreasedFAW l_rainfall s_lrainfall ltemperature sqtemperature, absorb(i.camp2 i.year)
reghdfe Charc l_FAW increased increasedFAW constant constantFAW decreased decreasedFAW l_rainfall s_lrainfall ltemperature sqtemperature, absorb(i.year)
reghdfe Charc l_FAW increased increasedFAW constant constantFAW decreased decreasedFAW l_rainfall s_lrainfall ltemperature sqtemperature, absorb(i.year)
outreg2 using results.tex, replace

reghdfe Charc l_FAW increased increasedarmy constant constantarmy decreased decreasedarmy l_rainfall s_lrainfall ltemperature sqtemperature, absorb(i.camp2 i.year)
outreg2 using results.tex, replace

reghdfe Charc l_FAW increasedarmy increased constant constantarmy decreasedarmy decreased l_rainfall s_lrainfall ltemperature sqtemperature, absorb(i.district i.year)

reghdfe Charc i.firewood_av10

reghdfe hbsfaw dist_firew, absorb(i.camp2)
outreg2 using results.tex, replace
estimates store FAW_Dist

reghdfe Charc l_FAW firewood_FAW dist_firew i.year, absorb(i.camp2)
estimates store Dist

reghdfe Charc l_FAW decreased increased constantarmy constant decreasedarmy increasedarmy s_lrainfall temperature
estimates store aware

reghdfe Charc l_FAW firewood_av10 army_aff, absorb(i.year)
estimates store author

coefplot (FAW_Dist, asequation(FAW_Dist)) ///
|| (Dist, asequation(ist)) ///
|| (aware, asequation(forest)) ///
|| (author, asequation(auth)) ///
, drop(_cons) keep(l_FAW hbsdist firewood_FAW dist_firew increased increasedarmy constant constantarmy decreased decreasedarmy firewood_av10 army_aff) ///
eqlabel(, labsize(small)) msymbol(circle) mcolor(ebblue) ciopts(recast(rcap) lcolor(ebblue)) ///
xline(0, lcolor(red)) ylabel(, labsize(vsmall)) omitted baselevels ///
bylabels("FAW" "Charcoal (1 = Yes)" "Charcoal (1 = Yes)" "Charcoal (1 = Yes)") ///
byopts(compact rows(1) note("p-values shown alongside markers" "*** p<.01, ** p<.05, * p<.1"))

********************************************************************************
* Final Regression and Coping Mechanism on Charcoal
********************************************************************************
reghdfe Charc maizearea_share maize_army, absorb(i.district)
outreg2 using results2, excel replace
reghdfe Charc inter_1 inter_army i.year, absorb(i.district)
outreg2 using results2, excel append
reghdfe Charc grewothercrop grew3_army i.year, absorb(i.district)
outreg2 using results2, excel append
reghdfe Charc mem_left migr_army i.year, absorb(i.district)
outreg2 using results2, excel append
reghdfe Charc piecework pie_army i.year, absorb(i.district)
outreg2 using results2, excel append
reghdfe Charc spray spray_army i.year, absorb(i.district)
outreg2 using results2, excel append

reghdfe Charc inter_1 inter_army grewothercrop grew3_army mem_left migr_army piecework pie_army spray spray_army l_rainfall s_lrainfall ltemperature sqtemperature i.year, absorb(i.district)
outreg2 using results2, excel replace

********************************************************************************
* IV Approach
********************************************************************************
ivreghdfe Charc (maizearea_share maize_FAW = camp_maize maize_army2) l_rainfall s_lrainfall ltemperature sqtemperature, absorb(i.district) first

ivreghdfe Charc (grewothercrop grew3_army = camp_grewothercrop grew_army) l_rainfall s_lrainfall ltemperature sqtemperature, absorb(i.district) first

ivreghdfe Charc (mem_left migr_army2 = piecework pie_army) l_rainfall s_lrainfall ltemperature sqtemperature, absorb(i.district) first

ivreghdfe Charc (l_spray spray_FAW = camp_spray spray_army) l_rainfall s_lrainfall v, absorb(i.district) first

********************************************************************************
* Charcoal and Deforestation Analysis
********************************************************************************
ivreghdfe Deforestion (Charc = l_FAW), absorb(i.HHID) first
outreg2 using results, excel append

reghdfe area_km2_buffer_10 l_FAW l_rainfall s_lrainfall ltemperature sqtemperature grewothercrop mem_left piecework spray i.year, absorb(i.HHID)
ivreghdfe area_km2_buffer_10 (Charc = l_FAW) l_rainfall s_lrainfall ltemperature sqtemperature grewothercrop mem_left piecework spray i.year, absorb(i.HHID)
reghdfe area_km2_buffer_10 Charc l_rainfall s_lrainfall ltemperature sqtemperature grewothercrop mem_left piecework spray i.year, absorb(i.HHID)

reghdfe area_km2_buffer_10 temperature i.year, absorb(i.HHID)
reghdfe temperature area_km2_buffer_10 i.year, absorb(i.HHID)

reghdfe Deforestion l_FAW l_rainfall s_lrainfall cultivown_land hh_head_edu hhsize hh_head_sex temperature sqtemp, absorb(i.camp1)
ivreghdfe Deforestion (Charc = l_FAW) temperature i.year, absorb(i.district) first

********************************************************************************
* Charcoal and Yield IV
********************************************************************************
ivreghdfe Charc (l_FAW = lyield), absorb(i.HHID) first cl(camp1)
ivreghdfe Charc (l_FAW = lyield) l_rainfall s_lrainfall temperature sqtemp i.year, absorb(i.HHID) first cl(camp1)
ivreghdfe Charc (l_FAW = lyield) l_rainfall s_lrainfall temperature sqtemp i.year, absorb(i.HHID) first cl(camp1)
ivreghdfe Charc (l_FAW = lyield) cultivown_land hhsize fert qseed l_rainfall s_lrainfall temperature sqtemp i.year, absorb(i.HHID) first cl(camp1)	

************************************************************
* SECTION: FAW and Charcoal Prices
************************************************************

* Estimate the effect of FAW on charcoal prices using IV
ivreghdfe charc_price (camIV = l_FAW) l_rainfall s_lrainfall temperature sq_temp cam_year i.year, absorb(i.district) first


