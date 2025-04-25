***	Protensia Hadunka***
***HICPS Rainfall 2015-2019
*Rainfall_2016/17
clear all
set more off
/*import excel "C:\Users\hadunka2\Downloads\CHIRPS_Nick\CHIRPS_zambia.xlsx", sheet("CHIRPS_zambia") firstrow

foreach v of varlist B-AJB 
{
   local x : variable label `v'
   rename `v' id_`x'
}

rename A date

reshape long id_, i(date) j(rainfall)*/

*Transponsed sheet
/*import excel "C:\Users\hadunka2\Desktop\CHIRPS_zambia_Final.xlsx", sheet("Rainfall_Final")
rename A HHID

foreach var of varlist B-QO {
destring `var', replace force*/

*import excel "C:\Users\hadunka2\Box\Rainfall_HICPS\CHIRPS_zambia_Final.xlsx", sheet("Rainfall_Final") firstrow clear
import excel "C:\Users\hadunka2\Box\Raw rainfall\Raw Rainfall.xlsx", sheet("Sheet1") firstrow

*Changing all the string variables to numeric
destring, replace force

egen totalrain2015 = rowtotal(_1_2015-NB)
egen totalrain2016 = rowtotal(_1_2016-ABD)
egen totalrain2017 = rowtotal(_1_2017-APE)
egen totalrain2018 = rowtotal(_1_2018-BDF)
egen totalrain2019 = rowtotal(_1_2019-BRG)

keep HHID totalrain2015 totalrain2016 totalrain2017 totalrain2018 totalrain2019
duplicates drop

reshape long totalrain, i(HHID) j(year)

*save "C:\Users\hadunka2\Box Sync\Patrese\Rainfall",replace
save "C:\Users\hadunka2\Box\Raw rainfall\Total_Rainfall",replace
