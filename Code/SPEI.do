import delimited "C:\Users\hadunka2\Downloads\SPEI_Precip.csv", clear 

twoway (line precip year) (line spei year), ///
    legend(label(1 "Precipitation") label(2 "SPEI")) 
	
import delimited "C:\Users\hadunka2\Downloads\zambia_spei_monthly_2010_2023.csv", clear 

gen year = substr(date, 1, 4)

destring year,replace

collapse (median) value , by(year)

save "C:\Users\hadunka2\Box\SPEI\SPEI_2010_2022", replace	
	
import delimited "C:\Users\hadunka2\Downloads\zambia_spei_monthly_2000_2010.csv", clear 

gen year = substr(date, 1, 4)

destring year,replace

collapse (median) value , by(year)

save "C:\Users\hadunka2\Box\SPEI\SPEI_2000_2010", replace




append using "C:\Users\hadunka2\Box\SPEI\SPEI_2010_2022"

merge 1:1 year  using "C:\Users\hadunka2\Box\SPEI\SPEI"

rename value median_SPEI
rename spei average_SPEI


twoway (line median_SPEI year) (line average_SPEI year), ///
    legend(label(1 "Median SPEI") label(2 "Average SPEI")) 



