# Analysis of the temperature and luminosity gathered by the dataloggers HOBO 

With this script I try to analyse the data gathered by the dataloggers. From several dataloggers distributed throughout the Portuguese litoral, on places with a great invasion by Acacia longifolia. The objective of this script is to evaluate the question: Are there a increasing of luminosity and temperature inside the Acacia stand (ruled by the defoliation effect of the biocontrol agent) ?
Of course, after the recent release of this biocontrol agent, it is not expectable a huge effect on the canopy density, but it is mandatory check the data to allow us the management of the current dataloggers and the eventually place of new dataloggers

## IMPORTANT REMARKS
When data is gathered from dataloggers, the exportation of the parsed file must follow the next rules (check in HOBOware preferences):
 - The data format follow the DMY style separated by "/". 
 - The column separator is ";"
 - A column with the row number must be included
 - Date and time must be in two separated columns
 - The time format will be as 24h
 - Include serial number of the datalogger


## DONE

1º FILTER BY PLACE AND CHECK OUTLEIRS      
For each place I will check the values of temperature and luminosity to detect outliers from malfunction of the datalogger. After this checking, all files cleaned will be merged into one single file.
For temperature I replaced the values above 50ºC to NAN values. I have done that, because I discarded the NAN values to estimate the mean temperature by month, but I did not discard the daily associated values of light. Nevertheless, this replacement need to be more accurated, also considering potential light outliers

2º ANALYSIS OF THE DATA
I have done a preliminary analysis of the São Jacinto data, and I obtained some results. By one hand, as expected, the temperature (in average) apparently increases throughout the time; but on the other hand, an unexpected decrease on light have been happened. I am not sure why. Probably, It could be explained by:       

(a) The position of the datalogger varied during the year           
(b) When I estimated the mean, I considered the night values of luminosity (0 lux), so, perhaps, this values could be responsible for this trend... In next tests I will    consider for de mean only values above 0 lux (only daylight period).



**MONTHLY AVERAGES**            
This figure shows the monthly mean of temperature and luminosity for each site. Apparently, the temperature decreased in all sites except for São Jacinto. For luminosity, the pattern is more variable, decreasing from the first years (performance-dependence of the datalogger?); only during the last years we can begin appreciate an apparently increasing in São Jacinto and Seixo, while the rest of sites follow a decreasing trend of luminosity.         

![alt text](https://github.com/lnfran85/HOBOS/blob/785acfcb7aa451bef328d4fdd15ea89174ca427f/monthly_means.png "Monthly trends")

**MONTHLY RANGES**       
![alt text](https://github.com/lnfran85/HOBOS/blob/785acfcb7aa451bef328d4fdd15ea89174ca427f/monthly_average_daily_range.png "Monthly ranges")

**MAX MIN MONTHLY AVERAGES**
![alt text](https://github.com/lnfran85/HOBOS/blob/785acfcb7aa451bef328d4fdd15ea89174ca427f/monthly_average_maxmin.png "Monthly maxmin averages")
