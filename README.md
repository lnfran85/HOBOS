# Analysis of the temperature and luminosity gathered by the datalogers HOBO 

With this script I try to analyse the data gathered by the datalogers. I have several datalogers distributed throughout the Portuguese litoral, on places with a great invasion by Acacia longifolia, and where the biocontrol agent Trichilogaster acaciaelongifoliae was released. The objective of this script is to evaluate the question: Are there a increasing of luminosity and temperature inside the Acacia stand (ruled by the defoliation effect of the biocontrol agent) ?
Of course, after the recent release of this biocontrol agent, it is not expectable a huge effect on the canopy density, but it is mandatory check the data to allow us the management of the current dataloggers and the eventually place of new dataloggers

## IMPORTANT REMARKS
When data is gathered from dataloggers, the exportation of the parsed file must follow the next rules (check in HOBOware preferences):
 - The data format follow the DMY style separated by "/". 
 - The column separator is ";"
 - A column with the row number must be included
 - Date and time must be in two separated columns
 - The time format will be as 24h
 - Include serial number of the datalogger


## TO DO

1º CHECK THE FILES      
Some HOBOS were translocated between places. It is expectable that (for example) the HOBO located on SJD with the serial number 23456 were moved to SPM, maintaining (or not) the code and adding more information on its label. CHECK TO AVOID crosserrors

2º FILTER BY PLACE AND CHECK OUTLEIRS      
For each place I will check the values of temperature and luminosity to detect outliers from malfunction of the datalogger. After this checking, all files cleaned will be merged into one single file.

3º SAVING THE FINAL FILE      
I will save a final file with the all data for each site. This file will be frequently updated with new data from the field.
