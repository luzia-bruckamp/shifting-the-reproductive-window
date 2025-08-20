# Replication package: Shifting the reproductive window
This repository contains replication files for "Shifting the reproductive window: The contribution of ART and egg donation to fertility rates in the UK" by Luzia Bruckamp and Ester Lazzari, published in Population Studies. All code to replicate the analyses from the paper is made available as well as detailed instructions for how to access the data.
## Data requirements
In order to run our code, you will need data from the Human Fertilisation and Embryology Authority (HFEA) and data from the Human Fertility Database (HFD).
### HFEA data
The data from the HFEA are usually accessible at https://www.hfea.gov.uk/about-us/data-research/. At the time of writing, the data are under review and not downloadable directly from the website. In this case, users should contact register.research@hfea.gov.uk to access the data sets. Because the data are being reviewed, there is some possibility that there will be some slight changes that might lead to discrepancies with our results. When contacting the HFEA, you can specify that you would like the version of the data that used to be available on the website, which we downloaded in November 2023.
The HFEA data consist of 7 data sets in csv format, for the years 1991-1994, 1995-1999, 2000-2004, 2005-2009, 2010-2014, 2015-2016, and 2017-2018. For compatibility with the cleaning code, you should save these data sets as:
- hfea_1991_1994.csv
- hfea_1995_1999.csv
- hfea_2000_2004.csv
- hfea_2005_2009.csv
- hfea_2010_2014.csv
- hfea_2015_2016.csv
- hfea_2017_2018.csv

The cleaning code selects the relevant variables from all data sets, cleans them, and merges all the data sets together. The data sets from 1991 to 2016 are all in the same format and are handled by the same cleaning function. The last data set is in a slightly different format and is cleaned separately.
### HFD data
The data from the HFD are accessible at https://www.humanfertility.org. In order to access the data, you will have to register with the HFD if you do not have an account yet. Once you are registered, you are able to log in and to download any data.
At the top of the page, go to “Data” and then “Zipped Data Files”. Scroll down to find the UK and download the zipped file for all of the UK. The downloaded folder should have the name “GBR_NP”. If it doesn’t, then you will need to either change the folder name or change the name in the R file.
## Setup instructions
You should put all the HFEA data sets and the folder with all the HFD data sets into the same folder. You will then set this folder as your working directory in the cleaning and analysis code so that they can be read in and the cleaned data can be exported there. All three R files will prompt you to change the working directory at the top of the script.
You need to first run the two cleaning scripts, “cleaning_hfd.R” and “cleaning_hfea.R” before you can run the analysis script, “analysis.R”.
## Notes
One last important note is that for two reasons, you might not be able to generate exactly the same results as us. First, the HFD data is periodically updated, which usually means that some decimal points change. Second, we use a random number generator when cleaning the HFEA data, in order to assign a birth year when it is missing. Even though we set the seed for reproducibility, this could generate slightly different results if you are using a different version of R.
If you have any questions about the code or data, please contact Luzia Bruckamp. Her email address is l.bruckamp[at]lse.ac.uk.
