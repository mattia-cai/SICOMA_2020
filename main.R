rm( list = ls() )

# Main SICOMA script (started in September 2020)
# Trying to use Git for version control


# TRY TRY TRY


##########################
### Auxiliary datasets ###
##########################

# Structural Business Statistics (2012-2016 data from file downloaded in Nov 2018)
source( "code/SBS_data.R" )         # Read from bulk dataset

# Input-Output (IO) system
source( "code/IO_EU28_2014.R" )

# A64 gross output data from the national accounts
source( "code/nama10_a64.R" )       # national accounts from eurostat bulk download
source( "code/grossOutputA64.R" )   # extract gross output and aggregate for EU28

# Markup (MUP) and gross output (GO) data from Ecfin file
source( "code/EcfinMupGo.R" )

# Make the data from DG COMP usable :)
source( "code/cleanCompData.R" )

# Count how many 4-digit aggregates in each NACE2 industry (needed for imputations in industries out of SBS scope)
source( "code/count_aggregates.R" )



################################
### Competition case dataset ###
################################

# Merge the various datasets
source( "code/full_dataset.R" )

# Estimate GO4 for the relevant sectors
source( "code/go4_calculations.R" )

# Logistic deterrence
source( "code/logistic_deterrence.R" )

# Avoided price increase assumptions + some dataset formatting
source( "code/compCases.R" )

# # In earlier versions, all these tasks were carried out in a single file
# source( "code/compCaseData.R" )



############################
### Descriptive analysis ###
############################

# # Main case descriptives (legacy version)
# source( "code/main_case_des.R" )

# Total affected market size by type (merger/cartel) and year
source( "code/mkt_by_type_plot.R" )  # Figure III-1 in 2019 FR
source( "code/cases_by_type.R" )       # Same as above, but includes case count as well
source( "code/mkt_by_type_table.R" ) # Table III-1 in 2019 FR  

# Affected market size by industry and type of case
source( "code/cases_by_industry.R" ) # Figures III-2/3 in 2019 FR
source( "code/cases_by_industry_table.R" )

# Average case duration
source( "code/average_duration.R" ) # Table III-2 in 2019 FR

# Distribution of mkt/GO4
source( "code/mkt_&_go4_table.R" )  # Table III-3 in FR 2019
source( "code/mkt_&_go4_histograms.R" ) # Figures III-4 and III-6 

# This one is only concerned with mkt (merger vs cartels)
source( "code/histogram_cartels_vs_mergers.R" )

# Deterrence multipliers
source( "code/deterrence_charts.R" )  # Table III-3 in FR 2019



################
### Modeling ###
################

# Key sector analysis (standardized price shock)
source( "code/key_sectors.R" )

# Compute within- and cross-industry price effects
# This is how it was done in the FR19 report.
# source( "code/priceImpacts.R" )

# It is apparent that we will need to try out several versions of the analysis. This time I will try to prepare a separate script for each analysis. Also, there is no interest for results without deterrence, so I will not produce them this time.

# Year-by-year results WITHOUT duration
source( "code/price_impacts_no_duration.R" )

# Year-by-year results WITH duration
source( "code/price_impacts_duration.R" )

# Economy-wide price impacts -- duration vs. no duration
source( "code/economy_wide.R" )

# Industry-level price changes - CHARTS
source( "code/price_by_industry_no_duration.R" ) # Without
source( "code/price_by_industry_duration.R" )    # With duration
source( "code/price_by_industry.R" )             # Side by side

# Average shock 2012-2019 (outdated, I believe)
# source( "code/average_shock.R" ) # With versus without duration

# A tentative QUEST-like permanent shock
source( "code/permanent_shock_example.R" ) # Single-industry example for PPT
source( "code/price_impacts_permanent.R" ) # Actual calculations
source( "code/prmnt_vs_avg.R" )             # Side by side


# # Compute within- and cross-industry price effects for a couple of significant cases
# source( "code/singleDecisions.R" )  # Could be probably eliminated

# Price level changes: main tables
source( "code/tables_like_book_chapter.R" )

# Numerical example for report
source( "code/numerical_example.R" )
