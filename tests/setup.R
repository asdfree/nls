if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

options("lodown.cachaca.savecache"=FALSE)

library(lodown)
lodown( "nls" , output_dir = file.path( getwd() ) )
library(lodown)
# examine all available NLS microdata files
nls_cat <-
	get_catalog( "nls" ,
		output_dir = file.path( getwd() ) )

# 2015 only
nls_cat <- subset( nls_cat , study_name == 'NLS Youth 1997 (NLSY97)' )
# download the microdata to your local computer


library(survey)

nlsy_files <- 
	list.files(
		file.path( getwd() ) ,
		full.names = TRUE ,
		recursive = TRUE
	)

# read in the R loading script
nlsy97_r <- 
	readLines( nlsy_files[ grepl( "nlsy97(.*)R$" , basename( nlsy_files ) ) ] )

	
# extract the column names from the R script
first_line <- grep( "names(new_data) <-" , nlsy97_r , fixed = TRUE )
close_parentheses <- grep( ")" , nlsy97_r , fixed = TRUE )
last_line <- min( close_parentheses[ close_parentheses > first_line ] )
column_names_lines <- nlsy97_r[ seq( first_line , last_line ) ]
column_names_lines <- gsub( 'names(new_data) <-' , 'column_names <-' , column_names_lines , fixed = TRUE )
eval( parse( text = column_names_lines ) )

# identify the .dat file
nlsy97_dat <- nlsy_files[ grepl( "nlsy97(.*)dat$" , basename( nlsy_files ) ) ]

# choose which columns to import
columns_to_import <-
	c( 'T5206900' , 'R9829600' , 'R0536300' , 'Z9061800' , 'T6657200' , 'R1205300' )

	
# readr::read_delim() columns must match their order in the csv file
columns_to_import <-
	columns_to_import[ order( match( columns_to_import , column_names ) ) ]

	
# confirm all column names are available
stopifnot( all( columns_to_import %in% column_names ) )

nls_variables_df <- 
	data.frame( 
		readr::read_delim( 
			nlsy97_dat , 
			col_names = columns_to_import , 
			col_types = 
				paste0( 
					ifelse( column_names %in% columns_to_import , 'n' , '_' ) , 
					collapse = "" 
				) ,
			delim = ' '
		) 
	)

# cluster and strata variables
nls_psustr_df <-
	readRDS( grep( "strpsu\.rds$" , nlsy_files , value = TRUE ) )
	
# you can read more about longitudinal weights here
# http://www.nlsinfo.org/weights

# the lodown:::get_nlsy_weights function returns a data.frame object
# containing the unique person identifiers and also a column of weights.

# this makes it easy to choose the correct longitudinal weight for whatever analysis you're trying to do.

# view which points-in-time are available for a particular study
# lodown:::get_nlsy_selections( "nlsy97" )

# download weights for respondents in 1997
w97 <- lodown:::nls_get_weights( "nlsy97" , 'YES' , 'SURV1997' )

# download weights for respondents who were in any of the 1997, 2002, or 2007 surveys
# w <- lodown:::nls_get_weights( "nlsy97" , 'YES' , c( 'SURV1997' , 'SURV2002' , 'SURV2007' ) )

# download weights for respondents who were in all of the 1997, 2002, and 2007 surveys
# w <- lodown:::nls_get_weights( "nlsy97" , 'NO' , c( 'SURV1997' , 'SURV2002' , 'SURV2007' ) )

# download weights for respondents who are in all available surveys
# w <- lodown:::nls_get_weights( "nlsy97" , "NO" , lodown:::nls_get_selections( "nlsy97" ) )

# save those weights into an data.frame object called `w97`
nls_survey_df <- merge( nls_psustr_df , w97 )

nls_df <- merge( nls_variables_df , nls_survey_df )
	
nls_design <- 
	svydesign( 
		~ R1489800 , 
		strata = ~ R1489700 , 
		data = x ,
		weights = ~ weight ,
		nest = TRUE
	)
nls_design <- 
	update( 
		nls_design , 
		q2 = q2 ,
		never_rarely_wore_bike_helmet = as.numeric( qn8 == 1 ) ,
		ever_smoked_marijuana = as.numeric( qn47 == 1 ) ,
		ever_tried_to_quit_cigarettes = as.numeric( q36 > 2 ) ,
		smoked_cigarettes_past_year = as.numeric( q36 > 1 )
	)
sum( weights( nls_design , "sampling" ) != 0 )

svyby( ~ one , ~ ever_smoked_marijuana , nls_design , unwtd.count )
svytotal( ~ one , nls_design )

svyby( ~ one , ~ ever_smoked_marijuana , nls_design , svytotal )
svymean( ~ bmipct , nls_design , na.rm = TRUE )

svyby( ~ bmipct , ~ ever_smoked_marijuana , nls_design , svymean , na.rm = TRUE )
svymean( ~ q2 , nls_design , na.rm = TRUE )

svyby( ~ q2 , ~ ever_smoked_marijuana , nls_design , svymean , na.rm = TRUE )
svytotal( ~ bmipct , nls_design , na.rm = TRUE )

svyby( ~ bmipct , ~ ever_smoked_marijuana , nls_design , svytotal , na.rm = TRUE )
svytotal( ~ q2 , nls_design , na.rm = TRUE )

svyby( ~ q2 , ~ ever_smoked_marijuana , nls_design , svytotal , na.rm = TRUE )
svyquantile( ~ bmipct , nls_design , 0.5 , na.rm = TRUE )

svyby( 
	~ bmipct , 
	~ ever_smoked_marijuana , 
	nls_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ ever_tried_to_quit_cigarettes , 
	denominator = ~ smoked_cigarettes_past_year , 
	nls_design ,
	na.rm = TRUE
)
sub_nls_design <- subset( nls_design , qn41 == 1 )
svymean( ~ bmipct , sub_nls_design , na.rm = TRUE )
this_result <- svymean( ~ bmipct , nls_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ bmipct , 
		~ ever_smoked_marijuana , 
		nls_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( nls_design )
svyvar( ~ bmipct , nls_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ bmipct , nls_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ bmipct , nls_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ never_rarely_wore_bike_helmet , nls_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( bmipct ~ never_rarely_wore_bike_helmet , nls_design )
svychisq( 
	~ never_rarely_wore_bike_helmet + q2 , 
	nls_design 
)
glm_result <- 
	svyglm( 
		bmipct ~ never_rarely_wore_bike_helmet + q2 , 
		nls_design 
	)

summary( glm_result )
library(srvyr)
nls_srvyr_design <- as_survey( nls_design )
nls_srvyr_design %>%
	summarize( mean = survey_mean( bmipct , na.rm = TRUE ) )

nls_srvyr_design %>%
	group_by( ever_smoked_marijuana ) %>%
	summarize( mean = survey_mean( bmipct , na.rm = TRUE ) )

unwtd.count( ~ never_rarely_wore_bike_helmet , yrbss_design )

svytotal( ~ one , subset( yrbss_design , !is.na( never_rarely_wore_bike_helmet ) ) )
 
svymean( ~ never_rarely_wore_bike_helmet , yrbss_design , na.rm = TRUE )

svyciprop( ~ never_rarely_wore_bike_helmet , yrbss_design , na.rm = TRUE , method = "beta" )

