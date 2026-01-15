# doctor, dentist, labs
# mobile examination
#vanlife interviews
library(haven)

nhanes_2015_2016_demo_url <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/DEMO_I.xpt"

nhanes_2017_2018_demo_url <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/DEMO_J.xpt"

nhanes_2015_2016_tchol_url <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/TCHOL_I.xpt"
	
nhanes_2017_2018_tchol_url <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/TCHOL_J.xpt"
	

nhanes_2015_2016_demo_tbl <- read_xpt( nhanes_2015_2016_demo_url )
nhanes_2017_2018_demo_tbl <- read_xpt( nhanes_2017_2018_demo_url )
nhanes_2015_2016_tchol_tbl <- read_xpt( nhanes_2015_2016_tchol_url )
nhanes_2017_2018_tchol_tbl <- read_xpt( nhanes_2017_2018_tchol_url )

nhanes_2015_2016_demo_df <- data.frame( nhanes_2015_2016_demo_tbl )
nhanes_2017_2018_demo_df <- data.frame( nhanes_2017_2018_demo_tbl )
nhanes_2015_2016_tchol_df <- data.frame( nhanes_2015_2016_tchol_tbl )
nhanes_2017_2018_tchol_df <- data.frame( nhanes_2017_2018_tchol_tbl )
demo_vars <-
	c( 
		 # unique person identifier (merge variable)
		"SEQN" ,

		# the two-year interviewed + MEC examined weight
		"WTMEC2YR" , 	
		# note that this is a special weight for only
		# individuals who took the mobile examination center (MEC) exam
		# there is one other weight available - WTINT2YR - 
		# that should be used when MEC variables are not part of the analysis
		
		# interviewed only or interviewed + MEC
		"RIDSTATR" ,
		
		# primary sampling unit varaible, used in complex design
		"SDMVPSU" ,
		
		# strata variable, used in complex design
		"SDMVSTRA" ,
		
		# race / ethnicity
		"RIDRETH3" ,

		# age
		"RIDAGEYR" ,
		
		# gender
		"RIAGENDR" ,
		
		# pregnant at interview
		"RIDEXPRG"
	)

nhanes_2015_2018_demo_df <-
	rbind(
		nhanes_2015_2016_demo_df[ , demo_vars ] ,
		nhanes_2017_2018_demo_df[ , demo_vars ]
	)
	

tchol_vars <-
	c( 
		# unique person identifier (merge variable)
		"SEQN" ,
		
		# laboratory total cholesterol variable
		# https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/TCHOL_J.htm
		"LBXTC" 		

	)

nhanes_2015_2018_tchol_df <-
	rbind(
		nhanes_2015_2016_tchol_df[ , tchol_vars ] ,
		nhanes_2017_2018_tchol_df[ , tchol_vars ]
	)
nhanes_full_df <-
	merge(
		nhanes_2015_2018_demo_df ,
		nhanes_2015_2018_tchol_df ,
		all = TRUE
	)

names( nhanes_full_df ) <- tolower( names( nhanes_full_df ) )
	
nhanes_df <- subset( nhanes_full_df , ridstatr %in% 2 )
nhanes_df[ , 'wtmec4yr' ] <- nhanes_df[ , 'wtmec2yr' ] / 2
# nhanes_fn <- file.path( path.expand( "~" ) , "NHANES" , "this_file.rds" )
# saveRDS( nhanes_df , file = nhanes_fn , compress = FALSE )
# nhanes_df <- readRDS( nhanes_fn )
library(survey)

nhanes_design <- 
	svydesign(
		id = ~ sdmvpsu , 
		strata = ~ sdmvstra ,
		nest = TRUE ,
		weights = ~ wtmec4yr ,
		data = nhanes_df
	)

nhanes_design <-

	update(

		nhanes_design ,

		one = 1 ,

		# define high total cholesterol as 1 if mg/dL is at or above 240 and zero otherwise.
		hi_tchol = ifelse( lbxtc >= 240 , 1 , 0 ) ,
		
		gender = factor( riagendr , levels = 1:2 , labels = c( 'male' , 'female' ) ) ,
		
		age_categories =
			factor(	
				1 + findInterval( ridageyr , c( 20 , 40 , 60 ) ) , 
				levels = 1:4 , 
				labels = c( "0-19" , "20-39" , "40-59" , "60+" )
			) ,

		# recode the ridreth3 variable as:
		# mexican american and other hispanic -> 4
		# non-hispanic white -> 1
		# non-hispanic black -> 2
		# non-hispanic asian -> 3
		# other race including multi-racial -> 5
		race_ethnicity =
			factor( 
				c( 4 , 4 , 1 , 2 , NA , 3 , 5 )[ ridreth3 ] ,
				levels = 1:5 ,
				labels = c( 'nh white' , 'nh black' , 'nh asian' , 'hispanic' , 'other' )
			) ,
			
		pregnant_at_interview = 
			ifelse( ridexprg %in% 1:2 , as.numeric( ridexprg == 1 ) , NA )
	)
sum( weights( nhanes_design , "sampling" ) != 0 )

svyby( ~ one , ~ race_ethnicity , nhanes_design , unwtd.count )
svytotal( ~ one , nhanes_design )

svyby( ~ one , ~ race_ethnicity , nhanes_design , svytotal )
svymean( ~ lbxtc , nhanes_design , na.rm = TRUE )

svyby( ~ lbxtc , ~ race_ethnicity , nhanes_design , svymean , na.rm = TRUE )
svymean( ~ riagendr , nhanes_design )

svyby( ~ riagendr , ~ race_ethnicity , nhanes_design , svymean )
svytotal( ~ lbxtc , nhanes_design , na.rm = TRUE )

svyby( ~ lbxtc , ~ race_ethnicity , nhanes_design , svytotal , na.rm = TRUE )
svytotal( ~ riagendr , nhanes_design )

svyby( ~ riagendr , ~ race_ethnicity , nhanes_design , svytotal )
svyquantile( ~ lbxtc , nhanes_design , 0.5 , na.rm = TRUE )

svyby( 
	~ lbxtc , 
	~ race_ethnicity , 
	nhanes_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE , na.rm = TRUE
)
svyratio( 
	numerator = ~ lbxtc , 
	denominator = ~ ridageyr , 
	nhanes_design ,
	na.rm = TRUE
)
sub_nhanes_design <- subset( nhanes_design , age_categories == "60+" )
svymean( ~ lbxtc , sub_nhanes_design , na.rm = TRUE )
this_result <- svymean( ~ lbxtc , nhanes_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ lbxtc , 
		~ race_ethnicity , 
		nhanes_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( nhanes_design )
svyvar( ~ lbxtc , nhanes_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ lbxtc , nhanes_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ lbxtc , nhanes_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ pregnant_at_interview , nhanes_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( lbxtc ~ pregnant_at_interview , nhanes_design )
svychisq( 
	~ pregnant_at_interview + riagendr , 
	nhanes_design 
)
glm_result <- 
	svyglm( 
		lbxtc ~ pregnant_at_interview + riagendr , 
		nhanes_design 
	)

summary( glm_result )
crude_overall <-
	svymean( ~ hi_tchol , subset( nhanes_design , ridageyr >= 20 ) , na.rm = TRUE )

stopifnot( round( coef( crude_overall ) , 3 ) == 0.115 )

crude_by_gender <-
	svyby( 
		~ hi_tchol , 
		~ gender , 
		subset( nhanes_design , ridageyr >= 20 ) , 
		svymean , 
		na.rm = TRUE 
	)
	
stopifnot( round( coef( crude_by_gender )[ 1 ] , 3 ) == 0.103 )
stopifnot( round( coef( crude_by_gender )[ 2 ] , 3 ) == 0.126 )

crude_by_age <-
	svyby(
		~ hi_tchol , 
		~ age_categories , 
		subset( nhanes_design , ridageyr >= 20 ) , 
		svymean , 
		na.rm = TRUE 
	)
	
stopifnot( round( coef( crude_by_age )[ 1 ] , 3 ) == 0.075 )
stopifnot( round( coef( crude_by_age )[ 2 ] , 3 ) == 0.157 )
stopifnot( round( coef( crude_by_age )[ 3 ] , 3 ) == 0.114 )

stopifnot( round( SE( crude_by_age )[ 1 ] , 3 ) == 0.005 )
stopifnot( round( SE( crude_by_age )[ 2 ] , 3 ) == 0.011 )
stopifnot( round( SE( crude_by_age )[ 3 ] , 3 ) == 0.008 )
pop_by_age <- 
	data.frame( 
		age_categories = c( "0-19" , "20-39" , "40-59" , "60+" ) ,
		Freq = c( 78782657 , 77670618 , 72816615 , 45363752 ) 
	) 	
nhanes_age_adjusted <-
	postStratify( 
		subset( nhanes_design , !is.na( hi_tchol ) ) , 
		~ age_categories , 
		pop_by_age 
	)
results_overall <-
	svymean( ~ hi_tchol , subset( nhanes_age_adjusted , ridageyr >= 20 ) , na.rm = TRUE )

stopifnot( round( coef( results_overall ) , 3 ) == 0.114 )

stopifnot( round( SE( results_overall ) , 3 ) == 0.006 )
nhanes_by_gender <-
	svystandardize(
		nhanes_design , 
		by = ~ age_categories , 		# stratification variable
		over = ~ gender ,				# break out variable
		population = pop_by_age , 		# data.frame containing census populations
		excluding.missing = ~ hi_tchol 	# analysis variable of interest
	)

results_by_gender <-
	svyby( 
		~ hi_tchol , 
		~ gender , 
		subset( nhanes_by_gender , ridageyr >= 20 ) ,
		svymean , 
		na.rm=TRUE
	)

stopifnot( round( coef( results_by_gender )[ 1 ] , 3 ) == 0.105 )
stopifnot( round( coef( results_by_gender )[ 2 ] , 3 ) == 0.121 )

stopifnot( round( SE( results_by_gender )[ 1 ] , 3 ) == 0.007 )
stopifnot( round( SE( results_by_gender )[ 2 ] , 3 ) == 0.008 )
nhanes_by_race <-
	svystandardize(
		nhanes_design , 
		by = ~ age_categories , 		# stratification variable
		over = ~ race_ethnicity ,		# break out variable
		population = pop_by_age , 		# data.frame containing census populations
		excluding.missing = ~ hi_tchol 	# analysis variable of interest
	)

results_by_race_ethnicity <-
	svyby( 
		~ hi_tchol , 
		~ race_ethnicity , 
		design = subset( nhanes_by_race , ridageyr >= 20 ) ,
		svymean , 
		na.rm=TRUE
	)

stopifnot( round( coef( results_by_race_ethnicity )[ 1 ] , 3 ) == 0.117 )
stopifnot( round( coef( results_by_race_ethnicity )[ 2 ] , 3 ) == 0.100 )
stopifnot( round( coef( results_by_race_ethnicity )[ 3 ] , 3 ) == 0.116 )
stopifnot( round( coef( results_by_race_ethnicity )[ 4 ] , 3 ) == 0.109 )

stopifnot( round( SE( results_by_race_ethnicity )[ 1 ] , 3 ) == 0.007 )
stopifnot( round( SE( results_by_race_ethnicity )[ 2 ] , 3 ) == 0.009 )
stopifnot( round( SE( results_by_race_ethnicity )[ 3 ] , 3 ) == 0.011 )	
stopifnot( round( SE( results_by_race_ethnicity )[ 4 ] , 3 ) == 0.009 )	
library(srvyr)
nhanes_srvyr_design <- as_survey( nhanes_design )
nhanes_srvyr_design %>%
	summarize( mean = survey_mean( lbxtc , na.rm = TRUE ) )

nhanes_srvyr_design %>%
	group_by( race_ethnicity ) %>%
	summarize( mean = survey_mean( lbxtc , na.rm = TRUE ) )
