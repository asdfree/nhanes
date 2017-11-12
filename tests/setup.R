if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

this_sample_break <- Sys.getenv( "this_sample_break" )

library(lodown)

nhanes_cat <-
	get_catalog( "nhanes" ,
		output_dir = file.path( getwd() ) )

record_categories <- ceiling( seq( nrow( nhanes_cat ) ) / ceiling( nrow( nhanes_cat ) / 5 ) )

nhanes_cat <- unique( rbind( nhanes_cat[ record_categories == this_sample_break , ] , nhanes_cat[ nhanes_cat$years == "2015-2016" , ] ) )

lodown( "nhanes" , nhanes_cat )
