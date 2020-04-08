### PRELIM

print( "Identifying live cells" )

time_start <- proc.time()[3]

### FUNCTIONS

library(doParallel)

do_rpt <- function(r,rmd) {
  
  require(rmarkdown)
  
  tf <- tempfile()
  dir.create(tf)
  
  result <- try( rmarkdown::render(input=rmd,
                                   output_file=r$output_file,
								   output_dir=r$output_dir,
                                   intermediates_dir=tf,
                                   params=r$params,
                                   quiet=TRUE) )
  unlink(tf)
  
  ifelse( class( result == "try-error" ), r$output_file, NA )
}	  


par_render <- function( fn, gate_list, eval = TRUE, .eval_all = eval_all, reduce = TRUE, .reduce_none = reduce_none ){
    print(fn)
	if( !( eval | .eval_all ) ){
	   message( paste0( "skipped ", fn, "." ) )
	   return( invisible( TRUE ) )
	}
	if( .reduce_none ) reduce <- FALSE
	params_list <- list( reduce = reduce, 
                         fn = fn, 
					     gate_list = gate_list )
	output_file <- stringr::str_replace( fn, "2.1GB_singlets", "live" )
	output_dir = '/home/rdxmig002/phd/CATALYST/vignettes/live_cells/html/'
	
    rmarkdown::render( input = 'live_manual.Rmd', 
	                   output_file = output_file,
	                   params = params_list, 
                       output_dir = output_dir, 
                       quiet = TRUE ) 
  
}

catch_render <- function(...) tryCatch( par_render( ... ) )

### RUN IN SERIAL

eval_all <- FALSE
reduce_none <- TRUE

catch_render( fn = '01_0782 day0 and day180 06sep2017_normalized-2.1GB_singlets', 
            gate_list = list( list( c( 2.2, 2 ), c( 4.6, 5.6 ) ) ), 
            eval = FALSE, 
            reduce = FALSE ) # checked on full, fine. # f
			
catch_render( fn = '01_0673 d0 09_0462 d0 _1_1-2.1GB_singlets', 
gate_list = list( list( c( 3.0, 4.1 ), c( 3.7, 6.7 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine, sm2
			
catch_render( fn = '01-0993 D0 AND 07-1147 DAY0_normalized-2.1GB_singlets', 
gate_list = list( 3.9, list( c( 2.7, 0.8 ), c( 4.6, 4.9 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine, sm2

catch_render( fn = '02-0185 DO AND 09-0296 D0 29SEP2017_normalized-2.1GB_singlets', 
gate_list = list( 3.7, list( c( 2.3, 3.3 ), c( 4.1, 6.0 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # looks good on full. m2

#03_0442 D0 and 09_0248 D0_normalized-2.1GB_singlets
catch_render( fn = '03_0442 D0 and 09_0248 D0_normalized-2.1GB_singlets', 
 gate_list = list( 4.1, list( c( 2.4, 3.3 ), c( 6.4, 5.7 ) ) ), 
 eval = FALSE, 
 reduce = TRUE ) # looks good on full. m2

catch_render( fn = '03_0448 D0 and 09_0581 D0_normalized-2.1GB_singlets', 
gate_list = list( 3.9, list( c( 2.8, 3 ), c( 4.6, 5.7 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine. sm2

catch_render( fn = '03-0697 D0 and 03-0545 D0_normalized-2.1GB_singlets', 
gate_list = list( 2.9, list( c( 0.6, 2 ), c( 4.5, 5.7 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine. sm2

catch_render( fn = '04_0152 D0 AND 04_0152 D180_normalized-2.1GB_singlets', 
gate_list = list( 3.4, list( c( 3.1, 3.4 ), c( 4.0, 5.8 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine. sm2

catch_render( fn = '04_0152 d360 and d540_3_1-2.1GB_singlets', 
gate_list = list( 3.3, list( c( 1.2, 2.8 ), c( 4.9, 5.8 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine. sm2

# 9 so far

####

catch_render( fn = '04_0238 d0and d180-2_normalized-2.1GB_singlets', 
gate_list = list( 4.3, list( c( 3, 3.9 ), c( 6.3, 6.7 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine. sm2

catch_render( fn = '04_0511 D0 AND 04_1119 D0_normalized-2.1GB_singlets', 
gate_list = list( 3.9, list( c( 1.8, 3.2 ), c( 6.4, 6 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine. sm2

#### 11

catch_render( fn = '04_0695 do and d180_4_1-2.1GB_singlets', 
gate_list = list( 3.3, list( c( 1.3, 3.3 ), c( 5.5, 5.9 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine. sm2

catch_render( fn = '04_0718 d360 and 04_0886 d360_normalized-2.1GB_singlets', 
gate_list = list( 4.1, list( c( 2.3, 2.9 ), c( 6.3, 5.6 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine. sm2

catch_render( fn = '04_0741_d0 and d180_normalized-2.1GB_singlets', 
gate_list = list( 4.2, list( c( 3.2, 3.2 ), c( 5.8, 5.9 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine. sm2

#### 14

catch_render( fn = '04_0779 D0 AND 07_0369 D0_normalized-2.1GB_singlets', 
gate_list = list( 3.9, list( c( 2.2, 2.4 ), c( 4.7, 5.4 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine. sm2

catch_render( fn = '04_0811D180 AND 07_1153 D180_normalized-2.1GB_singlets', 
gate_list = list( 3.7, list( c( 2.1, 1.4 ), c( 6.2, 6.9 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine. sm2

catch_render( fn = '04_0843 D0 AND 04_0843 D180_normalized-2.1GB_singlets', 
gate_list = list( 4.1, list( c( 2.4, 3.4 ), c( 6, 5.9 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine. sm2

#### 17

catch_render( fn = '04_0843 d360 and 04_0843 d540_normalized-2.1GB_singlets', 
gate_list = list( 3.9, list( c( 2.2, 2.8 ), c( 5.6, 5.4 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine. sm2

catch_render( fn = '04_0910 d0 and 07_1132 d0_normalized-2.1GB_singlets', 
gate_list = list( 3.4, list( c( 2.7, 3.3 ), c( 3.4, 5.7 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine. sm2

catch_render( fn = '04_0910 D540 AND 07_1132 D540_normalized-2.1GB_singlets', 
gate_list = list( 3.9, list( c( 2.1, 2.2 ), c( 5.8, 5.3 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine. sm2

#### 20

catch_render( fn = '04_1020 D360 and 04_1020 D540_normalized-2.1GB_singlets', 
gate_list = list( 4, list( c( 2.2, 3.3 ), c( 5.3, 5.8 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine. sm2

catch_render( fn = '04_1020D0 and 04_1020 D180_normalized_singlets', 
gate_list = list( 4.2, list( c( 2.45, 3.2 ), c( 5.6, 6.3 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine. sm2

catch_render( fn = '04_1056 d0 and 04_0979 d0_normalized-2.1GB_singlets', 
gate_list = list( 3.5 ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine. sm2

#### 23

catch_render( fn = '04_1056 d180 and 04_0979 d180_normalized-2.1GB_singlets', 
gate_list = list( 3.5, list( c( 2.5, 3.5 ), c( 5.8, 6.3 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine. sm2

catch_render( fn = '04_1104 d0 and d180_2_1-2.1GB_singlets', 
gate_list = list( 3.8, list( c( 3, 3.6 ), c( 5, 5.9 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine. sm2

catch_render( fn = '04_1241 D0 AND 07_0119 D0_normalized-2.1GB_singlets', 
gate_list = list( 4.1, list( c( 2.8, 2.8 ), c( 5.3, 6.8 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # looks good on full. m2

#### 26 (previously)

catch_render( fn = '04_1241 d180 and 07_0119 d180_normalized_singlets', 
gate_list = list( list( c( 3.1, 1.9 ), c( 6.2, 5.3 ) ) ), 
eval = FALSE, 
reduce = FALSE ) #checked on full, fine. sm2

#### 27

catch_render( fn = '04-0718 D0 AND 04-0886 D0_normalized-2.1GB_singlets', 
gate_list = list( 3.8, list( c( 1.9, 2.6 ), c( 6, 5.6 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine. sm2

catch_render( fn = '04-0718 D180 AND 04-0886 D180_normalized-2.1GB_singlets', 
gate_list = list( 4.2, list( c( 3, 3.3 ), c( 5.6, 5.6 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine. m2

#### 29

catch_render( fn = '04-0910 D360 AND 07-1132 D360_normalized-2.1GB_singlets', 
gate_list = list( 4.3, list( c( 1.7, 1.5 ), c( 6.5, 5 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine. sm2

catch_render( fn = '04-1056 D360 PLUS 04-0979 D360_normalized-2.1GB_singlets', 
gate_list = list( 3.8, list( c( 1.1, 1.8 ), c( 5.9, 4.9 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine. sm2

catch_render( fn = '040238 d0and d180-2_normalized-2.1GB_singlets', 
gate_list = list( 4.5, list( c( 2.7, 3.9 ), c( 6, 6.1 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine. sm2

#### 32

catch_render( fn = '06_0129 d0 and d180_5_1-2.1GB_singlets', 
gate_list = list( 5.1, list( c( 4.7, 2.9 ), c( 5.5, 5.5 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # checked on full, fine. sm2

catch_render( fn = '07_0060 D360 AND 04_0699 D360_normalized-2.1GB_singlets', 
gate_list = list( 3.8, list( c( 3.4, 3.4 ), c( 4.2, 6 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # checked on full, fine. sm2

catch_render( fn = '07_0150 D0 AND D180_2_1-2.1GB_singlets', 
gate_list = list( 3.9, list( c( 2, 2.9 ), c( 6.4, 5.7 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # checked on full, fine. sm2

#### 35

catch_render( fn = '07_0177 D0 AND 07_0177 D180_normalized-2.1GB_singlets', 
gate_list = list( 3.4, list( c( 1.8, 3.3 ), c( 5.6, 5.9 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # checked on full, fine. sm2

catch_render( fn = '07_0208 D0 and D540_normalized-2.1GB_singlets', 
gate_list = list( 3.6 ), 
eval = FALSE, 
reduce = TRUE ) # checked on full, fine. sm2

catch_render( fn = '07_0214 DE360 and D540_normalized_singlets', 
gate_list = list( 3.2, list( c( 1.2, 3.3 ), c( 5.5, 6.2 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # checked on full, fine. sm2

#### 38

catch_render( fn = '07_0236 D0 AND D180_3_1-2.1GB_singlets', 
gate_list = list( 3.7, list( c( 2.4, 3.3 ), c( 5.6, 5.8 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # checked on full, fine. sm2

catch_render( fn = '07_0259 d0 and 04_1067 d0_normalized-2.1GB_singlets', 
gate_list = list( 3.8, list( c( 3.4, 3.4 ), c( 4.2, 5.9 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # checked on full, fine. sm2

catch_render( fn = '07_0386 D0 and D540_normalized-2.1GB_singlets', 
gate_list = list( list( c( 2.9, 3 ), c( 3.7, 5.6 ) ) ), 
eval = FALSE, 
reduce = TRUE ) #checked on full, fine. sm2

#### 41

catch_render( fn = '07_0390 D0 and D540_normalized-2.1GB_singlets', 
gate_list = list( 3.9, list( c( 2.3, 3.5 ), c( 5.7, 6 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # checked on full, fine. sm2

catch_render( fn = '07_0424 d0 and d540_normalized_singlets', 
gate_list = list( 3.9, list( c( 2.6, 3.4 ), c( 6, 5.8 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # checked on full, fine. sm2

catch_render( fn = '07_0425D0 and D180_normalized-2.1GB_singlets', 
gate_list = list( 4.4, list( c( 3.1, 2.5 ), c( 7, 6.2 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # looks good on full. m2

#### 44

catch_render( fn = '07_0561 D0 AND D180_normalized-2.1GB_singlets', 
gate_list = list( 3.5, list( c( 2.1, 3.4 ), c( 6.3, 6 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # checked on full, fine. sm2

catch_render( fn = '07_0630 D0 AND 07_0630 D180_normalized_singlets', 
gate_list = list( 4.1 ), 
eval = FALSE, 
reduce = TRUE ) # checked on full, fine. sm2



# ORIGINAL STOP POINT

catch_render( fn = '07_0630 d360 and 07_0630 d540_normalized-2.1GB_singlets', 
gate_list = list( 3.9, list( c( 3.2, 0.4 ), c( 4.6, 5.7 )  ) ),
eval = FALSE, 
reduce = TRUE ) # looks good on full. m2

#### 47

catch_render( fn = '07_0663 d360 and 07_0663 d540 _2_normalized-2.1GB_singlets', 
gate_list = list( 3.5, list( c( 3.2, 2.8 ), c( 3.8, 5.7 )  ) ), 
eval = FALSE, 
reduce = TRUE ) # looks good on full. m1

catch_render( fn = '07_0692 D0 AND 04_1058 D0_normalized-2.1GB_singlets', 
gate_list = list( 3.4, list( c( 2, 3.6 ), c( 5.6, 6.1 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # looks good on full. m1

catch_render( fn = '07_0692 d180 and 04_1058 d180_normalized-2.1GB_singlets', 
gate_list = list( 3.8, list( c( 2.1, 3.7 ), c( 6.1, 6 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # looks good on full. m1

#### 50

catch_render( fn = '07_1003 day360 and day540_normalized-2.1GB_singlets', 
gate_list = list( 3, list( c( 1, 3.7 ), c( 5.7, 6.7 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # looks good on full. m1

catch_render( fn = '07_1115_1_1-2.1GB_singlets', 
gate_list = list( 4.8, list( c( 3.5, 3.3 ), c( 5.4, 5.7 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # looks good on full. m1

catch_render( fn = '07_1153 D540 AND 07_0811 D540_4_1-2.1GB_singlets', 
gate_list = list( list( c( 2.9, 3.1 ), c( 3.6, 5.4 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # looks good on full. m1

#### 53

catch_render( fn = '07_1158 d360 and 07_1158 d540_normalized_singlets', 
gate_list = list( 3.1, list( c( 1.6, 2.8 ), c( 6.3, 5.8 ) )  ), 
eval = FALSE, 
reduce = TRUE ) # looks good on full. m1

catch_render( fn = '07-0135 DO AND 09-0406 DO. FCS_normalized-2.1GB_singlets', 
gate_list = list( 4, list( c( 1.5, 3.2 ), c( 6.3, 6.1 ) ) ), 
eval = FALSE, 
reduce = FALSE ) # looks good on full. m2

catch_render( fn = '07-0259 D360 and 04-1067 D360_normalized-2.1GB_singlets', 
gate_list = list( 4.1 ), 
eval = FALSE, 
reduce = TRUE ) # looks good on full. m1

#### 56

catch_render( fn = '07-0262 D360 AND 540 27NOV2017_normalized-2.1GB_singlets', 
gate_list = list( 3.8, list( c( 1.1, 1.6 ), c( 5.9, 5.3 ) )  ), 
eval = FALSE, 
reduce = TRUE ) # looks good on full. m1

catch_render( fn = '07-0262 DO AND D180 24NOV2017_normalized-2.1GB_singlets', 
gate_list = list( 4, list( c( 1.2, 2.4 ), c( 7.3, 5.6 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # looks good on full. m1

catch_render( fn = '07-0692 D360 AND 04-1058 D360_normalized_singlets', 
gate_list = list( 4 ), 
eval = FALSE, 
reduce = TRUE ) # looks good on full. m1

#### 59

catch_render( fn = '09_0314 D0 AND 01_0978 D0_normalized-2.1GB_singlets', 
gate_list = list( 3.8, list( c( 2, 3.3 ), c( 6.4, 5.8 ) )  ), 
eval = FALSE, 
reduce = TRUE ) # looks good on full. m1

catch_render( fn = '09_0453 d0 and 09_0745 D0_normalized-2.1GB_singlets', 
gate_list = list( 3.9, list( c( 2.3, 3.7 ), c( 6.3, 6.3 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # looks good on full. m1

catch_render( fn = '09_0467 d0 and 09_0278 d0_normalized_singlets', 
gate_list = list( 3.6, list( c( 1.6, 2.8 ), c( 5.6, 5.8 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # looks good on full. m2

#### 62

catch_render( fn = '09_0507 d0 09_0587 d0_normalized-2.1GB_singlets', 
gate_list = list( 3.7, list( c( 2, 3.7 ), c( 6.5, 6 ) )  ), 
eval = FALSE, 
reduce = TRUE ) # looks good on full. m1

catch_render( fn = '09_0569 D0 and 09_0529 D0_normalized-2.1GB_singlets', 
gate_list = list( 3.4, list( c( 1.7, 3.5 ), c( 6.6, 6 ) ) ), 
eval = FALSE,  
reduce = TRUE ) # looks good on full. m1

catch_render( fn = '09_0789 D0 AND 09_0712 D0_normalized-2.1GB_singlets', 
gate_list = list( 3.7, list( c( 2.3, 3.3 ), c( 6.7, 5.9 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # looks good on full. m1

#### 65

catch_render( fn = '29NOV2017 07-0080D360 AND 07-0080 D540_normalized-2.1GB_singlets', 
gate_list = list( 3.9, list( c( 1.3, 3.0 ), c( 7.1, 6 ) )  ), 
eval = FALSE, 
reduce = TRUE ) # looks good on full. m1

catch_render( fn = '30NOV2017 07-0080 DAY0 AND 07-0080 DAY180_normalized-2.1GB_singlets', 
gate_list = list( 3.5, list( c( 2, 3.2 ), c( 5.7, 5.8 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # looks good on full. m1

catch_render( fn = 'frozen 07_0060 d180 and 04_0699 d180_6_1-2.1GB_singlets', 
gate_list = list( list( c( 2.9, 3.8 ), c( 3.6, 6.2 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # looks good on full. m1

#### 68

catch_render( fn = 'FROZEN_07_0060 D0 AND 04_0699D0_normalized-2.1GB_singlets', 
gate_list = list( 3.7,list( c( 3.4, 3.5 ), c( 4.1, 6.5 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # looks good on full. m1

catch_render( fn = 'FROZEN 20FEB TO 09MAR_ 07_1153 D360 AND 07_0811 D360_normalized-2.1GB_singlets', 
gate_list = list( 3, list( c( 2.8, 2.4 ), c( 3.8, 5.2 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # looks good on full. m1

catch_render( fn = 'FROZEN_04_1241 d360 and 07_0119 d360_normalized_singlets', 
gate_list = list( list( c( 3, 4 ), c( 4.1, 6.6 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # looks good on full. m1

#### 71

catch_render( fn = 'in beads for 10 days 07-0259 day540 and 04-1067 day 540_normalized-2.1GB_singlets', 
gate_list = list( list( c( 2.6, 0.8 ), c( 7.5, 3.8 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # looks good on full. m1

catch_render( fn = 'miguel-from_r-06_0127 and 06_0127b_5_1-2.1GB_singlets', 
gate_list = list( 3.6, list( c( 2.4, 3.8 ), c( 6.7, 6.1 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # looks good on full. m1

#### 73

catch_render( fn = 'miguel-concat-replace-04_0333_D0_AND_D180_1_TOGETHER_1-2.1GB_singlets', 
gate_list = list( 3.2, list( c( 0.9, 1.9 ), c( 6.1, 5.6 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # looks good on full. m2

catch_render( fn = '04_0695 D360 and 04_0695 D540_normalized_singlets', 
gate_list = list( 3.7, list( c( 1.8, 2.8 ), c( 5.7, 5.9 ) ) ), 
eval = FALSE, 
reduce = TRUE ) # looks good on full. m1

#### 75


### RUN IN PARALLEL

#fn <- '01_0782 day0 and day180 06sep2017_normalized-2.1GB_singlets'		
#params_list <- list( reduce = TRUE, 
#                     fn = fn	, 
#			         gate_list = list( 3.5, list( c( 2.3, 2 ), c( 5, 5.6 ) ) ) )
#repeats_list <- list( output_file = paste0( stringr::str_replace( fn, "2.1GB_singlets", "live" ) 	), 
#                      output_dir = '/home/rdxmig002/phd/CATALYST/vignettes/live_cells/html/',
#                      params = params_list )	
#
#registerDoParallel(cores=1)
#
#foreach(r=repeats_list, .combine=c) %dopar% do_rpt(r,"live_manual.Rmd" ) 
#
c( "minutes_to_run" = round( ( proc.time()[3] - time_start ) / 60, 1 ) )
