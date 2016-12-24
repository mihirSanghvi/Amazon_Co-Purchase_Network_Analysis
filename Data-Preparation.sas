%LET PATH = /folders/myfolders/My-R-Project;
LIBNAME SASDATA "&PATH./SASDATA";
OPTIONS COMPRESS=YES;

DATA SASDATA.AMAZON_META;
	INFILE "&PATH./RAWDATA/amazon-meta.txt" TRUNCOVER;
	
	INPUT 
	@7 isNewLine $10.@;
	
	IF isNewLine ^='' THEN
		DO;
			INPUT
			@7 id $10./ 
			@7 asin $10./ 
			@3 isDiscontinuedProduct $20.@;
			IF isDiscontinuedProduct ^= 'discontinued product' THEN
			DO;
			
				INFORMAT title group $300. 
				salesrank 10. 
				categoryCount reviewCount downloadedReviews 3.
				avgRating $3.;
				
				INPUT
				@10 title$300./ 
				@10 group/
				@14 salesrank//
				@15 categoryCount@/;
				
				IF categoryCount < 0  THEN DO
					categoryCount = 0;
				END;		
								
				DO I=1 TO categoryCount;
					INPUT;
 					IF I=categoryCount THEN DO;	
 						INPUT
						@"reviews: total: " reviewCount
						@"downloaded: " downloadedReviews
						@"avg rating: " avgRating;
						IF downloadedReviews< 0  THEN DO
							downloadedReviews= 0;
						END;	
						DO J=1 TO downloadedReviews;
							INPUT;
						END;
						DROP J;
					END; 					
 				END;
 				OUTPUT;
				DROP I categoryCount downloadedReviews;											
			END;
			DROP isDiscontinuedProduct;
	END;
	DROP isNewLine;	
	title = strip(title);
RUN;

PROC PRINT DATA=SASDATA.AMAZON_META;
RUN;


PROC EXPORT DATA = SASDATA.AMAZON_META
    OUTFILE='/folders/myfolders/My-R-Project/amazon-meta.csv'
    DBMS=CSV;
RUN;
