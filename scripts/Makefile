data := ../data
R := ../R

all: $(data)/02_attachments.Rds; 


$(data)/01_attachments.Rds: $(R)/scrape.R \
							01_scrape.R
	Rscript 01_scrape.R


$(data)/02_attachments.Rds &: 01_scrape.R \
							  $(data)/01_attachments.Rds \
						      $(R)/extract_text.R \
						      02_prep_text.R
	Rscript 02_prep_text.R
	
