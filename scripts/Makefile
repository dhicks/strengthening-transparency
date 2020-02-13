data := ../data
R := ../R

all: $(data)/03_tokens.Rds; 


$(data)/03_tokens.Rds: 03_nlp.R \
					   $(data)/01_comments.Rds \
					   $(data)/02_attachments.Rds
	Rscript 03_nlp.R


$(data)/02_attachments.Rds: 02_prep_text.R \
							  $(data)/01_attachments.Rds \
						      $(R)/extract_text.R
	Rscript 02_prep_text.R


$(data)/01_attachments.Rds $(data)/01_comments.Rds &: $(R)/scrape.R \
													  01_scrape.R
	Rscript 01_scrape.R
