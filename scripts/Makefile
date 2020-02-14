data := ../data
R := ../R

all: $(wildcard $(data)/04_*.Rds);


$(data)/04_*.Rds: 04_vocab.R \
				  $(R)/H.R \
				  $(data)/03_tokens.Rds
	Rscript 04_vocab.R


## NB This step is fragile.  03 caches the annotated text 
## of each document (comment/attachment).  If the document text is 
## modified but the cached version remains, 03 will effectively ignore
## the changes
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
