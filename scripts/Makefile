data := ../data
out := ../out
R := ../R

all: pipeline eda;


pipeline: $(wildcard $(data)/05_*.Rds)
$(data)/05_*.Rds: $(R)/H.R \
				  $(data)/03_tokens.Rds
	Rscript 05_vocab.R

eda: $(out)/04_eda.html
$(out)/04_eda.html: $(data)/03_tokens.Rds \
					$(data)/03_text.Rds \
					04_eda.R
	Rscript -e "rmarkdown::render('04_eda.R', output_dir = file.path('..', 'out'))"


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
