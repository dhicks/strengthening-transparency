all: pdf wc

split = 17

.PHONY: html
html: paper.html
paper.html: paper.qmd methods.qmd st_text_mining.yaml
	quarto render paper.qmd --to html

.PHONY: pdf
pdf: paper.pdf
paper.pdf: paper.qmd methods.qmd st_text_mining.yaml
	quarto render paper.qmd --to aft-pdf

.PHONY: wc
wc: 
	echo "splitting at page $(split)"
	qpdf paper.pdf --pages . 1-$(split) -- main.pdf
	pdftotext main.pdf - | wc -w

