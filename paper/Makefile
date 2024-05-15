all: html

.PHONY: html
html: paper.html
paper.html: paper.qmd methods.qmd
	quarto render paper.qmd --to html

.PHONY: pdf
pdf: paper.pdf
paper.pdf: paper.qmd methods.qmd
	quarto render paper.qmd --to pdf
