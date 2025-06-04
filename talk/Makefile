all: slides

.PHONY: slides
slides: talk.html
talk.html: talk.qmd
	quarto render talk.qmd

.PHONY: publish
publish:
	quarto publish netlify talk.qmd

