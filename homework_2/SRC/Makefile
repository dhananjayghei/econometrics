all:
#	R --vanilla < monte_carlo.R > monte_carlo.out
	Rscript -e "library(rmarkdown); rmarkdown::render('Fdist.Rmd', 'html_document')"

clean:
	rm *.out;
	rm *.pdf;
	rm *.html
