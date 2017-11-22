LOC_DIR = ./Data/ADP/LOC/Confirmed/
LOCS = $(shell find $(LOC_DIR) -type f -name '*')
OCC_FILE = ./Data/flipped\ occurrences.csv
COUNT_FILE = ./Data/counts.csv

all: paleocene.pdf

paleocene.pdf: paleocene.bib paleocene.tex Figures/map1.pdf Figures/Cohort_grey.pdf Figures/count_data.eps Figures/new_div.pdf Plates/*.eps
	pdflatex paleocene
	bibtex paleocene
	pdflatex paleocene
	pdflatex paleocene

paleocene.tex: Scripts/latexify_rangechart.r $(OCC_FILE)
	Rscript $<

Figures/Cohort_grey.pdf: Scripts/cohorts.R Data/cohorts.csv $(OCC_FILE)
	Rscript $<

Figures/count_data.eps: Scripts/plot_paleocene.R $(COUNT_FILE)
	Rscript $<

Figures/new_div.pdf: Scripts/paldiatdiv.R $(OCC_FILE)
	Rscript $<

$(OCC_FILE): Scripts/applyNewAges.R $(LOC_DIR) $(LOCS)
	Rscript $<

$(COUNT_FILE): Scripts/applyNewAges.R $(LOC_DIR) $(LOCS)
	Rscript $<

clean:
	rm *.aux *.bbl *.blg *.log *.spl */*-converted-to.pdf
