## $* = filename without extension
## $@ = the output file
## $< = the input file

.SUFFIXES: .tex .pdf .Rnw .R

DIR = pkg/vignettes
PKG = pkg

ALL =  examples compare_distributions poweRlaw

ALLTEX = $(ALL:=.tex)
ALLPDF = $(ALL:=.pdf)

.PHONY: force  all clean 

force: 
	make -C $(DIR) -f ../../Makefile all

all: $(ALLPDF)


view: 
	make all
	acroread ../../www/$(PKG)/*.pdf

build:
	make cleaner
	R CMD build $(PKG)

.Rnw.pdf:
	Rscript  -e "require(knitr); knit('$*.Rnw', output='$*.tex')" && \
	pdflatex $*.tex && \
	bibtex $* && \
	pdflatex $*.tex && \
	pdflatex $*.tex


clean:
	cd $(DIR) &&  \
	rm -fv *.aux *.dvi *.log *.toc *.bak *~ *.blg *.bbl \
		*.lot *.lof *.nav *.snm *.out *.pyc *.spl *.tpt \
		*-concordance.tex *.synctex.gz  \
		$(ALLPDF) $(ALLTEX) && \
	rm -fvr knitr_cache*/ knitr_figure*/ auto/ 



cleaner:
	make clean && \
	rm -fvr auto/ && \
	rm -fvr *.Rcheck $(PKG)_*.tar.gz && \
	rm -fv README.html 
	cd $(DIR) && rm -fvr a_introduction.pdf b_power*.pdf c_compar*.pdf d_jss*.pdf *.tex
