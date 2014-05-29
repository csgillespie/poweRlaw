## $* = filename without extension
## $@ = the output file
## $< = the input file

.SUFFIXES: .tex .pdf .Rnw .R

DIR = pkg/vignettes
PKG = poweRlaw

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
	make clean
	cd ../ && R CMD build $(PKG)

.Rnw.pdf:
	Rscript  -e "require(knitr); knit('$*.Rnw', output='$*.tex')" && \
	pdflatex $*.tex && \
	bibtex $* && \
	pdflatex $*.tex && \
	pdflatex $*.tex


clean:
	cd $(DIR) &&  \
	rm -fv *.aux *.dvi *.log *.toc *.bak *~ *.blg *.bbl \
		*.lot *.lof *.nav *.snm *.out *.pyc *.spl \
		*-concordance.tex *.synctex.gz  \
		$(ALLPDF) $(ALLTEX) && \
	rm -fvr knitr_cache*/ knitr_figure*/ auto/ 



cleaner:
	make clean && \
	rm -fvr auto/ && \
	rm -fvr *.Rcheck $(PKG)_*.tar.gz && \
	rm -fv README.html
