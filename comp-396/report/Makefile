.PHONY: clean

writeup.pdf: writeup.tex writeup.bbl
	lualatex $<
	lualatex $<

writeup.bbl: bib.bib
	lualatex writeup
	biber writeup

clean:
	rm *.bbl *.bcf *.log *.xml *.blg *.aux
