.PHONY: clean

writeup.pdf: writeup.tex writeup.bbl
	lualatex $<
	# lualatex $<
	# not clear whether we need to run a second time for references

writeup.bbl: bib.bib
	lualatex writeup
	biber writeup

clean:
	rm *.bbl *.bcf *.log *.xml *.blg *.aux
