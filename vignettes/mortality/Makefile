
.PHONY: all
all: out/fig_life_expectancy.pdf


out/deaths.rds: src/deaths.R \
  data/MAN05221.csv
	Rscript $<

out/popn.rds: src/popn.R \
  data/MAN02001.csv
	Rscript $<

out/model.est: src/model.R \
  out/deaths.rds \
  out/popn.rds
	Rscript $<

out/life_expectancy.rds: src/life_expectancy.R \
  out/model.est
	Rscript $<

out/fig_life_expectancy.pdf: src/fig_life_expectancy.R \
  out/life_expectancy.rds
	Rscript $<


.PHONY: clean
clean:
	rm -rf out
	mkdir out

