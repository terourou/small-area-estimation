
.PHONY: all
all: out/fig_guestnights.pdf

out/guestnights.rds: src/guestnights.R \
  data/ACS348801_20210324_065628_96.csv
	Rscript $<

out/model.est: src/model.R \
  out/guestnights.rds
	Rscript $<

out/forecast.pred: src/forecast.R \
  out/model.est
	Rscript $<

out/guestnights_forecasted.rds: src/guestnights_forecasted.R \
  out/forecast.pred
	Rscript $<

out/fig_guestnights.pdf: src/fig_guestnights.R \
  out/guestnights.rds \
  out/guestnights_forecasted.rds
	Rscript $<


.PHONY: clean
clean:
	rm -rf out
	mkdir out

