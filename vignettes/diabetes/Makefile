
.PHONY: all
all: out/fig_prevalence.pdf

out/diabetes.rds: src/diabetes.R \
  data/VDR\ Dec2005-2019\ final\ v686
	Rscript $<

out/population.rds: src/population.R \
  data/DPE403903_20210414_110734_1.csv \
  data/DPE479901_20210414_110618_25.csv
	Rscript $<

out/model.est: src/model.R \
  out/diabetes.rds \
  out/population.rds
	Rscript $<

out/forecast.pred: src/forecast.R \
  out/model.est
	Rscript $<

out/prev_historical.rds: src/prev_historical.R \
  out/diabetes.rds \
  out/population.rds
	Rscript $<

out/prev_forecast.rds: src/prev_forecast.R \
  out/model.est \
  out/forecast.pred \
  out/diabetes.rds
	Rscript $<

out/fig_prevalence.pdf: src/fig_prevalence.R \
  out/prev_historical.rds \
  out/prev_forecast.rds
	Rscript $<


.PHONY: clean
clean:
	rm -rf out
	mkdir out

