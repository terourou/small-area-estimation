
.PHONY: all
all: out/fig_unemployment.pdf


out/raw.rds: src/raw.R \
  data/ARXSTK1_20210313-211050.csv
	Rscript $<

out/unemployed.rds: src/unemployed.R \
  out/raw.rds
	Rscript $<

out/labour_force.rds: src/labour_force.R \
  out/raw.rds
	Rscript $<

out/model.est: src/model.R \
  out/unemployed.rds \
  out/labour_force.rds
	Rscript $<

out/rate_direct.rds: src/rate_direct.R \
  out/model.est
	Rscript $<

out/rate_modelled.rds: src/rate_modelled.R \
  out/model.est
	Rscript $<

out/fig_unemployment.pdf: src/fig_unemployment.R \
  out/rate_direct.rds \
  out/rate_modelled.rds
	Rscript $<


.PHONY: clean
clean:
	rm -rf out
	mkdir out

