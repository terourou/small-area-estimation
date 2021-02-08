
.PHONY: all
all: out/fig_income_popn.pdf


out/income_df.rds: src/income_df.R \
  data/NZIS-Sub-SURF1.xlsx
	Rscript $<

out/income_mean.rds: src/income_mean.R \
  out/income_df.rds
	Rscript $<

out/income_sd.rds: src/income_sd.R \
  out/income_df.rds
	Rscript $<

out/model.est: src/model.R \
  out/income_mean.rds \
  out/income_sd.rds
	Rscript $<

out/income_popn.rds: src/income_popn.R \
  out/model.est
	Rscript $<

out/fig_income_popn.pdf: src/fig_income_popn.R \
  out/income_popn.rds
	Rscript $<


.PHONY: clean
clean:
	rm -rf out
	mkdir out

