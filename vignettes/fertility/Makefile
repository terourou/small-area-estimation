
.PHONY: all
all: out/fig_agespecific.pdf \
     out/fig_tfr.pdf

out/births.rds: src/births.R \
  data/VSB477104_20210324_111551_95.csv
	Rscript $<

out/popn.rds: src/popn.R \
  data/Subnational\ population\ estimates\ (TA/TABLECODE7980_Data_d1066377-5e47-404a-a1a5-64b21615c99e.csv
	Rscript $<

out/model.est: src/model.R \
  out/births.rds \
  out/popn.rds
	Rscript $<

out/forecast.pred: src/forecast.R \
  out/model.est
	Rscript $<

out/agespecific_direct.rds: src/agespecific_direct.R \
  out/model.est
	Rscript $<

out/agespecific_modelled.rds: src/agespecific_modelled.R \
  out/model.est \
  out/forecast.pred
	Rscript $<

out/fig_agespecific.pdf: src/fig_agespecific.R \
  out/agespecific_direct.rds \
  out/agespecific_modelled.rds
	Rscript $<

out/tfr_direct.rds: src/tfr_direct.R \
  out/model.est
	Rscript $<

out/tfr_modelled.rds: src/tfr_modelled.R \
  out/model.est \
  out/forecast.pred
	Rscript $<

out/fig_tfr.pdf: src/fig_tfr.R \
  out/tfr_direct.rds \
  out/tfr_modelled.rds
	Rscript $<


.PHONY: clean
clean:
	rm -rf out
	mkdir out

