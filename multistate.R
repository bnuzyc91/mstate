data("ebmt4")
ebmt <- ebmt4
head(ebmt)
tmat <- transMat(x = list(c(2, 3, 5, 6), c(4, 5, 6), c(4, 5, 6), c(5, 6),c(), c()), 
                 names = c("Tx", "Rec", "AE", "Rec+AE", "Rel", "Death"))
tmat
msebmt <- msprep(data = ebmt, trans = tmat, time = c(NA, "rec", "ae", "recae", "rel", "srv"), 
                  status = c(NA, "rec.s", "ae.s", "recae.s",  "rel.s", "srv.s"), 
                  keep = c("match", "proph", "year", "agecl"))
covs <- c("match", "proph", "year", "agecl")
msebmt <- expand.covs(msebmt, covs, longnames = FALSE)
msebmt[msebmt$id == 1, -c(9, 10, 12:48, 61:84)]
cfull <- coxph(Surv(Tstart, Tstop, status) ~ match.1 +
                  match.2 + match.3 + match.4 + match.5 + match.6 +
                  match.7 + match.8 + match.9 + match.10 + match.11 +
                  match.12 + proph.1 + proph.2 + proph.3 + proph.4 +
                  proph.5 + proph.6 + proph.7 + proph.8 + proph.9 +
                  proph.10 + proph.11 + proph.12 + year1.1 + year1.2 +
                  year1.3 + year1.4 + year1.5 + year1.6 + year1.7 +
                  year1.8 + year1.9 + year1.10 + year1.11 + year1.12 +
                  year2.1 + year2.2 + year2.3 + year2.4 + year2.5 +
                  year2.6 + year2.7 + year2.8 + year2.9 + year2.10 +
                  year2.11 + year2.12 + agecl1.1 + agecl1.2 + agecl1.3 +
                  agecl1.4 + agecl1.5 + agecl1.6 + agecl1.7 + agecl1.8 +
                  agecl1.9 + agecl1.10 + agecl1.11 + agecl1.12 + agecl2.1 +
                  agecl2.2 + agecl2.3 + agecl2.4 + agecl2.5 + agecl2.6 +
                  agecl2.7 + agecl2.8 + agecl2.9 + agecl2.10 + agecl2.11 +
                  agecl2.12 + strata(trans), data = msebmt, method = "breslow")