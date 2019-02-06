library("rodeo")
library("readxl")

sheets <- c("vars", "pars", "funs", "pros", "stoi", "xstoi", "forc")

## read the tables
tables <- lapply(sheets, function(sheet) read_excel("model.xlsx", sheet = sheet))

## convert from read_xl specific "tibble" class to data.frame
tables <- lapply(tables, as.data.frame)
names(tables) <- sheets

## create rodeo object
model <- with(tables, rodeo$new(vars=vars, pars=pars, funs=funs, pros=pros, stoi=stoi, dim=1))

## alternative: work with cross tabulated stoichiometry matrix
#xxstoi <- as.matrix(tables$xstoi[,-1])
#rownames(xxstoi) <- tables$xstoi[,1]
#model <- with(tables, rodeo$new(vars=vars, pars=pars, funs=funs, pros=pros, stoi=xxstoi, asMatrix=TRUE, dim=1))

## write forcing function file
fforc <- with(tables, forcingFunctions(forc))
write(fforc, file = "forcings.f95")

## compile the model
ret <- model$compile(sources  = c("forcings.f95", "functions.f95"))

vars <- setNames(tables$vars$default, tables$vars$name)
pars <- setNames(tables$pars$default, tables$pars$name)


model$setPars(pars)
model$setVars(vars)

out1 <- model$dynamics(seq(0, 200, 0.1))
plot(out1)

pars <- c(S_in_def=0.1, b=0.1, c=0.1, d=0.1, e=0.1, f=0.1, g=0.0)
model$setPars(pars)
out2 <- model$dynamics(seq(0, 200, 0.1), method="bdf")
plot(out2)

pars <- c(S_in_def=NaN, b=0.1, c=0.1, d=0.1, e=0.1, f=0.1, g=0.0)
model$setPars(pars)
out3 <- model$dynamics(seq(0, 200, 0.1), method="bdf_d")
plot(out1, out2, out3)


model$plotStoichiometry(box=1)


cat(exportDF(tables$pros[c("name", "unit", "expression", "tex")], tex=TRUE,
         funCell=c(name=function(x){paste0("\\textit{",x,"}")},
           tex=function(x){paste0("$",x,"$")})))



