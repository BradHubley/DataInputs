
HOmem<-function(data, year, tmbdir, filename="rep_code"){

  require(TMB)


  data <- subset(data,YEAR==year)

  # define data
  nb=data$empty_baited # nb is the number of baited hooks at the end of the soak time
  nt=data$target_species # nt is the number of individuals of the target species caught
  nnt=data$other_species # nnt is the number of individuals of the non-target species caught
  ne=data$empty_unbaited+data$broken_hook # ne is the number of empty hooks at the end of the soak time
  st=data$SOAKMINP3P1  # st is the soak time
  empty = data$empty_unbaited+data$broken_hook
  # Put the required data into a matrix

  # The mean of the soak time (fix at the mean of the soak time)
  st_fixed=rep(mean(st),length(st))

  ## Calculate the initial values for TMB estimation
  # Using the explicit equation to calculate the estimators (MEM1 with pt=0 from Etienne 2010)
  soak=mean(st)
  # lambda for target species
  ldathat=(sum(nt)/(sum(nt+nnt+ne+nb)-sum(nb)))*(1/soak)*
    log(sum(nt+nnt+ne+nb)/sum(nb))
  print(paste("ldathat =",ldathat))
  # lambda non-target species
  ldanthat=((sum(nnt)+sum(ne))/(sum(nt+nnt+ne+nb)-sum(nb)))*
    (1/soak)*log(sum(nt+nnt+ne+nb)/sum(nb))
  print(paste("ldanthat =",ldanthat))
  # Escaping probability for target species
  pthat=0
  print(paste("pthat =",pthat))
  # Escaping probability for non-target species
  pnthat=sum(ne)/(sum(ne)+sum(nnt))
  print(paste("pnthat =",pnthat))

  # Logit function
  logitp=function(p){log(p/(1-p))}
  # Inverse logist function
  logitpi=function(t){exp(t)/(1+exp(t))}


  ## Use TMB to reproduce the results

  # Call TMB function value

  compile(file.path(tmbdir,paste0(filename,".cpp")))
  #  compile(cpp.file)

  # Dynamically link the C++ code
  # dyn.load(dll.file)))
  dyn.load(file.path(tmbdir,paste0(filename,".dll")))

  # data$empty = data$empty_unbaited+data$broken_hook
  # Put the required data into a matrix
  x = with(data, cbind(empty_baited, target_species, other_species, empty))

  # Soak time
  stv = as.vector(st)
  # st_fixedv = as.vector(st_fixed)  #fixed soak time

  # Data list
  data = list(x=x,s=stv)
  # datafixed = list(x=x,s=st_fixedv) #fixed soak time

  # Parameter list
  param = list()
  # paramfixed = list() #fixed soak time
  # Initial values for lambda.t, lambda.nt and pnt
  # Use the values calculated previously as the starting points
  param$theta = c(log(ldathat),log(ldanthat),logitp(pnthat))
  # paramfixed$theta = c(log(ldathat),log(ldanthat),logitp(pnthat)) #fixed soak time
#browser()
  # Construct an R object (f) that represents our C++ function
  # MakeADFun calls C++
  f = MakeADFun(data, param, DLL=filename)
  # ffixed = MakeADFun(datafixed, paramfixed, DLL="rep_code") #fixed soak time

  # Call TMB function value
  fit = nlminb(f$par,f$fn,f$gr)
  # fitfixed = nlminb(ffixed$par,ffixed$fn,ffixed$gr) #fixed soak time
  # Calculate standard deviations of all model parameters

  sdr = sdreport(f)
  # Estimated results for model with actual soak time
  summary(sdr)

  # sdrfixed = sdreport(ffixed) #fixed soak time
  # Estimated results for model with actual soak time
  # summary(sdrfixed)#fixed soak time

  return(list("fit"=fit,"sdr"=sdr))
}

