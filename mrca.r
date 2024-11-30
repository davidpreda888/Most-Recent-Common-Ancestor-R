#First analysis of how the simulation would work
simulate_MRCA <- function(N){
  parents <-list()
  kids<-list()
  kidsgen<- 0
  while(length(parents[[kids]]!= N)){ 
    for(i in 1:N){
      kids[[i]]<- vector()
      parents[[i]]<- vector()
      kids[[i]]<-sample(1:N, 2, replace= T)
    }
    
    for(i in 1:N){
      x<- kids[[i]][1]
      parents[[x]]<- union(parents[[x]],i)
      y<- kids[[i]][2]
      parents[[y]]<- union(parents[[y]],i)
    }
    kidsgen<- kidsgen + 1
  }
}

#MRCA with replacement
MRCA<- function(N){
  gen0 <- list()
  gen1 <- list()
  gen1parents <- list()
  gen2 <- list()
  gencount <- 1
  for(i in 1:N){
    gen0[[i]]<- vector()
    gen1[[i]] <- vector()
    gen2[[i]]<- vector()
    gen0[[i]] <- sample(1:N,2, replace= T)
  }
  
  for(i in 1:N){
    x <- gen0[[i]][1]
    gen1[[x]]<-union(gen1[[x]], i)
    y <- gen0 [[i]][2]
    gen1[[y]] <- union(gen1[[y]],i)
  }
  
  while(max(data.frame(sapply(gen2,length)))!= N){
    gencount<- gencount+1
    for(i in 1:N){
      gen1parents[[i]]<- vector()
      gen1parents[[i]]<- sample(1:N, 2, replace = T)
    }
    
    for(i in 1:N){
      x <- gen1parents[[i]][1]
      gen2[[x]] <- union(gen2[[x]],gen1[[i]])
      y <- gen1parents [[i]][2]
      gen2[[y]] <- union(gen2[[y]],gen1[[i]])
    }
  }
  return(gencount) 
}
MRCA(4000) #Test code

#Creates a table with the results
x <- table(replicate(MRCA(4000), 25))

#MRCA without replacement
MRCA<- function(N){
  gencounter<- 0
  old_gen <- list()
  
  for(i in 1:N){
    old_gen[[i]]<- c(i)
  }
  
  while(max(sapply(old_gen, length))!= N){
    new_gen<- list()
    for(i in 1:N){
      new_gen[[i]]<- vector()
    }
    for(i in 1:N){
      parents <-sample(1:N, 2, replace = F)
      new_gen[[parents[1]]] <- union(new_gen[[parents[1]]], old_gen[[i]])
      new_gen[[parents[2]]] <- union(new_gen[[parents[2]]], old_gen[[i]])
    }
    gencounter<- gencounter + 1
    old_gen = new_gen
  }
  return(gencounter)
}
MRCA(4000)

#Multiple tests with different population sizes
replicate(25,MRCA(4000))
replicate(25,MRCA(2000))
replicate(25,MRCA(1000))
replicate(25,MRCA(500))


#Multiple test with replication as True or False
largepop_repT <- replicate(40,MRCA(1000))
smallpop_repT <- replicate(40,MRCA(400))
largepop_repF <- replicate(40,MRCA(1000))
smallpop_repF <- replicate(40,MRCA(400))


#Hypothesis testing for large population size
#Ho: Ux = Uy
#Ha: Ux != Uy

#Mean and sample variance for with replacement
X <- mean(largepop_repT)
Sx <- var(largepop_repT)
#Mean and sample variance for without replacement
Y <- mean(largepop_repF)
Sy <- var(largepop_repF)

#Assuming Ho is true
z <- (X-Y)/(sqrt(((Sx/1000)+(Sy/1000))))

#P[Z<=z] = 1 and 1 > -1.96 so we do not reject Ho

#P-value
p = 2*pnorm(z)

#Hypothesis testing for small population size
#Ho: Ux = Uy
#Ha: Ux != Uy

#Mean and sample variance for with replacement
x <- mean(smallpop_repT)
sx <- var(smallpop_repT)
#Mean and sample variance for without replacement
y <- mean(smallpop_repF)
sy <- var(smallpop_repF)

#Assuming Ho is true
zsmallpop <- (x-y)/(sqrt(((sx/400)+(sy/400))))

#P[Z<=z] = 1 and 1 > -1.96 so we do not reject Ho

#P-value
psmall = 2*pnorm(zsmallpop)

