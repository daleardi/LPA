#########H#########I#########R#########E#########M#########E#########P#########L#########E#########A#########S#########E
#.-------.     .-''-.     ____        _______      .-''-.                .--.      .--..-./`)    .-'''-.     .-''-.    #      
#\  _(`)_ \  .'_ _   \  .'  __ `.    /   __  \   .'_ _   \               |  |_     |  |\ .-.')  / _     \  .'_ _   \   #      
#| (_ o._)| / ( ` )   '/   '  \  \  | ,_/  \__) / ( ` )   '              | _( )_   |  |/ `-' \ (`' )/`--' / ( ` )   '  #      
#|  (_,_) /. (_ o _)  ||___|  /  |,-./  )      . (_ o _)  |  _ _    _ _  |(_ o _)  |  | `-'`"`(_ o _).   . (_ o _)  |  #      
#|   '-.-' |  (_,_)___|   _.-`   |\  '_ '`)    |  (_,_)___| ( ' )--( ' ) | (_,_) \ |  | .---.  (_,_). '. |  (_,_)___|  #      
#|   |     '  \   .---..'   _    | > (_)  )  __'  \   .---.(_{;}_)(_{;}_)|  |/    \|  | |   | .---.  \  :'  \   .---.  #      
#|   |      \  `-'    /|  _( )_  |(  .  .-'_/  )\  `-'    / (_,_)--(_,_) |  '  /\  `  | |   | \    `-'  | \  `-'    /  #      
#/   )       \       / \ (_ o _) / `-'`-'     /  \       /               |    /  \    | |   |  \       /   \       /   #      
#`---'        `'-..-'   '.(_,_).'    `._____.'    `'-..-'                `---'    `---` '---'   `-...-'     `'-..-'    #      
#.-------. .-------.   .-./`)     _______  .-./`) ,---.   .--.  .-_'''-.                                               #      
#\  _(`)_ \|  _ _   \  \ .-.')   /   __  \ \ .-.')|    \  |  | '_( )_   \                                              #      
#| (_ o._)|| ( ' )  |  / `-' \  | ,_/  \__)/ `-' \|  ,  \ |  ||(_ o _)|  '                                             #      
#|  (_,_) /|(_ o _) /   `-'`"`,-./  )       `-'`"`|  |\_ \|  |. (_,_)/___|                                             #      
#|   '-.-' | (_,_).' __ .---. \  '_ '`)     .---. |  _( )_\  ||  |  .-----.                                            #      
#|   |     |  |\ \  |  ||   |  > (_)  )  __ |   | | (_ o _)  |'  \  '-   .'                                            #      
#|   |     |  | \ `'   /|   | (  .  .-'_/  )|   | |  (_,_)\  | \  `-'`   |                                             #      
#/   )     |  |  \    / |   |  `-'`-'     / |   | |  |    |  |  \        /                                             #      
#`---'     ''-'   `'-'  '---'    `._____.'  '---' '--'    '--'   `'-...-'                                              #      
#                                                                                                                      #      
#########H#########I#########R#########E#########M#########E#########P#########L#########E#########A#########S#########E

#This script calculates the Total Cost of a 30yr DU loan for use with APR calculations. 
#Total cost in this case is Loan Amount + Points + Loan Pricing Adjustments
#It uses the concept of a peicewise function. The domain has 3 input variables: Loan Amount, Value, and FICO 
#Depending on what interval these values fall into they are mapped to a different solution in the range
#Author: Dominick Aleardi
#Last Update: 3/10/14 11:25PM


#The idea of a peicewise function is very abstract. Any function that takes in Loan Amount, Value, and FICO and returns
#the correct cost is technically peicewise.The real problem is how to implement the mapping of the function.
#This makes the problem at hand more computational than mathematical. I am curious if the language you wrote PiR in 
#has a built in peicewise function. Then the issue is just defining the conditionals, as the execution of the mapping 
#should be handled by the compiler. If the mapping done by the compiled function is handled more efficiently 
#than how the previous code handled it then thats why you're seeing efficiency gains. 

#Unfortunately, R does not have a built in peicewise function and I could not find a package that included one. 
#########H#########I#########R#########E#########M#########E#########P#########L#########E#########A#########S#########E

#Define Constants
MAXLTV <- 97
MAXLA <- 625500
MINFICO <- 450
MAXFICO <- 850

#Initialize Variables
loanAmount <- 0
value <- 0
fico <- 0
continue <- "yes"
cost <- 0

#########H#########I#########R#########E#########M#########E#########P#########L#########E#########A#########S#########E

#Define the values from the Market Update that add up to the range of the function

#Create the rate sheet data frame
rateSheet<-data.frame(rates=seq(4.25,5.25,by=0.125),points=c(3.125,2.375,1.375,0.5,-.375,-1,-1.5,-2.375,-3.125))

#create the LTV/FICO matrix
ltv.fico.matrix<-matrix( c(
  c(1.5,.5,.5,0,0,-.25,-.25,-.25),
  c(1.5,1.5,1.25,1,.5,.5,0,0),
  c(1.5,1.5,1.25,1,.5,.5,0,0),
  c(3,3,2.5,2,1.25,.75,.25,0),
  c(3,3,3,2.5,1.75,1,.5,.25),
  c(3.25,3.25,3.25,2.75,1.5,1,.5,.25),
  c(3.25,3.25,2.75,2.25,1.25,1,.5,.25),
  c(3.25,3.25,2.75,2.25,1.25,1,.5,.25),
  c(3.25,3,2.25,1.75,1,1,.5,.25)),nrow=8,ncol=9
)

#Create the Loan Ladder vector
ll.vector<-c(3.75,3.75,2.5,1,1,0.75,0.5,0.25)


#########H#########I#########R#########E#########M#########E#########P#########L#########E#########A#########S#########E


#I orginally thought of using nested if else statements for the next two functions. 
#I new switch statements are faster than if else statements. So I decided to use them instead. 
#the trick was figuring out how to assign a continuous interval of values to a discrete case without a comparison test. 
#Integer division for the win! (this works as long as the ranges are uniform)
#I implemented the switch functions and they worked really well. However, as I was working I realized I could
#use the same integer division trick to directly access a vector or matrix. I ran a couple of speed tests
#using the direct indexing was almost 30% faster than a switch statement. Atleast on my computer.



#Function to get Loan Ladder LPA
loanLadder <- function(LA){
  lpa<-ll.vector[(LA/25000)+1] #Get the LPA
  if(is.na(lpa)) return(0) #if the vector is beyond it's length it returns NA. This handles values above 200K
  else return(lpa)
}


#Function to get LTV/FICO LPA
#LTV should be entered as a number between 1 and 97
#FICO should be entered as a number berween 450 and 850
ltv.fico <- function(LTV,FICO){
  #get the column
  if(LTV<=60) col<-1 
  else col<-(LTV/5)-10 #increment between 2 and 9  for every 5% from 61 to 97
  #get the row
  if(FICO<620) row <- 1
  else if(FICO>=740) row <-8
  else row <- (FICO/20)-30 #increment between 2 and 7 for every 20 points from 621 to 739
  #lookup the LPA
  return(ltv.fico.matrix[row,col])
}


#########H#########I#########R#########E#########M#########E#########P#########L#########E#########A#########S#########E


#Function to read in an integer
#Uses recursion to continuously prompt the user until an integer is entered
readInteger <- function(prmpt){
  n <- readline(prompt=prmpt)
  #check to see if it is an integer
  if(!grepl("^[0-9]+$",n)){
    cat("That is not an integer\n")
    return(readInteger(prmpt))
  }
  return(as.integer(n))
}

#Piecewise fucntion for calculating total loan amount
#Returns the total cost in points based upon loan amount, value, and fico or NULL upon error
totalCost <- function(LA,Val,FICO){
  #Error checking
  #Check to see if loan amount is between 0 and 625500
  if(LA<=0 | LA>MAXLA) {
    cat("ERROR: Loan amount must be between $0 and $", MAXLA,"\n")
    return(NULL) 
  }
  #Check LTV is between 0% and 97%
  LTV <- (LA/Val)*100
  if(LTV<=0 | LTV>MAXLTV){
    cat("ERROR: LTV must be between 0% and ",MAXLTV, "%\n")
    return(NULL) 
  }
  #Check to make sure FICO is between 450 and 850
  if(FICO<MINFICO | FICO>MAXFICO){
    cat("ERROR: FICO must be between ",MINFICO," and ",MAXFICO,"\n")
    return(NULL) 
  }
  
  #actually calculate the LPA
  LPA<-loanLadder(LA)+ltv.fico(LTV,FICO)
  return(LPA)
}

#########H#########I#########R#########E#########M#########E#########P#########L#########E#########A#########S#########E

#Main program
while(continue=="yes"){  
    #prompt for input
    cat("\n\nWelcome to Dominick's Pricing Calculator\n\n")
    loanAmount <- readInteger("Please enter the loan amount: ")
    value <- readInteger("Please enter the value: ")
    fico <- readInteger("Please enter the FICO score: ")
    
    #get total cost
    cost <- totalCost(loanAmount,value,fico)
    #check for NULL value
    if(is.null(cost)){
      cat("Oops! Looks like you entered some invalid information\n")
      continue<-tolower(readline("Would you like to try again? ('yes' to continue): "))
    }else{
      #calculate the total loan amount for each rate in the rate sheet
      totLoanAmount <- 0
      for (i in 1:length(rateSheet$points)) {
        totLoanAmount[i] <- loanAmount*(1+(cost+rateSheet$points[i])/100)
      }
      #print the results
      results<-data.frame(Rate=rateSheet$rates,Total.Cost=totLoanAmount)
      print(results)
      #prompt the user to continue
      continue<-tolower(readline("Would you like to try again? ('yes' to continue): "))
    }
  }

#clean up the environment
rm(list=c("continue","cost","fico","ll.vector","loanAmount","loanLadder",     
     "ltv.fico","ltv.fico.matrix","MAXFICO","MAXLA","MAXLTV","MINFICO","rateSheet",
     "readInteger","totalCost","value"))


#Post Script
#What this program really does is add up the results of 2 peicewise functions. If one wanted to they could
#create a giant multi-dimensional array of all possible input intervals and use the same logic 
#to get the total cost with a single look up. This is computationally heavy on the front end, but would 
#cut processing time down for each run. 
#Also, this problem is well suited for a multivariate decision tree that perfectly fits the output space. 
#I'm not sure it would be as computationally efficent as the peicewise table look up method, but I feel 
#like it is definetly more intuitive. Also, it would be easier to manage for the much more complex loan 
#pricing on every possible product. If pricing methods were to change you could just grow or prune the 
#appropriate nodes. 
