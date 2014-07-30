########################################################################################################################
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
########################################################################################################################

#This script calculates the Total Cost of a loan for use with APR calculations. 


####################################

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

####################################
#Define the values from the Market Update that add up to the range of the function

#Create the rate sheet data frame
rateSheet<-data.frame(rates=seq(4.25,5.25,by=0.125),points=c(3.125,2.375,1.375,0.5,-.375,-1,-1.5,-2.375,-3.125))

#create the LTV/FICO matrix
ltv.fico.matrix<-matrix( c(
  c(1.5,.5,.5,0,0,-.25,-.25,-.25),
  c(1.5,1.5,1.25,1,.5,.5,0,0),
  c(3,3,2.5,2,1.25,.75,.25,0),
  c(3,3,3,2.5,1.75,1,.5,.25),
  c(3.25,3.25,3.25,2.75,1.5,1,.5,.25),
  c(3.25,3.25,2.75,2.25,1.25,1,.5,.25),
  c(3.25,3.25,2.75,2.25,1.25,1,.5,.25),
  c(3.25,3,2.25,1.75,1,1,.5,.25)),nrow=8,ncol=8
)

#Create the Loan Ladder vector
la.vector<-c(3.75,3.75,2.5,1,1,0.75,0.5,0.25)


###################################


#I orginally thought of using nested if else statements for the next two functions. 
#I new switch statements are faster than if else statements. So I decided to use them instead. 
#the trick was figuring out how to assign a continuous range of values to a discrete case without a comparison test. 
#Integer division for the win! (this works as long as the ranges are uniform)
#I implemented the switch functions and they worked really well. However, as I was working I realized I could
#use the same integer division trick to directly access a vector or matrix. I ran a couple of speed tests
#using the direct indexing was almost twice as fast as the switch statement.    

#Function to get Loan Ladder LPA
loanLadder <- function(LA){
  lpa<-la.vector[LA/25000] #Get the LPA
  if(is.na(lpa)) return(0) #if the vector is beyond it's length it returns NA. This handles values above 200K
  else return(lpa)
}


#Function to get LTV/FICO LPA
#LTV should be entered as a number between 1 and 97
ltv.fico <- function(LTV,FICO){
  #get the column
  if(LTV<=60) col<-1 
  else col<-(LTV/5)-12
  #get the row
  if(FICO<620) row <- 1
  else if(FICO>=740) row <-8
  else row <- (FICO/20)-31
  #lookup the LPA
  return(ltv.fico.matrix[row,col])
}

#Post Script
#What this program really does is add up the results of 3 peicewise functions. If one wanted to they could
#create a giant multi-dimensional array and use the same logic to get the total cost with a single look up. 
#This is computationally heavy on the front end, but would cut processing time down.


##################################


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

###################################

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
      #print the results
      cat(rateSheet$rates[i]," -> ",totLoanAmount[i],"\n")
    }
    #prompt the user to continue
    continue<-tolower(readline("Would you like to try again? ('yes' to continue): "))
  }
}








