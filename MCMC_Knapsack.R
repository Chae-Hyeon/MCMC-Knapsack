#############################################################################
#The function "knapsack(x)" will calculate the appropriate items to bring 
#on the hypothesized hiking trip.  Supply the function with the number of 
#proposals.  While testing, 100,000 proposals were able to produce the desired
# maximum for the given seed.
#############################################################################
#name the function
knapsack<-function(x){

#set the seed for reproducibility
set.seed(23)

#Enter the list of items, weights, and values
item=c("map","compass","water","sandwich","glucose","tin","banana","apple","cheese", 
	"beer","suntan_cream","camera","T-shirt","trousers","umbrella", 
	"waterproof_trousers","waterproof_overclothes","note-case","sunglasses","towel", 
	"socks","book")
weight=c(9,13,153,50,15,68,27,39,23,52,11,32,24,48,73,42,43,22,7,18,4,30)
value=c(150,35,200,160,60,45,60,40,30,10,70,30,15,10,40,70,75,80,20,12,50,10)
V=400

#initialize all of the objects that the program requires
i=1
l=1
selection<-sample(c(rep(0,11),rep(0,11)))
best_selection=c(1,1,1,1,1,0,1,0,0,0,1,0,0,0,0,1,1,1,1,0,1,0)
candidates<-list();benefits<-list();total_weight<-list()
selection_new<-selection
chooseitem<-c(1:22)

#Selection Loop: here we repeatedly propose and accept/reject scenarios to take
#on the trip
for(m in 1:x){

#here we tune the algorithm to help produce more exploration in the cases where
#the chain gets stuck
	if(m/i>500){l<- .15}
	if(m/i<=500){l<- 1}

#Generation/Proposal Step: Here we randomly select a bit to flip
j<-sample(chooseitem,1)
selection_new<- selection
	if(selection[j]==1){selection_new[j]<-0}
	if(selection[j]==0){selection_new[j]<-1}

#Accept/Reject Step: Here we either advance or fail to advance our Markov
#chain based on the acceptance criteria that the weight be less than V
#and the choice meets the Metropolis-Hasting criteria.
	if(sum(weight*selection_new)>V){selection<-selection}
	if(sum(weight*selection_new)<=V &&
	rbinom(1,1,min(1,exp(l*(sum(selection_new*value)-sum(selection*value)))))==1){
	selection<-selection_new;
	candidates[[i]]<-selection_new;
	benefits[[i]]<-sum(value*selection_new);
	total_weight[[i]]<-sum(weight*selection_new);
	i<-i+1	
	}
}

#produce a plot of the markov chain to show that the algorithm is working as intended
plot(unlist(benefits))
lines(unlist(benefits))

#output the information regarding the max.
cat("The max value is ",max(unlist(benefits)),"\n")
cat("This value occurred at index: ",which.max(unlist(benefits)),"\n")
cat("The number of candidates generated is: ",length(benefits),"\n")
cat("The optimal selection string is: ",best_selection,"\n")
cat("Our selection string is: ",candidates[[which.max(unlist(benefits))]],"\n")
cat("The difference between our selection and the optimal selection is: 
",sum(abs(best_selection-candidates[[which.max(unlist(benefits))]])), "items","\n")

}

#call the function
knapsack(100000)

#The max value is  1030 
#This value occurred at index:  122 
#The number of candidates generated is:  200 
#The optimal selection string is:  1 1 1 1 1 0 1 0 0 0 1 0 0 0 0 1 1 1 1 0 1 0 
#Our selection string is:  1 1 1 1 1 0 1 0 0 0 1 0 0 0 0 1 1 1 1 0 1 0 
#The difference between our selection and the optimal selection is:  0 items 
#   user  system elapsed 
#   3.583   0.030   3.646