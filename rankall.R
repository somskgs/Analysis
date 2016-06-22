##function called rankall that takes two arguments: an outcome name (outcome) and a hospital rank-
##ing (num). The function reads the outcome-of-care-measures.csv ##le and returns a 2-column data frame containing the hospital in each state that has the ranking speci##ed in num. For example the function call
##rankall("heart attack", "best") would return a data frame containing the names of the hospitals that
##are the best in their respective states for 30-day heart attack death rates. The function should return a value
##for every state (some may be NA). The ##rst column in the data frame is named hospital, which contains
##the hospital name, and the second column is named state, which contains the 2-character abbreviation for
##the state name. Hospitals that do not have data on a particular outcome should be excluded from the set of
##hospitals when deciding the rankings.
##Handling ties. The rankall function should handle ties in the 30-day mortality rates in the same way
##that the rankhospital function handles ties.
##The function should use the following template.

selection<-function(ch,data,num){

newdata<-data[data$State==ch,]
		ranks<-rank(newdata[,3],ties.method='first') #rank function creates ranks to the data column which you want to evaluate
		newdata<-cbind(newdata,ranks) #binding the ranks list to the table
		newdata$Hospital.Name<-as.character(newdata$Hospital.Name)
		newdata$State<-as.character(newdata$State)
		# Below code checks if the user wanted result of best hospital or worst or 10th best or 7th best etc and prints result as per the same
		if(num=="best")
		{

			return(c(newdata[newdata$ranks==1,1],newdata[newdata$ranks==1,2]))
		
		}
		else if(num=="worst")
		{
			return(c(newdata[newdata$ranks==1,1],newdata[newdata$ranks==1,2]))
		}
		else if(num>nrow(newdata))
		{
			print("NA")

		}
		else{
			return(c(newdata[newdata$ranks==1,1],newdata[newdata$ranks==1,2]))			
		}
}

rankall <- function(outcome,num="best")
{
	tbloutcome<-read.csv("outcome-of-care-measures.csv") # table is read
	i<-1
	# if the out come is to check heart attack this if clause gets executed	
	if(outcome=="heart attack")
	{
		hospdata<-tbloutcome[,c(2,7,11)] # Choosing related columns from table
		hospdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack<-as.numeric(as.character(hospdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)) # converting hospital factorial column to numeric
		good<-complete.cases(hospdata[,3])
		hospdata<-hospdata[good=="TRUE",] # cleaning data from NA values or non applicable values						
		hospdata<-hospdata[order(hospdata$Hospital.Name),]
		dup<-duplicated(hospdata[,2])
		StateCode<-hospdata[dup==FALSE,2]
		catch<-data.frame(matrix(NA,nrow=length(StateCode),ncol=2))
		for(i in 1:length(StateCode)){
			j<-i
			catch[i,c(1,2)]<-selection(StateCode[i],hospdata[],num)
		}		
		print(catch)		
	}
	else if(outcome == "heart failure")
	{
		hospdata<-tbloutcome[,c(2,7,17)]
		hospdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure<-as.numeric(as.character(hospdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
		good<-complete.cases(hospdata[,3])
		hospdata<-hospdata[good=="TRUE",]
		hospdata<-hospdata[order(hospdata$Hospital.Name),]
		dup<-duplicated(hospdata[,2])
		StateCode<-hospdata[dup==FALSE,2]
		catch<-data.frame(matrix(NA,nrow=length(StateCode),ncol=2))
		for(i in 1:length(StateCode)){
			j<-i
			catch[i,c(1,2)]<-selection(StateCode[i],hospdata[],num)
	 	}		
		print(catch)		
					
	}
	else if(outcome == "pneumonia")
	{
		hospdata<-tbloutcome[,c(2,7,23)]
		hospdata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia<-as.numeric(as.character(hospdata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
		good<-complete.cases(hospdata[,3])
		hospdata<-hospdata[good=="TRUE",]
		hospdata<-hospdata[order(hospdata$Hospital.Name),]
		dup<-duplicated(hospdata[,2])
		StateCode<-hospdata[dup==FALSE,2]
		catch<-data.frame(matrix(NA,nrow=length(StateCode),ncol=2))
		for(i in 1:length(StateCode)){
			j<-i
			catch[i,c(1,2)]<-selection(StateCode[i],hospdata[],num)
	 		
		}		
		print(catch)		
	}
	else{
		print("No such outcome exists ")
	}	
}