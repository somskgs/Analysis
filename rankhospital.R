##This function takes 3 arguments State code, Hospital  Outcome that has to be found out - whether pnuemonia, heart attack or heart failure (in which of these the hospital is ranked best worst or 7th best etc)
## & the third argument is the number argument , whether to find 7th best or 10th best or best or worst

rankhospital <- function(state,outcome,num="best")
{
	tbloutcome<-read.csv("outcome-of-care-measures.csv") # table is read
	i<-1
	check<-0
	for(i in 1:nrow(tbloutcome)){			
		if(tbloutcome[i,"State"]==state)	
		{						# in this loop we are validating if state code exists
			check<-1
			break
		}
	}
	if(check!=1)
	{
		print("No Such State Exists")			# Error returned if it does not exist
	}

# if the out come is to check heart attach this if clause gets executed	
	if(outcome=="heart attack")
	{
		hospdata<-tbloutcome[tbloutcome$State==state,c(2,7,11)] # Choosing related columns from table
		hospdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack<-as.numeric(as.character(hospdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)) # converting hospital factorial column to numeric
		good<-complete.cases(hospdata[,3])
		hospdata<-hospdata[good=="TRUE",] # cleaning data from NA values or non applicable values						
		hospdata<-hospdata[order(hospdata$Hospital.Name),] # Ordering data as per Hospital names in alphabetical order from A to Z
		ranks<-rank(hospdata[,3],ties.method='first') rank function creates ranks to the data column which you want to evaluate
		hospdata<-cbind(hospdata,ranks) binding the ranks list to the table
		print(nrow(hospdata))	
		print(ncol(hospdata))
		# Below code checks if the user wanted result of best hospital or worst or 10th best or 7th best etc and prints result as per the same
		if(num=="best")
		{
			result<-hospdata[hospdata$ranks==1,1]
			print(result)
		
		}
		else if(num=="worst")
		{
			result<-hospdata[hospdata$ranks==nrow(hospdata),1]
			print(result)
		}
		else if(num>nrow(hospdata))
		{
			print("NA")

		}
		else{
			result<-hospdata[hospdata$ranks==num,1]
			print(result)
		}
		
	}
	else if(outcome == "heart failure")
	{
		hospdata<-tbloutcome[tbloutcome$State==state,c(2,7,17)]
		hospdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure<-as.numeric(as.character(hospdata$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
		good<-complete.cases(hospdata[,3])
		hospdata<-hospdata[good=="TRUE",]
		hospdata<-hospdata[order(hospdata$Hospital.Name),]
		ranks<-rank(hospdata[,3],ties.method='first')
		hospdata<-cbind(hospdata,ranks)
		print(nrow(hospdata))
		print(ncol(hospdata))	
		if(num=="best")
		{
			result<-hospdata[hospdata$ranks==1,1]
			print(result)
		
		}
		else if(num=="worst")
		{
			result<-hospdata[hospdata$ranks==nrow(hospdata),1]
			print(result)
		}
		else if(num>nrow(hospdata))
		{
			print("NA")

		}
		else{
			result<-hospdata[hospdata$ranks==num,1]
			print(result)
		}
			
	}
	else if(outcome == "pneumonia")
	{
		hospdata<-tbloutcome[tbloutcome$State==state,c(2,7,23)]
		hospdata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia<-as.numeric(as.character(hospdata$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
		good<-complete.cases(hospdata[,3])
		hospdata<-hospdata[good=="TRUE",]
		hospdata<-hospdata[order(hospdata$Hospital.Name),]
		ranks<-rank(hospdata[,3],ties.method='first')
		hospdata<-cbind(hospdata,ranks)
		print(nrow(hospdata))
		print(ncol(hospdata))
		if(num=="best")
		{
			result<-hospdata[hospdata$ranks==1,1]
			print(result)
		
		}
		else if(num=="worst")
		{
			result<-hospdata[hospdata$ranks==nrow(hospdata),1]
			print(result)
		}
		else if(num>nrow(hospdata))
		{
			print("NA")

		}
		else{
			result<-hospdata[hospdata$ranks==num,1]
			print(result)
		}
		
		
	}
	else{
		print("No such outcome exists ")
	}	
}