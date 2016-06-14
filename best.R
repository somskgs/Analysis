best<-function(state,outcome){		
	tbloutcome<-read.csv("outcome-of-care-measures.csv")
	i<-1
	if(outcome=="heart attack")
	{
		hospdata<-tbloutcome[tbloutcome$State==state,c(2,7,13)]
		good<-complete.cases(as.numeric(as.character(hospdata[,3])))
		hospdata<-hospdata[good=="TRUE",]
		minimum<-min(as.numeric(as.character(hospdata[,3])))
		print(minimum)
		while(i<length(hospdata[,3])){
			if(as.numeric(as.character(hospdata[i,3]))== minimum)
			{
				print(hospdata[i,1])	
			}
			i<-i+1
		}
	}
	else if(outcome == "heart failure")
		{
		hospdata<-tbloutcome[tbloutcome$State==state,c(2,7,19)]
		good<-complete.cases(as.numeric(as.character(hospdata[,3])))
		hospdata<-hospdata[good=="TRUE",]
		minimum<-min(as.numeric(as.character(hospdata[,3])))
		print(minimum)
		while(i<length(hospdata[,3])){
			if(as.numeric(as.character(hospdata[i,3])) == minimum)
			{
				print(hospdata[i,1])			
			}
			i<-i+1
		}
	}
	else if(outcome == "pneumonia")
		{
		hospdata<-tbloutcome[tbloutcome$State==state,c(2,7,25)]
		good<-complete.cases(as.numeric(as.character(hospdata[,3])))
		hospdata<-hospdata[good=="TRUE",]
		minimum<-min(as.numeric(as.character(hospdata[,3])))		
		print(minimum)
		while(i<length(hospdata[,3])){
			if(as.numeric(as.character(hospdata[i,3]))== minimum)
			{
				print(hospdata[i,1])
			
			}
			i<-i+1
		}
	}
	else{
		print("No such outcome exists")
	}	
}