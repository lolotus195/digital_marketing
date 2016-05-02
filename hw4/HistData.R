# Digital and Algorithmic Marketing
# Instructor: Sanjog Misra
# Historical Data for Project

# load data
load(file = "Historical_Data.rdat")
# Loads a list calles histdat
# The list contains 318 elements
length(histdat)
# Each element is a matrix that gives you the results on an experiment that was run.
# For example the 11th element is a experiment with 48 messages
# The first set of variables (named V1,V2, etc.) correspond to message elements
# Unique_Clicks are responses and Unique_Sent are the number of emails sent.
histdat[[11]]

# Different experiments may have different sets of message elements
# They will also have different sample sizes
# However, the variable names are consistent. So V1 in experiment 11
# is the same as V1 in experiment 35.
# In total there are 9 message elements and there will be the same 9 elements in the 
# upcoming experiments related to the project.

# By next Wednesday (May 04) please upload a csv file 
# with the first 9 columns labeled (V1,V2,...,V9) (all caps)
# there should be a 10th column called N 
# Each row in your csv file should correspond to a message
# That is the level of the message element V1 through V9.
# N for each row should be the same and should reflect the number of 
# emails you wish to send out for each message.
# I have placed a shell csv file with the appropriate 
# formatting in the project folder.


# Computing your profit score
# Each message campaign costs the client $200
# So if you have 32 messages it will cost your client $6400 to set up and mail these.
# The number of emails doesnt impact your cost (apart from the opportunity cost).

# Your total project profitability will be calculated as follows

profit = function(unique_clicks,ncampaigns,other)
{
	unique_clicks*.1 - 200*ncampaigns - other
} 

# so if you send out 5000,000 emails and got a 10% response
# and you tested 64 messages in the first experiment
# and 32 messages in the second experiment
# and you purchased $5000 worth of other data
# you would have made the client
profit(5000000*.1,96,5000)
# $25,800

# If for example the control reponse rate was 3% and you decided to jsut 
# go with that, do no experimentation and bought no datathe client would make
profit(5000000*.03,0,0)
# $15,000


