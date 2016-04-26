# Create a grid of male looks and female looks.
match.prd <- expand.grid(MaleLooks = sort(unique(df$SenderLooks)),
                           FemaleLooks = sort(unique(df$ReceiverLooks)))
# Create newdata for the predictions, first for male senders, then female.
match.prd %>%
  select(SenderLooks=MaleLooks, ReceiverLooks=FemaleLooks) %>%
  mutate(SenderGender=factor("male", c("female", "male"))) -> newdata.m
match.prd %>%
  select(SenderLooks=FemaleLooks, ReceiverLooks=MaleLooks) %>%
  mutate(SenderGender=factor("female", c("female", "male"))) -> newdata.f

# Make the y_hat predictions and calculate match score.
match.prd$yhat.m <- predict(mdl.contact, type="response", newdata=newdata.m)
match.prd$yhat.f <- predict(mdl.contact, type="response", newdata=newdata.f)
match.prd$score <- sqrt(match.prd$yhat.m * match.prd$yhat.f)