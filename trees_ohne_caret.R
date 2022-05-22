```{r tree dataset1}
cart_ds1 <- rpart(Sepsis ~ .-X-Patient_ID-Hour , data = train1,
                  control = rpart.control(minsplit = 15,
                                          minbucket = 5, 
                                          cp = 0))

rpart.plot(cart_ds1)

cart_ds1_2 <- prune(cart_ds1, cp = 0.0025)
rpart.plot(cart_ds1_2)
```

```{r tree dataset2}
cart_ds2 <- rpart(Sepsis ~ .-X-Patient_ID-Hour-ICULOS , data = train2,
                  control = rpart.control(minsplit = 15,
                                          minbucket = 5, 
                                          cp = 0))

rpart.plot(cart_ds2)

cart_ds2_2 <- prune(cart_ds2, cp = 0.004)
rpart.plot(cart_ds2_2)


ci_ds2 <- ctree(Sepsis ~ ., 
                data = subset(train2, select = -c(X, Hour, Patient_ID))
                # data =train2
)
ci_ds2
plot(ci_ds2)
table(predict(ci_ds2), train2$Sepsis)
```

```{r tree datase3}
cart_ds3 <- rpart(Sepsis ~ .-Patient_ID , data = train3,
                  control = rpart.control(minsplit = 15,
                                          minbucket = 5, 
                                          cp = 0))

rpart.plot(cart_ds3)

cart_ds3_2 <- prune(cart_ds3, cp = 0.01)
rpart.plot(cart_ds3_2, type = 5)
```