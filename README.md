# OCRUG x MAC Data Science Hackathon
## Predictive Modeling in Term Deposit Subscription and Variable Relationships

Attached you will find the provided data set and R code used to run the predictive model. If you have any questions or find areas for improvements, feel free to reach out. 
___
**Team Math Geeks + Ryan:**

Ryan Luu <br>
Kent Jitpatima <br>
Sunny Zhou <br>
Vu Tran <br>
Alex Ordonez
___
**Conclusions**
* Age group 1 (18-29) and 3 (60+) are more willing to subscribe to a term deposit.
* Those who sucessfully subsribed in the previous marketing campaign are much more likely to subcribe for a term deposit.
* The probability of a client declining to a subcription significantly drops after 8 attempts.
* With this information, we hope the callers strategically spend their call times onto the target audience, thus efficiently utilizing the time and resources of the company.

**Limitations of our Analyses**
* The predictive model we built in determining whether a client will subscribe to a bank term deposit may not be accurate in 2019 because Portugal's 2019 economy heavily differs than that of its 2008-2010 economy, which was during and near the Great Recession.
* Due to an imbalance of classifiers, we used synthetic data to better train the random forest model. Our original model without synthetic data had a 91% overall accuracy but more than 50% false positive rate. Our improved model sported a 87% overall accuracy but had a 20% false positive rate. Despite the lower overall accuracy, our new model showed to be more well-rounded in class error. Overall accuracy is sometimes not the best indicator of a model's performance. 
* Although synthesizing data for our classification model leads to no information loss, we increased the risk of overfitting the model since it replicates the minority class events

**Acknowledgments**
<br> Shout out to John Peach, Ryan Benz, and Emil Hvitfeldt Hansen of OCRUG for helping us get through our first hackathon and guiding us in the right direction on how to approach the dataset. 
