# CodeBook

Data dictionary of the project output data, which consists of 68 variables of 180 observations.  Each observation represents a specific activity performed by one subject.  Except the first two, all the rest variable namses are the combinations of abbreviated codes to indicate the measurment.  The names consists of 5 parts: "\<signal domain\>\<acceleration type\>\<meter type\>\_\<measurement type\>\<signal information\>\_\<measurement axis\>."
* \<signal domain\>: t - time; f - frequency
* \<acceleration type\>: Body - body acceleration; Gravity - gravity acceleration
* \<meter type\>: Acc - accelerometer; Gyro: gyroscope
* \<measurement type\>: mean - mean value; std: standard deviation
* \<measurement dirction\>: 3 dimensional axis X,Y,Z
* \<signal information\>: Jerk: jerk signal; Mag - signal magnitude

For example, the "tBodyGyroJerkMag_mean" indicates "the avarage magnitude of the jerk signal of body acceleration time measured by gyroscope."
Note all these variables are the **mean value** of multiple observations of the activity performed by the same subject.

1. Activity: Type of activity performed 
  * WALKING
  * WALKING_UPSTAIRS
  * WALKING_DOWNSTAIRS
  * SITTING
  * STANDING
  * LAYING
2. Subject: Subject of the observation
  * Number, 1 to 30
3. tBodyAcc_mean_X
4. tBodyAcc_mean_Y
5. tBodyAcc_mean_Z
6. tBodyAcc_std_X
7. tBodyAcc_std_Y
8. tBodyAcc_std_Z
9. tGravityAcc_mean_X
10. tGravityAcc_mean_Y
11. tGravityAcc_mean_Z
12. tGravityAcc_std_X
13. tGravityAcc_std_Y
14. tGravityAcc_std_Z
15. tBodyAccJerk_mean_X
16. tBodyAccJerk_mean_Y
17. tBodyAccJerk_mean_Z
18. tBodyAccJerk_std_X
19. tBodyAccJerk_std_Y
20. tBodyAccJerk_std_Z
21. tBodyGyro_mean_X
22. tBodyGyro_mean_Y
23. tBodyGyro_mean_Z
24. tBodyGyro_std_X
25. tBodyGyro_std_Y
26. tBodyGyro_std_Z
27. tBodyGyroJerk_mean_X
28. tBodyGyroJerk_mean_Y
29. tBodyGyroJerk_mean_Z
30. tBodyGyroJerk_std_X
31. tBodyGyroJerk_std_Y
32. tBodyGyroJerk_std_Z
33. tBodyAccMag_mean
34. tBodyAccMag_std
35. tGravityAccMag_mean
36. tGravityAccMag_std
37. tBodyAccJerkMag_mean
38. tBodyAccJerkMag_std
39. tBodyGyroMag_mean
40. tBodyGyroMag_std
41. tBodyGyroJerkMag_mean
42. tBodyGyroJerkMag_std
43. fBodyAcc_mean_X
44. fBodyAcc_mean_Y
45. fBodyAcc_mean_Z
46. fBodyAcc_std_X
47. fBodyAcc_std_Y
48. fBodyAcc_std_Z
49. fBodyAccJerk_mean_X
50. fBodyAccJerk_mean_Y
51. fBodyAccJerk_mean_Z
52. fBodyAccJerk_std_X
53. fBodyAccJerk_std_Y
54. fBodyAccJerk_std_Z
55. fBodyGyro_mean_X
56. fBodyGyro_mean_Y
57. fBodyGyro_mean_Z
58. fBodyGyro_std_X
59. fBodyGyro_std_Y
60. fBodyGyro_std_Z
61. fBodyAccMag_mean
62. fBodyAccMag_std
63. fBodyAccJerkMag_mean
64. fBodyAccJerkMag_std
65. fBodyGyroMag_mean
66. fBodyGyroMag_std
67. fBodyGyroJerkMag_mean
68. fBodyGyroJerkMag_std
