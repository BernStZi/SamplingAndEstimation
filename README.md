---
output: html_document
---
# Sampling and Estimation
## A lecture with tutorial about sampling and estimation in the field of sample surveys. 

### Lecture 
The lecture is split into three part.

Part 1 ([Browser](https://github.com/BernStZi/SamplingAndEstimation/blob/short/lecture/part_1.pdf) | 
[Download](https://github.com/BernStZi/SamplingAndEstimation/raw/short/lecture/part_1.pdf))
covers the concept of sampling from a finite population. The depiction of estimation and inference is driven  **sampling design** that was used in selecting the sample. **Simple random sampling** is explained as one of the most basic sampling designs. Then **stratfication** of the surveyed population is introduced as one of the most common elements of sampling designs.

Part 2 ([Browser](https://github.com/BernStZi/SamplingAndEstimation/blob/short/lecture/part_2.pdf) | 
[Download](https://github.com/BernStZi/SamplingAndEstimation/raw/short/lecture/part_2.pdf))
introduces features of more **complex sampling designs** that however find often application in sampling surveys.
These features include **cluster sampling**, **mutlistage sampling** and **sampling with unequal inclusion probabilities**.
**Design effects** are also discussed as a measure of comparing the efficiency of sampling and estimation strategies.

Part 3 ([Browser](https://github.com/BernStZi/SamplingAndEstimation/blob/short/lecture/part_3.pdf) | 
[Download](https://github.com/BernStZi/SamplingAndEstimation/raw/short/lecture/part_3.pdf))
focuses of **survey weights** for the estimation of population statistics. Their usage is motivated to both reduce sampling and non-sampling error with the help of auxiliary information. Three methods to produce survey weights are presented, **post-stratification**, **raking** and the **generalized regression estimator**.
The implications of using survey on doing statistical inference are also shown.

### Tutorial

The tutorial consists of exercises in which students will apply the methodology covered in the lecture using the R language. The exercise is also split into three parts:

Part 1 ([Browser](https://github.com/BernStZi/SamplingAndEstimation/blob/short/tutorial/Ex1.pdf) | 
[Download](https://github.com/BernStZi/SamplingAndEstimation/raw/short/tutorial/Ex1.pdf)) sets its focus on the application of basic R commands, such as loading and inspecting a data set. Furthermore the R package **survey** will be introduced and an introduction on how to define **survey objects** will be given. The first exercise focuses on estimation within **simple random sample**, using the survey package and the data of the 5th ESS-Round for Sweden.

Part 2 ([Browser](https://github.com/BernStZi/SamplingAndEstimation/blob/short/tutorial/Ex2.pdf) | 
[Download](https://github.com/BernStZi/SamplingAndEstimation/raw/short/tutorial/Ex2.pdf))  will provide a further insight in the application of the **survey package** and **stratified sampling**. The focus will be on estimation with stratified samples and the use of different allocations, like **proportional**, **equal** and **optimal** allocation.

Part 3 ([Browser](https://github.com/BernStZi/SamplingAndEstimation/blob/short/tutorial/Ex3.pdf) | 
[Download](https://github.com/BernStZi/SamplingAndEstimation/raw/short/tutorial/Ex3.pdf)) focuses on **design effects**,   **multistage samples** and **cluster samples** and the estimation of central parameters within those sampling strategies. To estimate a design effect of a genuine survey, the exercise uses the ESS data of the 5th round for Germany.
The estimation of parameters in complex sampling designs will use a previously generated sample. Participants will then learn how to specifiy such samples as a **survey object** and how to estimate parameters of interest.

### R-Intro
If you are new to R there you will find a document [here](https://github.com/BernStZi/SamplingAndEstimation/blob/short/tutorial/preparation/Preparation.md), which you might find helpful in guiding you through your first steps in R and gives you hints on where to find further information.

### Further Reading for R
A more detailed description of the utilization of R with sample surveys can be found [here](https://github.com/BernStZi/SamplingAndEstimation/tree/short/tutorial/slides).



