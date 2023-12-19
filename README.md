# Autism Bill USA

## Objective

This project aims to provide a survival analysis to determine the variables and the duration required for the approval of a specific autism related bill on USA states.


## Justification

Autism Spectre Disorder (ASD) is a multi-diverse neurodevelopmental disorder that usually emerges during childhood, and persists during adolescence and into adulthood. On most cases, the condition begins to be apparent on the first five years of life, which can lead to difficulties in socialization and communication, as well as tendencies toward repetitive and restricted behavior.

A study conducted in the United States detected a notable rise in ASD diagnostics through out the country recently, which caused state governors to act on it with new state policies, providing help and care to ASD individuals. However, the US currently has 50 states, all of which are vastly different in many regards, causing them to react differently to this situation.


With all this in mind, political science Professor Denilson (IPOL/REL-UnB) reached out to our statistics professor Juliana Fachini (CIC/EST-UnB), asking for help to analyze the data regarding a specific autism related bill. As an expert in Survival Analysis, professor Fachini suggested this project to our group, sharing her knowledge and guiding us in this task.


## Choices and Metodology

Our dataset was provided by both professors, and we were free to shape it as we please. There wasn't much necessity in data cleaning, aside from removing missing values. However, we did a lot of data transformation, from creating new variables, data categorization, adjusting the dataset to work on the different types of plots we used, and much more.

All of the statistical theory regarding survival analysis techniques we used is present on the "Relatorio\_Final-Grupo6.Rmd" file. Note that this file is capable of generating a PDF file through the use of RStudio's RMarkdown, making the project much more readable. Just make sure to have all related files in the same folder as the .Rmd, and change user specific values within the code, such as the working directory.

At the end of the project, we were able to identify two variables as the most significant in predicting the approval of said bill: "Citizen Ideology" and "Government Ideology". Even so, the ideal predictive model consists only of the "Government Ideology" variable, granted it was the most significant of them, and the fact that they are very correlated causes multicollinearity issues on a model containing both.


## License and Contact

This project was created by Matheus Erbisti, Bruno Brandão, Rafael Araruna and Ramon Moreira, with guidance from our college professors Juliana Fachini and Denilson. It falls under the MIT License, which means you are free to use and adapt our code at your will, just make sure to reference us!

If you need to make contact about this project, you can reach out to me on LinkedIn (https://www.linkedin.com/in/matheus-erbisti-b74168172/) or via e-mail at matheuserbisti@hotmail.com.


Thank you for your attention!


