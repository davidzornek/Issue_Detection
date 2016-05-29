# Issue_Detection

This repository is a work in progress for a methodology to detect when adverse insurance claim experience is due to an underlying issue that should be addressed, as opposed to normal noise in the data.

Issue_Detection_Simulation.R takes real insurance claim data and transforms it into a new data set simulating a distributional shift toward a segment of business with significantly higher claim frequency.

Univariate_Issue_Detection.R begins tackling the problem in the simplest univariate case.

The remaining scripts belong to a Shiny app that allows users to understand how various affect the methodology's ability to flag changes as issues. It also provides important insights that will drive selection of an appropriate statistical significance to meet a client's needs.
