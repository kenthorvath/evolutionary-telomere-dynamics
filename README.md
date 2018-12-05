# Evolutionary Telomere Dynamics
Author: Kent Horvath, MD PhD <kenthorvath@gmail.com>  
Date: October 6, 2016  
GitHub Repository: https://github.com/kenthorvath/evolutionary-telomere-dynamics  
License: MIT (see LICENSE file)  

## Description: 
Here we provide a Monte Carlo simulation to model evolutionary telomere dynamics across generations. For an in depth description of epidemiological and theoretical considerations of our model, please refer to [1].

## Requirements:
The following open source libraries and packages are required for compiling and running this software. Installation instructions are provided for MacOS via [homebrew](https://brew.sh).
  - Java Runtime Environment (via: `brew cask install java`)
    - tested with Open JDK Runtime Environment 18.9
  - sbt 1.2.6+ (via: `brew install sbt`)
  - scala 2.12 (via: `brew install scala`)

## Compile:
Compile the project from the project root directory using `sbt assembly`. 

## Run:
Run the Simulator from the project root via:
`scala -cp ./target/scala-2.12/pac-effect-assembly-1.0.jar Simulator <pacAgeCenter> <crossOverYear> <brinkEffect> <cancerIncidenceAdjustment> <maternalInheritance> <initialPopulationTL> <runLength> <numberOfTrials> <outputFileName>`

### Parameters:
  - `<pacAgeCenter>` (`None` or a `Double`): PAC in years above which TL is added, below which subtracted
  - `<crossOverYear>` (`None` or an `Int`): Year that PAC model is switched on during simulation
  - `<brinkEffect>` (`Boolean`): Whether or not to include a brink effect in the model
  - `<cancerIncidenceAdjustment>` (`Double`): Scaling factor for cancer incidence (0.0 -> implies no cancer incidence)
  - `<maternalInheritance>` (`Double`): Weight of inherited maternal birth TL (range -> 0.0 to 1.0)
  - `<initialPopulationTL>` (`Int`): Given in base pairs
  - `<runLength>` (`Int`): Given in years
  - `<numberOfTrials>` (`Int`): Must be greater than or equal to 1
  - `<outputFileName>` (`String`): path to write CSV output

Example: `scala -cp ./target/scala-2.12/pac-effect-assembly-1.0.jar Simulator 34.2 500 True 1.0 0.5 10000 1000 1 output.csv`

## References
1. [Paternal Age and Transgenerational Telomere Length Maintenance: A Simulation Model](http://doi.org/10.1038/s41598-018-36923-x), 
    Horvath, K. Eisenberg, D., Stone, R., Anderson, J., Kark, J., Aviv, A., Nature Scientific Reports (accepted)
