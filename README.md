# GenOrder
Library that models medical orders and allows calculation and planning of those orders

## Build Status

Mono | .NET | NuGet|
---- | ---- | ---- |
[![Mono CI Build Status](https://img.shields.io/travis/halcwb/GenOrder/master.svg)](https://travis-ci.org/halcwb/GenOrder) | [![.NET Build Status](https://img.shields.io/appveyor/ci/halcwb/GenOrder/master.svg)](https://ci.appveyor.com/project/halcwb/GenOrder) | [![NuGet Status](http://img.shields.io/nuget/v/Informedica.GenOrder.Lib.svg?style=flat)](https://www.nuget.org/packages/Informedica.GenOrder.Lib/)


## Background

Medical treatment in general consists of a set of orders. One could view the content of an order as an 'Orderable'. Orders have to be prescribed and administered. Orders can be prescribed in 4 different ways:

1. As a continuous process
2. As a continuous administration
3. As a discontinuous administration
4. As a timed discontinuous administration

This library aims to facilitate calculation and planning of orders.

### Continuous process

For example a drip line can be viewed as an order process. It has a start and a stop. But for the rest there is no calculation involved.

### Continuous administration

For example infusion of a drug using a infusion pump. Often the infusion rate is determined according to the drug concentration of a substance. This has to be calculated. Also, there is a start date and stop date and depending on the amount of drug and the infusion rate, the number of drug preparations can be calculated. Also, the drug preparation itself involves calculations.

### Discontinuous administration

A drug can also be administered discontinuously. For example, *paracetamol 2 dd 500 mg*. This involves both quite complex calculation and planning. Especially in pediatrics, dose calculations are important and can pose difficulties.

### Timed discontinuous administration

A timed discontinuous administration is an order that has both a frequency and a duration. For example, repeated drug infusions over a day. Again this involves both dose calculations and calculation of instances, totals, etc... Also, planning can be complex.

