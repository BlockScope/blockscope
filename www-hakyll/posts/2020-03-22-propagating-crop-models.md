---
title: Propagating Crop Models
tags: thermal-time, simulation, evapotranspiration
---
I worked with [CropLogic](/cv#croplogic) between 2008/11. Their product then
was a website for farmers. They've since changed the product. I don't know much
about what they're up to now. The initial work with the crop model I did more
than 10 years ago. This was developed by a Crown Research Institute in Lincoln,
New Zealand.  I worked closely with
a [modeller](https://www.researchgate.net/profile/Robert_Zyskowski) and an
[agronomist](https://www.researchgate.net/profile/Hamish_Brown) from the
institute, Rob and Hamish.

Porting the guts of the model from C++ to C# was quick but I needed help from
Rob with his domain knowledge to find the bugs I'd introduced.  Some of these
were to do with differences in programming languages and some were to do with
units of measure and the many different ways of quantifying acreage, yields,
irrigation flows and fertilizer applications.

The science involves running field experiments, gathering and analysing data,
fitting curves and using those equations of fit inside the models. Models have
components for soil and components for parts of the plant. Water, insolation
and nutrients are input directly or indirectly, like with thermal time.

Equations for evapotranspiration are well known and will be part of every crop
model. What about whole models? Zoomed out enough, will they look logically
similar? What's out there and how do they compare?

# AgMIP

    Agricultural Model Intercomparison and Improvement Project

This is for comparing models, sharing data and converting between data formats.

# APSIM

    Agricultural Systems Modelling and Simulation

A [custom
license](https://github.com/APSIMInitiative/ApsimX/blob/master/LICENSE.md) is
needed for any use but this is free for non-commercial use. The
[code](https://github.com/APSIMInitiative/ApsimX) is C#.

# DSSAT

    Decision Support System for Agrotechnology Transfer

This is BSD-3 licensed. The [code](https://github.com/DSSAT/dssat-csm-os) is Fortran.

# BioMA

    Biophysical Model Applications

Its core code in C# is licensed MIT but components can be licensed CC with
commercial exclusions.
