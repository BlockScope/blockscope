---
title: Propagating Crop Models
tags: thermal-time, simulation, evapotranspiration
---
I worked with CropLogic_ between 2008/11. Their product then was a website for
farmers. They've since changed the product. I don't know much about what
they're up to now. The initial work with the crop model I did more than 10
years ago. This was developed by a Crown Research Institute in Lincoln, New
Zealand.  I worked closely with a modeller_ and an agronomist_ from the
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

AgMIP
_____

    Agricultural Model Intercomparison and Improvement Project

This is for comparing models, sharing data and converting between data formats
[#]_.

$$DSSAT \\leftarrow AgMIP \\leftrightarrow APSIM$$

AMEI
____

    Agricultural Model Exchange Initiative

Their Crop2ML_ is an XML format for describing models.

APSIM
_____

    Agricultural Systems Modelling and Simulation

An `APSIM custom license`_ is needed for any use but this is free for
non-commercial use. `APSIM code`_ is C#.

DSSAT
_____

    Decision Support System for Agrotechnology Transfer

This is BSD-3 licensed. The `DSSAT code`_ is Fortran.

BioMA
_____

    Biophysical Model Applications

Its core code in C# is licensed MIT but components can be licensed CC with
commercial exclusions.

OpenAlea
________

OpenAlea_ is a software environment for plant modelling in Python with a core
licensed CeCILL-C_ and external packages free to specify their own licenses.
Last release was made 2010.

.. _CropLogic: /cv#croplogic

.. _modeller: https://www.researchgate.net/profile/Robert_Zyskowski

.. _agronomist: https://www.researchgate.net/profile/Hamish_Brown

.. _APSIM code: https://github.com/APSIMInitiative/ApsimX

.. _DSSAT code: https://github.com/DSSAT/dssat-csm-os

.. _APSIM custom license: https://github.com/APSIMInitiative/ApsimX/blob/master/LICENSE.md

.. _Crop2ML: https://github.com/AgriculturalModelExchangeInitiative/Crop2ML

.. _OpenAlea: http://openalea.gforge.inria.fr

.. _CeCILL-C: https://en.wikipedia.org/wiki/CeCILL

.. [#] Conversion from APSIM to AgMIP is partial, weather and soil only.
