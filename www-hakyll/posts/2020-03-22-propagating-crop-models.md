---
title: Propagating Crop Models
tags: thermal-time, simulation, evapotranspiration
---
I worked with [CropLogic](/cv#croplogic). Their product then was website for
farmers. They've since changed the product. I don't know much about what
they're up to now. The initial work with the crop model I did more than 10
years ago.

The model was developed by a Crown Research Institute in Lincoln, New Zealand.
We were given code in Borland C++. Porting to C# was quick but it took the
modeller onsite at the institute with domain knowledge to find the bugs I'd
introduced. Some of these were to do with differences in programming languages
and some were to do with units of measure. There are many different ways of
quantifying acreage, yields, irrigation flows and fertilizer applications.

The science involves running field experiments, gathering and analysing data,
fitting curves and using those equations of fit inside the models. Models have
components for soil and components for parts of the plant. Water, insolation
and nutrients are input directly or indirectly modelled themselves. I worked
closely with a modeller and an agronomist at the institute.

Equations for evapotranspiration are well known. Zoom out enough and most
models will look similar if one was to draw a logical diagram of how they
worked.  Some of the published code I've found has mixed licensing models, open
and free for scientific collaboration but any commercial use must be negotiated
separately.  So what is out there and how is it licensed?
