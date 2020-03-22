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

Equations for evapotranspiration are well known. Zoom out enough and many crop
models will look similar if one was to draw a logical diagram of how they
worked.  There's more code out in the open these days and much of it can be
found easily centralized on github or the like. Some of the published code I've
found has mixed licensing models, open and free for scientific collaboration
but any commercial use must be negotiated separately.  So what is out there and
how is it licensed?
