---
title: Phil de Joux
---

### Work History

<div id="myPlot"></div>

* `2014/02-2017/04` [**Aqualinc Research**](http://www.aqualinc.co.nz)  
Develop a website for monitoring on farm measurements for regulated level
breaches.
* `2012/07-2012/12` [**Tagly**](https://angel.co/company/tagly)  
Develop server and browser components of a live feed.
* `2011/04-2012/04` [**Travieo**](http://www.travieo.com)  
Develop a travel booking website.
* `2008/02-2011/09` [**CropLogic**](http://www.croplogic.com)  
Work with scientists tuning and testing a discrete event simulation crop model.
Work with the product team developing a website for potato growers. Pull in
external weather and field observations. Provide a way for growers to setup
their crops, to enter their irrigation and fertilizer applications and to view
the model recommended future inputs and expected yields.
* `2007/12-2010/04` [**Waimakariri District Council**](http://www.waimakariri.govt.nz)  
Develop a website for logging public submissions and scheduling hearing time
slots.
* `2003/07-2008/05` [**NutriCentre**](http://www.nutricentre.com)  
Develop an online store.
* `2003/06-2007/07` [**NIWA**](http://www.niwa.co.nz)  
Develop tools and the [EDENZ](http://edenz.niwa.co.nz) website for publishing
environmental time series data.
* `2001/08-2002/09` **Aspelle**  
Develop authentication and authorization parts of a security product.
* `1999/10-2001/03` [**Obvious Technology**](https://angel.co/company/obvious-technology)  
Develop a product for searching video, hitting on annotated key frames.
* `1999/07-1999/09` [**Software Migrations**](http://www.smltd.com)  
Develop a frontend for tools translating mainframe assembly code modules to C.
* `1995/02-1999/01` [**Trimble**](http://www.trimble.com)  
Maintain the inhouse computer graphics library, improving clipping and
multithreading.

### Tech to Favour

* Languages: F# and Haskell.
* XML: XML and XSD.
* SQL: SQL and LINQ to SQL.
* Build: Paket, Shake, Bazel and Stack.
* COMMS: JSON.
* Source Control and Bug Tracking: Mercurial, Git, GitHub, GitLab and FogBugz.
* Environment: NixOS and NixPkgs.
* Frameworks and Platforms: Reflex-FRP.

### Tech to Shun

* Languages: Pascal, C++, VB.NET and Elm.
* XML: XSLT.
* SQL: Reporting Services.
* Build: NAnt, Mage, ClickOnce, Nuget and Cabal.
* COMMS: SOAP and WCF.
* Source Control and Bug Tracking: ClearCase, ClearQuest, Jira.
* Environment: virtual machines and containers.
* Frameworks and Platforms: Win16, Win32, Winsock, COM, DirectShow, Quicktime,
  MFC, ADSI, ASP.NET, Winforms, GDI+, WiX, Commerce Server, ASMX, ASHX and WPF.

## Education

Computer Graphics 1996, Mathematical Modeling BSc Hons 1993/5, Medicine 1982/5

<script src="//unpkg.com/timelines-chart@2"></script>
<script>
    function getRandomData(ordinal = false) {

        const NGROUPS = 6,
            MAXLINES = 15,
            MAXSEGMENTS = 20,
            MAXCATEGORIES = 20,
            MINTIME = new Date(2013,2,21);

        const nCategories = Math.ceil(Math.random()*MAXCATEGORIES),
            categoryLabels = ['A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z'];

        return [...Array(NGROUPS).keys()].map(i => ({
            group: 'group' + (i+1),
            data: getGroupData()
        }));

        //

        function getGroupData() {

            return [...Array(Math.ceil(Math.random()*MAXLINES)).keys()].map(i => ({
                label: 'label' + (i+1),
                data: getSegmentsData()
            }));

            //

            function getSegmentsData() {
                const nSegments = Math.ceil(Math.random()*MAXSEGMENTS),
                    segMaxLength = Math.round(((new Date())-MINTIME)/nSegments);
                let runLength = MINTIME;

                return [...Array(nSegments).keys()].map(i => {
                    const tDivide = [Math.random(), Math.random()].sort(),
                        start = new Date(runLength.getTime() + tDivide[0]*segMaxLength),
                        end = new Date(runLength.getTime() + tDivide[1]*segMaxLength);

                    runLength = new Date(runLength.getTime() + segMaxLength);

                    return {
                        timeRange: [start, end],
                        val: ordinal ? categoryLabels[Math.ceil(Math.random()*nCategories)] : Math.random()
                        //labelVal: is optional - only displayed in the labels
                    };
                });

            }
        }
    }
</script>
<script>
    TimelinesChart()
    .data(getRandomData(true))
    .zQualitative(true)
    (document.getElementById('myPlot'));
</script>
