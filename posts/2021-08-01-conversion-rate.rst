---
title: How much for a developer?
subtitle: How to convert salary to contractor rate?
tags: rates
---

More Remote
-----------
I've worked as a contracting software developer since 1999, almost always
offsite but usually not far from the client so that I can drop in to visit for
meetings from time to time. I've had the occassional contract that has been
truly remote, where I've not ever met the client in person. With the pandemic
many more people are working remotely and more employers are hiring remotely.

Permanent versus Contract
-------------------------
When you're a worker with a wage or a salaried employee there will be labour
laws and union rules that cover the engagement between you and the employer. How
much will you get paid? When are you required to work? What holidays do you get
and how much sickness is covered?

When contracting, aside from the rate of payment for services there are other
things to consider negotiating on too, like obligations and termination in the
event of sickness and, for a longer contract, how to take time off. I want to
see these points listed in the schedule of a written contract. Some contracts
will not be worth taking on when the rate or conditions are not good. I've only
seen this when the intention is to hire a contractor as an employee in all but
name.

For a long time a rule of thumb I've heard of and used myself is, take an annual
salary in thousands and use that as hourly rate for contracting, so
``$100,000/yr`` converts to ``$100/hr``. Is this conversion is fair? I've seen
it fit well in England, France and New Zealand but does it fit with the United
States market?

I got offered a contract out of New York at ``$80/hr``, a city where senior
permanent dev staff with more than a decade's worth of experience get about
``$150,000/yr`` as base salary [#]_. The negotiations fell through but led me to
review rates, comparing permanent with contract.

The Other Side of the Table
---------------------------
From the other side, I found a detailed analysis of the `true cost`_ of engaging
consultants versus hiring employees::

    +------------+------------+
    | CONSULTANT |  EMPLOYEE  |
    +============+============+
    |      Hourly Rate        |
    |       $80  |  $72       |
    +------------+------------+
    |      Annual Salary      |
    |  $166,400  |  $150,000  |
    +------------+------------+
    |     REAL HOURLY COST    |
    |       $96  |  $143      |
    +------------+------------+
    |     REAL ANNUAL COST    |
    |  $199,680  |  $298,500  |
    +------------+------------+

From Revenue to Shareholder Salary
----------------------------------
When a client wants to add a contractor to a team of salaried software
developers, how to convert pay rates and conditions from permanent to
contractor? From consulting company revenue, what might the maximum possible
consultant's shareholder salary be?

::

     166,400 - asymptotic revenue (52 weeks * 40 hours/week * $80/hour)
      41,600 - 2/8th of hours are not chargeable (6/8th, 6 hours/day are)
    --------
     124,800 - maximum chargeable revenue

    What about time off (not including time looking for gigs)?
       5,000 - 10 days' sick leave
      25,000 - 25 days' vacation
       5,000 - 10 days' statutory holidays
       5,000 - 10 days' off for accounting and conference attendance
    --------
      40,000 - missed revenue because of time off
    ========
      84,800 - chargeable revenue for time on
      10,000 - business expenses (office, hardware, utilities, insurances)
    --------
      74,800 - shareholder salary (assuming no company profit)

    What does a contractor miss out on in benefits?
      15,000 - missed equity (hard to put a figure on)
      15,000 - missed 10% hack time (included in salary but not in contract)
      15,000 - missed 10% retirement (401K)
       5,000 - missed health insurance
    --------
      50,000 - missed benefits ($35,000 on top of salary)

So there you have it, the permanent employee gets effectively ``$185,000`` in
salary and benefits whereas the contractor might get ``$75,000``.

Update for 2021
---------------

To quote my insurance broker:

  The world of insurance has been rocked since covid19 with skyrocketing premiums
  in the areas of liability insurances.  For many of our clients we actually
  cannot get professional indemnity cover at all, e.g. if they are in the
  financial advice industry or are a manufacturer.  Particularly having cover
  where claims can originate in the USA is the hardest of the lot.

The cost of worldwide indemnity cover for any territory and jurisdiction has doubled to
``$4,000/yr``, reducing shareholder salary.

.. code-block:: diff

    --- 2020
    +++ 2021
      84,800 - chargeable revenue for time on
    - 10,000 - business expenses (office, hardware, utilities, insurances)
    + 12,000 - business expenses (office, hardware, utilities, insurances)
    --------
    - 74,800 - shareholder salary (assuming no company profit)
    + 72,800 - shareholder salary (assuming no company profit)

Excluding USA/Canada from the territories and using New Zealand as jurisdiction
can get indemnity cover for the rest of the world down to ``$1,200/yr``. A
litigious society is way more costly.

.. _true cost: https://www.toptal.com/freelance/don-t-be-fooled-the-real-cost-of-employees-and-consultants

.. _city rates: https://www.indeed.com/career/software-engineer/salaries/New-York--NY
.. _state rates: https://www.indeed.com/career/software-engineer/salaries/NY

.. [#] Average base salary for ``10+`` years experience:

           - ``$155,440`` New York `city rates`_.
           - ``$133,585`` NY `state rates`_.
