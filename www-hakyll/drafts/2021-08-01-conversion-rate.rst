---
title: Conversion Rate
subtitle: How to convert salary to contractor rate?
slug: How much for a developer?
tags: rates
---

When you're a worker with a wage or a salaried employee there will be labour
laws and union rules that cover the engagement between you and the employer. How
much will you get paid? When are you required to work? What holidays do you get
and how much sickness is covered?

When contracting, the main negotiation may be around the rate of payment for
services but there are other things to consider too, like obligations and
termination in the event of sickness and for a longer contract, how to take time
off. I want to see these points listed in the schedule of a written contract.

I've worked as a contracting software developer since 1999, almost always
offsite but usually not far from the client so that I can drop in to visit for
meetings from time to time. I've had the occassional contract that has been
truly remote, where I've not ever met the client in person. With the pandemic
many more people are working remotely and it seems employers are more willing to
hire remotely. Some of these contracts will not be worth taking on because the
conditions or the rate is not good. I've been offered a couple of contracts
where the client intends to hire a contractor as an employee in all but name and
in each case they've not been good offers.

For a long time a rule of thumb I've heard of and used myself is, take an annual
salary in thousands and use that as hourly rate for contracting, so
``$100,000/yr`` converts to ``$100/hr``. Is this conversion is fair? I've seen
it fit well in England, France and New Zealand but does it fit with the United
States market?

From the other side, I found an interesting analysis of the `true cost`_ of
engaging consultants versus hiring employees::

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

I got offered a contract out of New York at ``$80/hr``, a city where senior
permanent dev staff with more than a decade's worth of experience get about
``$150,000/yr`` as base salary [#]_. The negotiations fell through but led me to
review rates, especially comparing permanent with contract.

When a client wants to add a contractor to a team of salaried software
developers, how to convert pay rates and conditions from permanent to
contractor? From company revenue, what might the maximum possible shareholder
salary be?

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
      15,000 - missed 10% hack time (paid)
      15,000 - missed 10% retirement (401K)
       5,000 - missed health insurance
    --------
      50,000 - missed benefits

So there you have it, the permanent employee gets effectively ``$200,000`` in
salary and benefits whereas the contractor might get ``$75,000``.

.. _true cost: https://www.toptal.com/freelance/don-t-be-fooled-the-real-cost-of-employees-and-consultants

.. _city rates: https://www.indeed.com/career/software-engineer/salaries/New-York--NY
.. _state rates: https://www.indeed.com/career/software-engineer/salaries/NY

.. [#] Average base salary for ``10+`` years experience:

           - ``$155,440`` New York `city rates`_.
           - ``$133,585`` NY `state rates`_.
