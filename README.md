<H2>Interval arithmetic for Ada</H2>
<p>The library provides interval computations. Both intervals of integer and floating-point bounds are supported</p>

<P>Interval computations provide an alternative to traditional numeric 
computations. The fundamental difference is that interval computations trade 
result precision for accuracy, while the traditional floating-point computations 
do accuracy for precision. That is - the result of a traditional floating-point operation has 
the precision of arguments. The rounding errors are accumulated as accuracy loss. 
Differently to this the 
result of an interval operation suffers no accuracy loss, in the sense that the mathematical&nbsp; 
outcome of the operation is contained by the result interval. Rounding errors in 
interval operations are accumulated as the 
interval width, i.e. as precision loss.<P>This fundamental property of interval 
operations 
yields the <i>extension principle</i> by which numeric operations are extended 
to the interval ones.</P>
<P>For example consider the operation +. <i>I</i><sub>1</sub>=[<i>a</i>,&nbsp;<i>b</i>],
<i>I</i><sub>2</sub>=[<i>c</i>,&nbsp;<i>d</i>]. + is monotonically ascending, thus Inf is <i>a</i>+<i>c</i> and Sup 
is <i>b</i>+<i>d</i> and so [<i>a</i>,&nbsp;<i>b</i>]+[<i>c</i>,&nbsp;<i>d</i>]=[<i>a</i>+<i>c</i>,&nbsp;<i>b</i>+<i>d</i>].<P>
Any combination of interval operations maintain the property, but changing order 
of operations or splitting operations into smaller ones may change the precision 
of the result (the result interval width). E.g. <i>x</i><sup>2</sup> is not 
necessarily equal to <i>x</i>*<i>x</i>, still both contain the exact outcome.</P>

Home page: https://www.dmitry-kazakov.de/ada/intervals.htm

Changes log: https://www.dmitry-kazakov.de/ada/intervals.htm#changes_log
