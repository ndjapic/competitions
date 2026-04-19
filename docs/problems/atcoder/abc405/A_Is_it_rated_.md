# Problem: A_Is_it_rated_.pas

```pascal
program A_Is_it_rated_;
uses
    math;
const
    nn = 200 * 1000;
var
    r, x: int32;

begin
    readln(r, x);

    if (x = 1) and (1600 <= r) and (r <= 2999) then
        writeln('Yes')
    else if (x = 2) and (1200 <= r) and (r <= 2399) then
        writeln('Yes')
    else
        writeln('No');
end.

```
