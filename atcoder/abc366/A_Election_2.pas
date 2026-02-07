program A_Election_2;
{$mode objfpc}{$H+}{$J-}
uses
    math;
var
    n, t, a: int8;

begin
    readln(n, t, a);

    if max(t, a) > n div 2 then
        writeln('Yes')
    else
        writeln('No');
end.
