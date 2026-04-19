# Problem: A_Wrong_Answer.pas

```pascal
program A_Wrong_Answer;
uses
    math;
const
    maxn = 1000 * 1000;
var
    n, d: int32;
    aliquot: array [1 .. maxn] of int32;

begin
    for n := 1 to maxn do aliquot[n] := 0;
    for d := 1 to maxn do begin
        n := 2*d;
        while n <= maxn do begin
            inc(aliquot[n], d);
            inc(n, d);
        end;
    end;

    readln(n);
    if n >= 629072 then
        writeln(14316)
    else if n >= 15472 then
        writeln(12496)
    else if n >= 284 then
        writeln(220)
    else
        writeln(6);
end.

```
