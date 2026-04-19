# Problem: B_Fibonacci_Cubes.pas

```pascal
program B_Fibonacci_Cubes;
{$MODE DELPHI}
uses
    math;
var
    ntc, tci, n, m, i, j, w, l, h: int32;
    f: array [1 .. 10] of int32;
    ans: string;

begin
    f[1] := 1;
    f[2] := 2;
    for i := 3 to 10 do f[i] := f[i-1] + f[i-2];

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m);
        setlength(ans, m);

        for j := 1 to m do begin

            readln(w, l, h);

            if n = 1 then
                ans[j] := '1'
            else if f[n] > h then
                ans[j] := '0'
            else if f[n] > min(w, l) then
                ans[j] := '0'
            else if f[n] + f[n-1] <= h then
                ans[j] := '1'
            else if f[n] + f[n-1] <= max(w, l) then
                ans[j] := '1'
            else
                ans[j] := '0';

        end;

        writeln(ans);

    end;
end.

```
