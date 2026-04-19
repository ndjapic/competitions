# Problem: B_Bessie_and_MEX.pas

```pascal
program B_Bessie_and_MEX;
const
    maxn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, mex: int32;
    a, p: array [1 .. maxn] of int32;
    seen: array [0 .. maxn] of boolean;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for mex := 0 to n do seen[mex] := false;

        mex := 0;
        for i := 1 to n do begin
            read(a[i]);
            if a[i] > 0 then
                p[i] := mex
            else if a[i] < 0 then
                p[i] := mex - a[i];
            seen[p[i]] := true;
            while seen[mex] do inc(mex);
        end;
        readln;

        for i := 1 to n-1 do write(p[i], ' ');
        writeln(p[n]);

    end;
end.

```
