# Problem: B_Outstanding_Impressionist.pas

```pascal
program B_Outstanding_Impressionist;
{$mode delphi}
uses
    math;
const
    nn = 200 * 1000 + 1;
var
    ntc, tci: int16;
    n, i, x: int32;
    l, r: array [1 .. nn] of int32;
    c, prev: array [0 .. 2*nn] of int32;
    s: string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        setlength(s, n);

        for x := 0 to 2*n do c[x] := 0;

        for i := 1 to n do begin
            readln(l[i], r[i]);
            if l[i] = r[i] then inc(c[l[i]]);
        end;

        for x := 0 to 2*n do
            if c[x] = 0 then
                prev[x] := x
            else
                prev[x] := prev[x-1];

        for i := 1 to n do begin
            if l[i] < r[i] then begin
                if l[i] <= prev[r[i]] then
                    s[i] := '1'
                else
                    s[i] := '0';
            end else if c[l[i]] = 1 then
                s[i] := '1'
            else
                s[i] := '0';
        end;

        writeln(s);

    end;
end.

```
