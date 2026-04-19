# Problem: E_Nearly_Shortest_Repeating_Substring.pas

```pascal
program E_Nearly_Shortest_Repeating_Substring;
{$H+}{$MODESWITCH RESULT+}
const
    maxn = 200 * 1000;
var
    ntc, tci: int16;
    n, d: int32;
    e: int16;
    s: string;
    divs: array [1 .. maxn] of array of int32;
    t: array [1 .. maxn] of int16;

function differences(l, r: int32): int32;
var
    i, j: int32;
begin
    result := 0;
    i := 1;
    j := l;
    while (result <= 1) and (i <= n) do begin
        if s[i] <> s[j] then inc(result);
        inc(i);
        inc(j);
        if j > r then j := l;
    end;
end;

begin
    for n := 1 to maxn do begin
        setlength(divs[n], 1);
        t[n] := 1;
        divs[n][0] := 1;
    end;

    for d := 2 to maxn do begin
        n := d;
        while n <= maxn do begin
            if length(divs[n]) = t[n] then setlength(divs[n], 2*t[n]);
            divs[n][t[n]] := d;
            inc(t[n]);
            inc(n, d);
        end;
    end;

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        readln(s);

        e := 0;
        d := divs[n][e];
        while (d < n) and (differences(1, d) > 1) and (differences(n-d+1, n) > 1) do begin
            inc(e);
            d := divs[n][e];
        end;

        writeln(d);

    end;
end.

```
