# Problem: B_Battle_Cows.pas

```pascal
program B_Battle_Cows;
{$MODESWITCH RESULT+}
uses
    math;
const
    maxn = 100 * 1000;
var
    ntc, tci: int16;
    n, k, i, j, ans: int32;
    a: array [1 .. maxn] of int32;

function wins(j: int32): int32;
var
    swp, w, i: int32;
begin
    swp := a[j];
    a[j] := a[k];
    a[k] := swp;

    w := 1;
    result := 0;
    for i := 2 to j-1 do
        if a[i] > a[w] then w := i;

    if a[w] <= a[j] then begin
        if a[w] < a[j] then inc(result);
        i := j+1;
        while (i <= n) and (a[i] < a[j]) do begin
            inc(result);
            inc(i);
        end;
    end;

    swp := a[j];
    a[j] := a[k];
    a[k] := swp;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k);

        for i := 1 to n do read(a[i]); readln;

        ans := wins(1);
        if k > 1 then begin
            j := 1;
            for i := 2 to k-1 do
                if a[j] < a[i] then j := i;
            ans := max(ans, wins(j));
            ans := max(ans, wins(k));
        end;

        writeln(ans);

    end;
end.

```
