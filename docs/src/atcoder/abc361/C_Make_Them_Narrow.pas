program C_Make_Them_Narrow;
{$mode objfpc}{$H+}{$J-}
uses
    math;
const
    nn = 200 * 1000;
var
    n, k, i, ans: int32;
    a, merge: array [1 .. nn] of int32;

procedure msort(lend, rend: int32);
var
    i, l, r, m: int32;
begin
    if rend - lend > 1 then begin

        m := (lend + rend) div 2;
        msort(lend, m);
        msort(m, rend);

        l := lend;
        r := m;
        for i := lend to rend - 1 do
            if (r = rend) or (l < m) and (a[l] <= a[r]) then begin
                merge[i] := a[l];
                inc(l);
            end else begin
                merge[i] := a[r];
                inc(r);
            end;

        for i := lend to rend - 1 do a[i] := merge[i];

    end;
end;

begin
    readln(n, k);

    for i := 1 to n do read(a[i]); readln; msort(1, n+1);

    ans := high(int32);
    for i := 0 to k do ans := min(ans, a[n-(k-i)] - a[1+i]);
    writeln(ans);
end.
