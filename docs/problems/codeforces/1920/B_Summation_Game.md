# Problem: B_Summation_Game.pas

```pascal
program B_Summation_Game;
uses
    math;
const
    maxn = 200 * 1000;
var
    ntc, tci: int16;
    n, k, x, y, i, ans: int32;
    a, merge: array [0 .. maxn] of int32;

procedure msort(l, r: int32);
var
    m, i, j, k: int32;
begin
    if r-l > 1 then begin

        m := (l+r) div 2;
        msort(l, m);
        msort(m, r);

        j := l;
        k := m;
        for i := l to r-1 do
            if (k = r) or (j < m) and (a[j] <= a[k]) then begin
                merge[i] := a[j];
                inc(j);
            end else begin
                merge[i] := a[k];
                inc(k);
            end;

        for i := l to r-1 do a[i] := merge[i];

    end;
end;

begin
    a[0] := 0;
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, k, x);

        for i := 1 to n do read(a[i]);
        readln;

        msort(1, n+1);
        for i := 1 to n do inc(a[i], a[i-1]);

        ans := low(int32);
        while k >= 0 do begin
            y := min(n-k, x);
            ans := max(ans, a[n-k-y] - a[n-k] + a[n-k-y]);
            dec(k);
        end;

        writeln(ans);

    end;
end.

```
