# Задатак: E_Merge_Not_Sort.pas

```pascal
program E_Merge_Not_Sort;
uses
    math;
const
    maxn = 1000;
    max2n = 2000;
var
    n, i, t, j, s, x, y: int16;
    c: array [1 .. max2n] of int16;
    a, b: array [1 .. maxn] of int16;
    l, r, d: array [1 .. max2n] of int16;
    dp: array [0 .. max2n] of array [0 .. max2n] of boolean;

begin
    readln(n);

    for i := 1 to 2*n do read(c[i]);
    readln;

    t := 0;
    l[1] := 1;
    for i := 1 to 2*n do
        if (i = 2*n) or (c[i+1] > c[l[t+1]]) then begin

            inc(t);
            r[t] := i;
            d[t] := r[t] - l[t] + 1;
            if i < 2*n then l[t+1] := i+1;

        end;

    for s := 0 to 2*n do dp[0][s] := s = 0;

    for j := 1 to t do
        for s := 0 to 2*n do
            dp[j][s] := dp[j-1][s] or (s >= d[j]) and dp[j-1][s-d[j]];

    {for j := 1 to t do begin
        for s := 0 to 2*n do dp[j][s] := dp[j-1][s];
        for s := d[j] to 2*n do
            if dp[j-1][s-d[j]] then
                dp[j][s] := true;
    end;}

    if not dp[t][n] then
        writeln(-1)
    else begin

        x := n;
        y := n;
        s := n;
        for j := t downto 1 do
            if not dp[j-1][s] then begin
                {write(' j=', j);}
                for i := r[j] downto l[j] do begin
                    a[x] := c[i];
                    {write(' x=', x);}
                    dec(x);
                end;
                dec(s, d[j]);
            end else
                for i := r[j] downto l[j] do begin
                    b[y] := c[i];
                    {write(' y=', y);}
                    dec(y);
                end;
        writeln;

        for i := 1 to n-1 do write(a[i], ' '); writeln(a[n]);
        for i := 1 to n-1 do write(b[i], ' '); writeln(b[n]);

    end;
end.

```
