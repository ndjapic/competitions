# Задатак: C_Mad_MAD_Sum.pas

```pascal
program C_Mad_MAD_Sum;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, i: int32;
    ans: int64;
    a, c: array [0 .. nn] of int32;
    d: array [0 .. nn] of array of int32;

procedure mad();
var
    x, i: int32;
begin
    for x := 0 to n do begin
        setlength(d[x], 1);
        c[x] := 0;
    end;

    for i := 1 to n do begin
        x := a[i];
        a[i] := 0;
        inc(ans, x);
        if length(d[x]) <= c[x] then setlength(d[x], 2*c[x]);
        d[x][c[x]] := i;
        inc(c[x]);
    end;

    for x := n downto 0 do
        if c[x] > 1 then begin
            i := d[x][1];
            while (i <= n) and (a[i] < x) do begin
                a[i] := x;
                inc(i);
            end;
        end;
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        for i := 1 to n do read(a[i]); readln;

        ans := 0;
        mad();
        mad();
        mad();

        for i := 1 to n do inc(ans, int64(n+1-i) * a[i]);
        writeln(ans);

    end;
end.

```
