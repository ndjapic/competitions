# Problem: B_Serval_and_Final_MEX.pas

```pascal
program B_Serval_and_Final_MEX;
uses
    math;
const
    nn = 5000;
var
    ntc, tci: int32;
    n, i, k: int32;
    a, b, l, r: array [1 .. nn] of int32;

procedure add_op(x, y: int32);
var
    m, i: int32;
begin
    inc(k);
    l[k] := x;
    r[k] := y;

    m := 0;
    for i := 1 to n do
        if (i < x) or (y < i) then begin
            inc(m);
            b[m] := a[i];
        end else if i = x then begin
            inc(m);
            b[m] := 1;
        end;

    n := m;
    for i := 1 to n do a[i] := b[i];
end;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do read(a[i]); readln;

        k := 0;

        if a[1] = 0 then add_op(1, 2);
        if a[n] = 0 then add_op(n-1, n);

        i := 2;
        while (i < n) and (a[i] > 0) do inc(i);

        if i < n then add_op(2, n);
        if n > 1 then add_op(1, n);

        writeln(k);
        for i := 1 to k do writeln(l[i], ' ', r[i]);

    end;
end.

```
