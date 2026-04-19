# Problem: A_Full_House_2.pas

```pascal
program A_Full_House_2;
uses
    math;
const
    nn = 4;
var
    i, mn, mx, cn, cx: int8;
    a: array [1 .. nn] of int8;

procedure swp(i, j: int8);
begin
end;

begin
    mn := 13;
    mx := 1;

    for i := 1 to nn do begin
        read(a[i]);
        mn := min(mn, a[i]);
        mx := max(mx, a[i]);
    end;
    readln;

    cn := 0;
    cx := 0;
    for i := 1 to nn do begin
        if a[i] = mn then inc(cn);
        if a[i] = mx then inc(cx);
    end;

    if (mn < mx) and (cn + cx = nn) then
        writeln('Yes')
    else
        writeln('No');
end.

```
