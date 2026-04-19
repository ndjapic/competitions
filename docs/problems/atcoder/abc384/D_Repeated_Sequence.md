# Problem: D_Repeated_Sequence.pas

```pascal
program D_Repeated_Sequence;
const
    nn = 200 * 1000;
var
    n, i, l, r: int32;
    s: int64;
    a: array [0 .. nn] of int64;

function f(l, r: int32): int64;
begin
    f := a[r] - a[l-1];
end;

function g(l, r: int32): int64;
begin
    g := a[n] - (a[r] - a[l-1]);
end;

begin
    readln(n, s);

    a[0] := 0;
    for i := 1 to n do begin
        read(a[i]);
        inc(a[i], a[i-1]);
    end;
    readln;
    s := (s-1) mod a[n] + 1;

    l := 1;
    r := 0;
    while (r <= n) and (f(l, r) <> s) do begin
        while f(l, r) > s do inc(l);
        if f(l, r) < s then inc(r);
    end;

    if f(l, r) <> s then begin
        l := 1;
        r := 0;
        while (r <= n) and (g(l, r) <> s) do begin
            while g(l, r) < s do inc(l);
            if g(l, r) > s then inc(r);
        end;
    end;

    if (f(l, r) = s) or (g(l, r) = s) then
        writeln('Yes')
    else
        writeln('No');
end.

```
