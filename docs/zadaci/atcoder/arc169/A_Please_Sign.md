# Задатак: A_Please_Sign.pas

```pascal
program A_Please_Sign;
const
    maxn = 250 * 1000;
var
    n, i, p: int32;
    a, d: array [1 .. maxn] of int32;
    s: array [1 .. maxn] of int64;

begin
    readln(n);

    for i := 1 to n do begin
        read(a[i]);
        s[i] := 0;
    end;
    readln;

    d[1] := 1;
    s[1] := a[1];
    for i := 2 to n do begin
        read(p);
        d[i] := d[p] + 1;
        inc(s[d[i]], a[i]);
    end;
    readln;

	i := n;
	while (i > 1) and (s[i] = 0) do dec(i);

    if s[i] > 0 then
        writeln('+')
    else if s[i] < 0 then
        writeln('-')
    else
        writeln(0);
end.

```
