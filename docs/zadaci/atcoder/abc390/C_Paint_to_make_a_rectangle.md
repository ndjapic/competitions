# Задатак: C_Paint_to_make_a_rectangle.pas

```pascal
program C_Paint_to_make_a_rectangle;
{$mode delphi}
uses
    math;
const
    nn = 1000;
var
    h, w, a, b, c, d, i, j: int16;
    found: boolean;
    s: array [1 .. nn] of string;
    l, r: array [1 .. nn] of int16;

begin
    readln(h, w);
    for i := 1 to h do readln(s[i]);

    c := w;
    d := 1;
    for i := 1 to h do begin
        l[i] := 1;
        r[i] := w;
        while (l[i] <= r[i]) and (s[i][l[i]] <> '#') do inc(l[i]);
        while (l[i] <= r[i]) and (s[i][r[i]] <> '#') do dec(r[i]);

        if l[i] <= r[i] then begin
            c := min(c, l[i]);
            d := max(d, r[i]);
        end;
    end;

    a := 1;
    b := h;
    while (a <= b) and (l[a] > r[a]) do inc(a);
    while (a <= b) and (l[b] > r[b]) do dec(b);

    found := false;
    for i := a to b do
        for j := c to d do
            if not found then found := s[i][j] = '.';

    if found then
        writeln('No')
    else
        writeln('Yes');
end.

```
