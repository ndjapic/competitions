# Problem: B_Piano_2.pas

```pascal
program B_Piano_2;
uses
    math;
const
    aa = 200;
var
    n, m, i, j: int8;
    x, l, r: int16;
    found: boolean;
    s: array [1 .. aa] of char;

begin
    readln(n, m);

    for x := 1 to aa do s[x] := '.';

    for i := 1 to n do begin
        read(x);
        s[x] := 'a';
    end;
    readln;

    for j := 1 to m do begin
        read(x);
        s[x] := 'b';
    end;
    readln;

    l := 1;
    found := false;
    while not found and (l <= aa) do begin

        while (l <= aa) and (s[l] <> 'a') do inc(l);
        r := l+1;
        while (r <= aa) and (s[r] <> 'a') do inc(r);

        if r <= aa then begin
            x := l+1;
            while (x < r) and (s[x] <> 'b') do inc(x);
            found := x = r;
        end;
        l := r+1;

    end;

    if found then
        writeln('Yes')
    else
        writeln('No')
end.

```
