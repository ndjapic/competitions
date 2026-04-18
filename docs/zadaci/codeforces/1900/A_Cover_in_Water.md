# Задатак: A_Cover_in_Water.pas

```pascal
program A_Cover_in_Water;
uses
    math;
const
    maxn = 100;
var
    ntc, tci: int8;
    n, i, blocked, maxempty, countempty: int8;
    s: array [1 .. maxn] of char;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        countempty := 0;
        maxempty := 0;
        blocked := 0;

        readln(n);
        for i := 1 to n do begin
            read(s[i]);
            if s[i] = '#' then
                blocked := i
            else begin
                inc(countempty);
                maxempty := max(maxempty, i - blocked);
            end;
        end;
        readln;

        if maxempty > 2 then
            writeln(2)
        else
            writeln(countempty);

    end;
end.

```
