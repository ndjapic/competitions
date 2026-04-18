# Задатак: D_Binary_Cut.pas

```pascal
program D_Binary_Cut;
{$H+}
uses
    math;
var
    ntc, tci: int16;
    n, i, c01, c10: int16;
    s: string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(s);
        n := length(s);

        c01 := 0;
        c10 := 0;
        for i := 2 to n do
            if s[i-1] < s[i] then
                inc(c01)
            else if s[i-1] > s[i] then
                inc(c10);

        c01 := max(1, c01);
        writeln(c01 + c10);

    end;
end.

```
