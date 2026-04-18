# Задатак: B_Make_Three_Regions.pas

```pascal
program B_Make_Three_Regions;
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, j, ans: int32;
    i: int8;
    s: array [1 .. 2] of string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        readln(s[1]);
        readln(s[2]);

        ans := 0;
        for i := 1 to 2 do
            for j := 2 to n-1 do
                if (s[i][j] = '.')
                and (s[i][j-1] = '.')
                and (s[i][j+1] = '.')
                and (s[3-i][j] = '.')
                and (s[3-i][j-1] = 'x')
                and (s[3-i][j+1] = 'x')
                then inc(ans);

        writeln(ans);

    end;
end.

```
