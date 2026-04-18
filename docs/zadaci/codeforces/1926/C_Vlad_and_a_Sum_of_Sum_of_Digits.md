# Задатак: C_Vlad_and_a_Sum_of_Sum_of_Digits.pas

```pascal
program C_Vlad_and_a_Sum_of_Sum_of_Digits;
const
    maxn = 200 * 1000;
var
    ntc, tci, n: int32;
    d: array [0 .. maxn] of int32;
    s: array [0 .. maxn] of int32;

begin
    d[0] := 0;
    s[0] := 0;
    for n := 1 to maxn do begin
        d[n] := d[n div 10] + n mod 10;
        s[n] := s[n-1] + d[n];
    end;

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        writeln(s[n]);

    end;
end.

```
