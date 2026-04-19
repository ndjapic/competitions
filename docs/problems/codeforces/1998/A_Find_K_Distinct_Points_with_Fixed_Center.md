# Problem: A_Find_K_Distinct_Points_with_Fixed_Center.pas

```pascal
program A_Find_K_Distinct_Points_with_Fixed_Center;
var
    ntc, tci: int32;
    k, i, xc, yc: int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(xc, yc, k);
        for i := 1 to k do writeln(xc -k-1 + 2*i, ' ', yc);

    end;
end.

```
