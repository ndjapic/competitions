# Problem: C_Subsequence_and_Prefix_Sum.pas

```pascal
program C_Subsequence_and_Prefix_Sum;
{$mode objfpc}{$H+}{$J-}
const
    nn = 100;
    aa = 10;
    prime = 1000 * 1000 * 1000 + 7;
var
    n, i: int8;
    j, c: int16;
    ans: int32;
    a: array [1 .. nn] of int8;
    seen: array [0 .. nn, -aa*nn .. aa*nn] of boolean;

begin
    readln(n);

    seen[0, 0] := true;
    ans := 1;

    for i := 1 to n do begin

        read(a[i]);

        for j := -10*i to 10*i do seen[i, j] := false;
        for j := -10*(i-1) to 10*(i-1) do seen[i, j] := seen[i-1, j];
        for j := -10*(i-1) to 10*(i-1) do seen[i, j+a[i]] := seen[i, j+a[i]] or seen[i-1, j];

        c := 0;
        for j := -10*i to 10*i do
            if seen[i, j] then inc(c);
        ans := ans * c mod prime;

    end;
    readln;

    writeln(ans);
end.

```
