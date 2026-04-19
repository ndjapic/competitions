# Problem: A_Twice_Subsequence.pas

```pascal
program A_Twice_Subsequence;
{$MODE DELPHI}
const
    nn = 200 * 1000;
var
    n, m, i, j: int32;
    a, b, ind1, ind2: array [1 .. nn] of int32;

begin
    readln(n, m);

    for i := 1 to n do read(a[i]); readln;
    for j := 1 to m do read(b[j]); readln;

    j := 1;
    for i := 1 to n do
        if (j <= m) and (a[i] = b[j]) then begin
            ind1[j] := i;
            inc(j);
        end;

    if j <= m then
        writeln('No')
    else begin

        j := m;
        for i := n downto 1 do
            if (j > 0) and (a[i] = b[j]) then begin
                ind2[j] := i;
                dec(j);
            end;

        if j > 0 then
            writeln('No')
        else begin

            j := 1;
            while (j <= m) and (ind1[j] = ind2[j]) do inc(j);

            if j <= m then
                writeln('Yes')
            else
                writeln('No');

        end;

    end;
end.

```
