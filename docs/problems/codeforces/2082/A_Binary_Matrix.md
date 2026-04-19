# Problem: A_Binary_Matrix.pas

```pascal
program A_Binary_Matrix;
{$MODE DELPHI}
uses
    math;
const
    nn = 100;
var
    ntc, tci: int16;
    n, m, i, j, r, c: int8;
    row, col: array [1 .. nn] of int8;
    s: string;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n, m);
        r := 0;
        c := 0;

        for j := 1 to m do col[j] := 0;

        for i := 1 to n do begin
            row[i] := 0;
            readln(s);
            for j := 1 to m do
                if s[j] = '1' then begin
                    row[i] := 1 - row[i];
                    col[j] := 1 - col[j];
                end;
            inc(r, row[i]);
        end;

        for j := 1 to m do inc(c, col[j]);

        writeln(max(r, c));

    end;

end.

```
