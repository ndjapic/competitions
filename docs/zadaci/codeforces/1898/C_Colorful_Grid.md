# Задатак: C_Colorful_Grid.pas

```pascal
program C_Colorful_Grid;
const
    maxn = 16;
var
    ntc, tci: int8;
    n, m, i, j: int8;
    k: int32;
    h, v: array [1 .. maxn, 1 .. maxn] of char;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m, k);
        dec(k, n+m-2);

        if (k < 0) or odd(k) then
            writeln('NO')
        else begin
            writeln('YES');

            for i := 1 to n do
                for j := 1 to m do
                    if odd(i+j) then begin
                        h[i, j] := 'B';
                        v[i, j] := 'B'
                    end else begin
                        h[i, j] := 'R';
                        v[i, j] := 'R';
                    end;

            v[1, 2] := 'R';
            h[2, 2] := 'B';

            for i := 1 to n do begin
                for j := 1 to m-2 do write(h[i, j], ' ');
                writeln(h[i, m-1]);
            end;

            for i := 1 to n-1 do begin
                for j := 1 to m-1 do write(v[i, j], ' ');
                writeln(v[i, m]);
            end;

        end;

    end;
end.

```
