# Problem: D_Switch_Seats.pas

```pascal
program D_Switch_Seats;
const
    nn = 400 * 1000;
var
    ntc, tci, n, i, x, y, ans: int32;
    a, c: array [1 .. nn] of int32;
    ind: array [1 .. nn] of array [0 .. 1] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for x := 1 to n do c[x] := 0;

        for i := 1 to 2*n do begin
            read(x);
            a[i] := x;
            ind[x][c[x]] := i;
            inc(c[x]);
        end;
        readln;

        ans := 0;
        for i := 1 to 2*n-1 do begin
            x := a[i];
            y := a[i+1];
            if (x <> y) and
                (ind[x][0] = i) and
                (ind[y][0] = i+1) and
                (ind[y][1] > i+2) and
                (abs(ind[x][1] - ind[y][1]) = 1) then inc(ans);
        end;

        writeln(ans);

    end;
end.

```
