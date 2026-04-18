# Задатак: C_Wonderful_City.pas

```pascal
program C_Wonderful_City;
uses
    math;
const
    nn = 1000;
    inf = 1000 * 1000 * 1000 * 1000 * 1000 * 1000;
var
    ntc, tci: int16;
    n, i, j: int16;
    d: int32;
    hor, ver: int64;
    h: array [1 .. nn, 1 .. nn] of int32;
    a, b: array [1 .. nn] of int32;
    must, can, will: array [1 .. nn] of boolean;
    dp0, dp1: array [0 .. nn] of int64;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for i := 1 to n do begin
            for j := 1 to n do read(h[i, j]);
            readln;
        end;

        for i := 1 to n do read(a[i]); readln;
        for j := 1 to n do read(b[j]); readln;


        for i := 1 to n do begin
            can[i] := true;
            will[i] := false;
        end;

        for i := 1 to n-1 do begin
            must[i] := false;
            for j := 1 to n do begin
                d := h[i+1, j] - h[i, j];
                if d = 0 then must[i] := true;
                if d = 1 then can[i] := false;
                if d = -1 then can[i+1] := false;
            end;
        end;

        dp0[1] := 0;
        if can[1] then
            dp1[1] := a[1]
        else
            dp1[1] := 0;

        for i := 2 to n do begin
            if not must[i-1] then begin
                dp0[i] := dp0[i-1];
                dp1[i] := dp0[i-1];
            end else if can[i] then begin
                dp0[i] := dp1[i-1];
                dp1[i] := dp0[i-1] + a[i];
            end else begin
                dp0[i] := dp1[i-1];
                dp1[i] := inf;
            end;
        end;

        hor := max(dp0[n], dp1[n]);


        for i := 1 to n do begin
            can[i] := true;
            will[i] := false;
        end;

        for j := 1 to n-1 do begin
            must[j] := false;
            for i := 1 to n do begin
                d := h[i, j+1] - h[i, j];
                if d = 0 then must[j] := true;
                if d = 1 then can[j] := false;
                if d = -1 then can[j+1] := false;
            end;
        end;

        dp0[1] := 0;
        if can[1] then
            dp1[1] := b[1]
        else
            dp1[1] := 0;

        for j := 2 to n do begin
            if not must[j-1] then begin
                dp0[j] := dp0[j-1];
                dp1[j] := dp0[j-1];
            end else if can[j] then begin
                dp0[j] := dp1[j-1];
                dp1[j] := dp0[j-1] + b[j];
            end else begin
                dp0[j] := dp1[j-1];
                dp1[j] := inf;
            end;
        end;

        ver := max(dp0[n], dp1[n]);


        if hor + ver < inf then
            writeln(hor + ver)
        else
            writeln(-1);

    end;
end.

```
