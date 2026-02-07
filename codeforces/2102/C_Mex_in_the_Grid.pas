program C_Mex_in_the_Grid;
const
    nn = 500;
var
    ntc, tci: int8;
    n, i, j, k, h, x, s, s2: int32;
    a: array [1 .. nn, 1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);
        h := (n+1) div 2;

        x := 0;
        for s := 1 to n do begin
            s2 := s div 2;
            if odd(s) then begin

                i := h - s2;
                j := h - s2;

                for k := 1 to s-1 do begin
                    inc(j);
                    a[i, j] := x;
                    inc(x);
                end;

                i := h - s2;
                j := h - s2;

                for k := 1 to s-1 do begin
                    inc(i);
                    a[i, j] := x;
                    inc(x);
                end;

                i := h - s2;
                j := h - s2;
                a[i, j] := x;
                inc(x);

            end else begin

                i := h - s2;
                j := h + s2;

                for k := 1 to s-1 do begin
                    inc(i);
                    a[i, j] := x;
                    inc(x);
                end;

                i := h + s2;
                j := h - s2;

                for k := 1 to s do begin
                    inc(j);
                    a[i, j] := x;
                    inc(x);
                end;

            end;
        end;

        for i := 1 to n do begin
            for j := 1 to n-1 do write(a[i, j], ' ');
            writeln(a[i, n]);
        end;

    end;
end.
