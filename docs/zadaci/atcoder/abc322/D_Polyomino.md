# Задатак: D_Polyomino.pas

```pascal
program D_Polyomino;
uses
    math;
const
    maxn = 4;
var
    i, j, k, j4, k4: int32;
    p: array [1 .. maxn, 1 .. maxn, 1 .. maxn] of char;
    found: boolean;
    lj, rj, lk, rk, dj, dk, rot: array [1 .. maxn] of int8;

begin

    for i := 1 to 3 do begin
        lj[i] := 4;
        rj[i] := 1;
        lk[i] := 4;
        rk[i] := 1;

        for j := 1 to 4 do begin
            for k := 1 to 4 do begin

                read(p[i, j, k]);
                if p[i, j, k] = '#' then begin
                    lj[i] := min(lj[i], j);
                    rj[i] := max(rj[i], j);
                    lk[i] := min(lk[i], k);
                    rk[i] := max(rk[i], k);
                end;

            end;
            readln;
        end;
    end;

    found := false;

    dj[1] := 1 - lj[1];
    while not found and (dj[1] <= 4 - rj[i]) do begin

        dk[1] := 1 - lk[1];
        while not found and (dk[1] <= 4 - rk[i]) do begin

            dj[2] := 1 - lj[2];
            while not found and (dj[2] <= 4 - rj[i]) do begin

                dk[2] := 1 - lk[2];
                while not found and (dk[2] <= 4 - rk[i]) do begin

                    dj[3] := 1 - lj[3];
                    while not found and (dj[3] <= 4 - rj[i]) do begin

                        dk[3] := 1 - lk[3];
                        while not found and (dk[3] <= 4 - rk[i]) do begin

                            rot[1] := 0;
                            rot[2] := 0;
                            while not found and (rot[2] < 4) do begin

                                rot[3] := 0;
                                while not found and (rot[3] < 4) do begin

                                    found := true;
                                    for j := 1 to 4 do
                                        for k := 1 to 4 do
                                            p[4, j, k] := '.';

                                    i := 1;
                                    while found and (i <= 3) do begin

                                        j := lj[i];
                                        while found and (j <= rj[i]) do begin

                                            k := lk[i];
                                            while found and (k <= rk[i]) do begin

                                                if p[i, j, k] = '#' then begin

                                                    case rot[i] of

                                                        0: begin
                                                            j4 := j + dj[i];
                                                            k4 := k + dk[i];
                                                        end;

                                                        1: begin
                                                            k4 := 5 - j - dj[i];
                                                            j4 := k + dk[i];
                                                        end;

                                                        2: begin
                                                            j4 := 5 - j - dj[i];
                                                            k4 := 5 - k - dk[i];
                                                        end;

                                                        3: begin
                                                            k4 := j + dj[i];
                                                            j4 := 5 - k - dk[i];
                                                        end;

                                                    end;

                                                    found := p[4, j4, k4] = '.';

                                                end;

                                                inc(k);
                                            end;

                                            inc(j);
                                        end;

                                        inc(i);
                                    end;

                                    inc(rot[3]);
                                end;

                                inc(rot[2]);
                            end;

                            inc(dk[3]);
                        end;

                        inc(dj[3]);
                    end;

                    inc(dk[2]);
                end;

                inc(dj[2]);
            end;

            inc(dk[1]);
        end;

        inc(dj[1]);
    end;

    if found then
        writeln('Yes')
    else
        writeln('No');

end.


```
