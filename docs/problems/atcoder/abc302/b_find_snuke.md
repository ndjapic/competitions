# Problem: b_find_snuke.pas

```pascal
program B_Find_snuke;
var
    h, w, i, j, i1, j1, d, e: int8;
    found: boolean;
    s: array [1 .. 100, 1 .. 100] of char;
    di, dj: array [1 .. 8] of int8;
    snuke: array [0 .. 4] of char;

begin
    snuke[0] := 's';
    snuke[1] := 'n';
    snuke[2] := 'u';
    snuke[3] := 'k';
    snuke[4] := 'e';

    di[1] := 0; dj[1] := 1;
    di[2] := -1; dj[2] := 1;
    di[3] := -1; dj[3] := 0;
    di[4] := -1; dj[4] := -1;
    di[5] := 0; dj[5] := -1;
    di[6] := 1; dj[6] := -1;
    di[7] := 1; dj[7] := 0;
    di[8] := 1; dj[8] := 1;

    readln(h, w);
    for i := 1 to h do begin
        for j := 1 to w do read(s[i, j]);
        readln;
    end;

    found := false;
    for i1 := 1 to h do
        for j1 := 1 to w do
            for d := 1 to 8 do
                if not found then begin

                    e := 0;
                    i := i1;
                    j := j1;

                    while (e < 5) and (0 < i) and (i <= h) and (0 < j) and (j <= w) and (s[i, j] = snuke[e]) do begin
                        inc(i, di[d]);
                        inc(j, dj[d]);
                        inc(e);
                    end;

                    found := i = 5;
                    if found then
                        for e := 0 to 4 do
                            writeln(i1+di[d]*e, ' ', j1+dj[d]*e);
                end;
end.


```
