{$IOCHECKS OFF}
program equal_strings;
uses
    math;
var
    ntc: int8;
    n, x, i, j, na, h, u, v, min_cost, max_cost, noa: int16;
    ans: int32;
    s, t: array [1 .. 2000] of char;
    a: array [1 .. 2000] of int16;
    r2c, c2r: array [1 .. 1000] of int16;
    c: array [1 .. 1000, 1 .. 1000] of int16;
    parent: array [1 .. 1001] of int16;

begin
    readln(ntc);
    repeat

        readln(n, x);

        for i := 1 to n do read(s[i]); readln;
        for i := 1 to n do read(t[i]); readln;

        na := 0;
        for i := 1 to n do
            if s[i] <> t[i] then begin
                inc(na);
                a[na] := i;
            end;

        if odd(na) then
            ans := -1
        else if na = 0 then
            ans := 0
        else begin

            h := na div 2;

            for u := 1 to h do
                for v := 1 to h do
                    c[u, v] := min(x, abs(a[2*v] - a[2*u-1]));

            for u := 1 to h do begin
                min_cost := high(int16);
                for v := 1 to h do min_cost := min(min_cost, c[u, v]);
                for v := 1 to h do dec(c[u, v], min_cost);
            end;

            for v := 1 to h do begin
                min_cost := high(int16);
                for u := 1 to h do min_cost := min(min_cost, c[u, v]);
                for u := 1 to h do dec(c[u, v], min_cost);
            end;

            {for u := 1 to h do begin
                for v := 1 to h do write(' ', c[u, v]);
                writeln;
                flush(output);
            end;}

            repeat {writeln('Entering repeat 1'); flush(output);}

                for u := 1 to h do begin
                    r2c[u] := 0;
                    c2r[u] := 0;
                end;

                noa := 0;

                for u := 1 to h do if (noa < h) and (r2c[u] = 0) then begin

                    parent[h+1] := 0;
                    for v := 1 to h do
                        if (c2r[v] = 0) and (c[u, v] = 0) then begin
                            parent[v] := parent[h+1];
                            parent[h+1] := v;
                        end;

                    v := parent[h+1];
                    if (v > 0) and (parent[v] = 0) then begin
                        c2r[v] := u;
                        inc(noa);
                        {writeln(noa, ' ', u, '->', v); flush(output);}
                    end;

                end;

                for v := 1 to h do if (noa < h) and (c2r[v] = 0) then begin

                    parent[h+1] := 0;
                    for u := 1 to h do
                        if (r2c[u] = 0) and (c[u, v] = 0) then begin
                            parent[u] := parent[h+1];
                            parent[h+1] := u;
                        end;

                    u := parent[h+1];
                    if (u > 0) and (parent[u] = 0) then begin
                        r2c[u] := v;
                        inc(noa);
                        {writeln(noa, ' ', u, '->', v); flush(output);}
                    end;

                end;

                min_cost := high(int16);
                max_cost := 0;
                for u := 1 to h do
                    if r2c[u] = 0 then
                        for v := 1 to h do
                            if c2r[v] = 0 then begin
                                min_cost := min(min_cost, c[u, v]);
                                max_cost := max(max_cost, c[u, v]);
                            end;

                if max_cost = 0 then begin

                    for v := 1 to h do begin
                        u := c2r[v];
                        if (u > 0) and (r2c[u] = 0) then r2c[u] := v;
                    end;

                    for u := 1 to h do begin
                        v := r2c[u];
                        if (v > 0) and (c2r[v] = 0) then c2r[v] := u;
                    end;

                    u := 1;
                    v := 1;
                    while (u <= h) and (v <= h) do
                        if r2c[u] > 0 then
                            inc(u)
                        else if c2r[v] > 0 then
                            inc(v)
                        else begin
                            r2c[u] := v;
                            c2r[v] := u;
                            inc(noa);
                            inc(u);
                            inc(v);
                        end;

                end else if min_cost < high(int16) then
                    for u := 1 to h do
                        for v := 1 to h do
                            if (r2c[u] = 0) and (c2r[v] = 0) then
                                dec(c[u, v], min_cost)
                            else if (r2c[u] > 0) and (c2r[v] > 0) then
                                inc(c[u, v], min_cost);

            until noa >= h;

            for v := 1 to h do
                if c2r[v] > 0 then
                    r2c[c2r[v]] := v;

            ans := 0;
            for u := 1 to h do begin
                v := r2c[u];
                c[u, v] := min(x, abs(a[2*v] - a[2*u-1]));
                inc(ans, c[u, v]);
            end;

        end;

        writeln(ans);

        dec(ntc);
    until ntc = 0;
end.
