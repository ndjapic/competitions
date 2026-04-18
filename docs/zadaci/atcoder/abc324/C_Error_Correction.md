# Задатак: C_Error_Correction.pas

```pascal
program C_Error_Correction;
const
    maxn = 500 * 1000;
var
    n, nt, ns, i, j, k: int32;
    t, s: array [1 .. maxn] of char;
    a: array [1 .. maxn] of int32;

begin
    read(n);
    read(t[1]);

    nt := 0;
    repeat
        inc(nt);
        read(t[nt]);
    until eoln;
    readln;

    k := 0;
    for i := 1 to n do begin

        ns := 0;
        repeat
            inc(ns);
            read(s[ns]);
        until eoln;
        readln;

        if ns - nt = 1 then begin

            j := 1;
            while (j <= nt) and (t[j] = s[j]) do inc(j);
            while (j <= nt) and (t[j] = s[j+1]) do inc(j);

            if j > nt then begin
                inc(k);
                a[k] := i;
            end;

        end else if nt - ns = 1 then begin

            j := 1;
            while (j <= ns) and (s[j] = t[j]) do inc(j);
            while (j <= ns) and (s[j] = t[j+1]) do inc(j);

            if j > ns then begin
                inc(k);
                a[k] := i;
            end;

        end else if nt = ns then begin

            j := 1;
            while (j <= nt) and (t[j] = s[j]) do inc(j);
            inc(j);
            while (j <= nt) and (t[j] = s[j]) do inc(j);

            if j > nt then begin
                inc(k);
                a[k] := i;
            end;

        end;

    end;

    writeln(k);
    for i := 1 to k-1 do write(a[i], ' ');
    if k > 0 then writeln(a[k]);
end.

```
