program B_Two_Out_of_Three;
const
    maxn = 100;
var
    ntc, tci: int16;
    n, i, j, x: int8;
    ans: boolean;
    a, b, prev, last: array [1 .. maxn] of int8;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        for x := 1 to maxn do last[x] := 0;

        readln(n);
        for i := 1 to n do begin
            read(a[i]);
            b[i] := 1;
            prev[i] := last[a[i]];
            last[a[i]] := i;
        end;
        readln;

        i := 1;
        while (i <= n) and (prev[i] = 0) do inc(i);
        ans := i <= n;

        if ans then begin

            b[i] := 2;
            j := i;
            while (j <= n) and ((a[j] = a[i]) or (prev[j] = 0)) do inc(j);
            ans := j <= n;
            if ans then b[j] := 3;

        end;

        if ans then begin
            for i := 1 to n-1 do write(b[i], ' '); writeln(b[n]);
        end else
            writeln(-1);

    end;
end.
