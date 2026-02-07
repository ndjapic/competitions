program D_Doubles;
{$mode objfpc}{$H+}{$J-}
uses
    Generics.Defaults, Generics.Collections;
const
    nn = 100;
    kk = 100 * 1000;
type
    intList = specialize TList<int32>;
var
    n, i, i1, i2, j, j1, j2, x: int32;
    s: int64;
    p, mx: extended;
    a: array of intList;
    k: array of int32;
    nxt: array [0 .. nn] of array [0 .. kk] of int32;

begin
    readln(n);
    setlength(a, n);
    setlength(k, n);

    for i := 0 to n-1 do begin
        a[i] := intList.Create();
        read(k[i]);
        for j := 0 to k[i]-1 do begin
            read(x);
            a[i].Add(x);
        end;
        readln;
        a[i].Sort();

        nxt[i][k[i]-1] := k[i];
        for j := k[i]-2 downto 0 do
            if a[i][j] = a[i][j+1] then
                nxt[i][j] := nxt[i][j+1]
            else
                nxt[i][j] := j+1;
    end;

    {for i := 0 to n-1 do begin
        for j := 0 to k[i]-1 do write(' ', a[i][j]);
        writeln;
    end;}

    mx := 0.0;
    for i1 := 0 to n-2 do
        for i2 := i1+1 to n-1 do begin
            s := 0;
            j1 := 0;
            j2 := 0;
            while (j1 < k[i1]) and (j2 < k[i2]) do
                if a[i1][j1] < a[i2][j2] then
                    j1 := nxt[i1][j1]
                else if a[i1][j1] > a[i2][j2] then
                    j2 := nxt[i2][j2]
                else begin
                    inc(s, int64(nxt[i1][j1] - j1) * (nxt[i2][j2] - j2));
                    j1 := nxt[i1][j1];
                    j2 := nxt[i2][j2];
                end;
            p := extended(s) / (int64(k[i1]) * k[i2]);
            if mx < p then mx := p;
        end;

    writeln(mx:11:9);
end.
