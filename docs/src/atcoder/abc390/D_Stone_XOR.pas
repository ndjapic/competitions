program D_Stone_XOR;
{$mode delphi}{$inline on}
uses
    math,
    Generics.Defaults,
    Generics.Collections;
const
    nn = 12;
    init_size = 65536;
var
    n, i, j, k, r: int32;
    x: int64;
    a: array [0 .. nn] of int64;
    c: array [1 .. nn] of int32;
    b: array [1 .. nn] of array of array of int64;
    d: TDictionary<int64, boolean>;

begin
    readln(n);
    for i := 0 to n-1 do read(a[i]);
    readln;

    c[1] := 1;
    setlength(b[1], 1);
    setlength(b[1][0], 1);
    b[1][0][0] := a[0];

    for r := 2 to n do begin

        setlength(b[r], init_size);

        c[r] := 0;
        for k := 0 to c[r-1] - 1 do
            for i := 0 to r-1 do
                if (i = r-1) or (b[r-1][k][i] > 0) then begin
                    if length(b[r]) = c[r] then setlength(b[r], 2*c[r]);
                    setlength(b[r][c[r]], r);
                    for j := 0 to r-2 do b[r][c[r]][j] := b[r-1][k][j];
                    b[r][c[r]][r-1] := 0;
                    inc(b[r][c[r]][i], a[r-1]);
                    inc(c[r]);
                end;

    end;

    d := TDictionary<int64, boolean>.Create();

    for k := 0 to c[n]-1 do begin
        x := 0;
        for i := 0 to n-1 do x := x xor b[n][k][i];
        d.AddOrSetValue(x, True);
    end;

    writeln(d.Count);
end.
