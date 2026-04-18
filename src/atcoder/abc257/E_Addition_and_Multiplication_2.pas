program E_Addition_and_Multiplication_2;
{$H+}
const
    maxn = 1000 * 1000;
var
    n, d, j: int32;
    i, i0: int8;
    x: string;
    c: array [1 .. 9] of int32;

begin
    readln(n);

    for i := 1 to 9 do read(c[i]);
    readln;

    i0 := 9;
    for i := 8 downto 1 do
        if c[i] < c[i0] then i0 := i;

    d := n div c[i0];
    setlength(x, d);

    for j := 1 to d do begin
        i := 9;
        while c[i] + int64(d-j) * c[i0] > n do dec(i);
        x[j] := chr(ord('0') + i);
        dec(n, c[i]);
    end;

    writeln(x);
end.
