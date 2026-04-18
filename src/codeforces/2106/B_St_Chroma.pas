program B_St_Chroma;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, i, x: int32;
    p: array [1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, x);

        for i := 1 to x do p[i] := i-1;
        for i := x+1 to n do p[i] := n-i+x;

        for i := 1 to n-1 do write(p[i], ' ');
        writeln(p[n]);

    end;
end.
