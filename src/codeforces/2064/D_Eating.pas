program D_Eating;
uses
    math;
const
    nn = 200 * 1000;
var
    ntc, tci: int16;
    n, q, i, j, x: int32;
    w, s: array [1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n, q);

        for i := 1 to n do read(w[i]); readln;

        s[n] := 0;
        for i := n downto 2 do s[i-1] := s[i] xor w[i];

        for j := 1 to q do begin
            readln(x);
            i := n;
            while (i > 0) and (x xor s[i] >= w[i]) do dec(i);
            write(n-i);
            if j < q then write(' ');
        end;
        writeln;

    end;
end.
