program B_Permutation_Printing;
const
    maxn = 100 * 1000;
var
    ntc, tci: int16;
    n, i, l, r: int32;
    p: array [1 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin
        readln(n);

        l := 1;
        r := n;
        for i := 1 to n do
            if odd(i) then begin
                p[i] := l;
                inc(l);
            end else begin
                p[i] := r;
                dec(r);
            end;

        for i := 1 to n-1 do write(p[i], ' ');
        writeln(p[n]);
    end;
end.
