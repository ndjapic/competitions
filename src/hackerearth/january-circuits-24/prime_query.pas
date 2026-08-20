program prime_query;
uses
    math;
const
    maxn = 100 * 1000;
var
    ntc: int8;
    n, i, x, q, l, r: int32;
    ans: int64;
    c: array [0 .. maxn, 0 .. 2] of int32;
    d: array [0 .. 2] of int32;
 
begin
    readln(ntc);
    repeat
 
        for x := 0 to 2 do c[0, x] := 0;
 
        readln(n);
        for i := 1 to n do begin
            for x := 0 to 2 do c[i, x] := c[i-1, x];
            read(x);
            inc(c[i, min(x, 2)]);
        end;
        readln;
 
        readln(q);
        for i := 1 to q do begin
 
            readln(l, r);
            dec(l);
            for x := 0 to 2 do d[x] := c[r, x] - c[l, x];
 
            ans := int64(d[0] + d[1]) * d[2] +
                int64(d[1] - 1) * d[1] div 2 +
                int64(d[2] - 1) * d[2] div 2;
            writeln(ans);
 
        end;
 
        dec(ntc);
    until ntc = 0;
end.
