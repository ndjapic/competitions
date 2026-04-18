program E_Yet_Another_Sigma_Problem;
const
    nn = 200 * 1000;
    ten8 = 100 * 1000 * 1000;
    prime = 998244353;
var
    n, i: int32;
    s, t: int64;
    e: int8;
    a: array [1 .. nn] of int64;
    c: array [1 .. 10] of int32;
    p10: array [0 .. 10] of int64;

begin
    p10[0] := 1;
    for e := 1 to 10 do begin
        p10[e] := p10[e-1] * 10;
        c[e] := 0;
    end;

    readln(n);

    for i := 1 to n do read(a[i]);
    readln;

    s := 0;
    for i := n downto 1 do begin
        s := (s + a[i] * (i-1)) mod prime;

        for e := 1 to 10 do begin
            t := int64(a[i]) * c[e] mod prime;
            s := (s + p10[e] mod prime * t) mod prime;
        end;

        e := 1;
        while a[i] >= p10[e] do inc(e);
        inc(c[e]);
    end;

    writeln(s);
end.
