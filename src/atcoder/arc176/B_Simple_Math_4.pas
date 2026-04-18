program B_Simple_Math_4;
var
    ntc, tci: int32;
    n, m, k: int64;
    ans, i: int8;
    p2: array [0 .. 3] of int8;

begin
    p2[0] := 1;
    for i := 1 to 3 do p2[i] := p2[i-1] * 2 mod 5;

    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m, k);
        if n >= m then n := (n-k) mod (m-k) + k;
        if (m-k = 1) and (n = k) then
            ans := 0
        else
            ans := p2[n mod 4];
        if odd(ans) then inc(ans, 5);
        writeln(ans);

    end;
end.
