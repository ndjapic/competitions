program D_Counting_Rhyme;
const
    maxn = 1000 * 1000;
var
    ntc, tci, n, i, ai, x, nx: int32;
    ans: int64;
    c: array [1 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n);

        for ai := 1 to n do c[ai] := 0;

        for i := 1 to n do begin
            read(ai);
            inc(c[ai]);
        end;
        readln;

        ans := int64(n-1) * n div 2;

        for ai := n downto 1 do
            if c[ai] > 0 then begin
                nx := 0;
                x := ai;
                while x <= n do begin
                    inc(nx, c[x]);
                    c[x] := 0;
                    inc(x, ai);
                end;
                dec(ans, int64(nx-1) * nx div 2);
            end;

        writeln((ans - 0) div 1);

    end;
end.
