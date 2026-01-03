program E_Increasing_Subsequences;
const
    maxn = 3600;
var
	ntc, tci: int16;
    x, p2: int64;
    e: int8;
    n, i: int16;
    a: array [1 .. maxn] of int16;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(x);
        n := 0;

        while x > 0 do begin

            e := 0;
            while x shr e > 1 do inc(e);
            p2 := int64(1) shl e;

            for i := 1 to e do a[n+i] := n+e+1-i;
            inc(n, e);
            dec(x, p2);
            if x > 0 then inc(x);

        end;

        writeln(n);
        for i := n downto 2 do write(a[i], ' ');
        writeln(a[1]);

    end;
end.
