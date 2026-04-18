program C_Closest_Cities;
const
    maxn = 100 * 1000;
var
	ntc, tci: int16;
    n, m, i, x, y: int32;
    a, l, r: array [1 .. maxn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n);
        for i := 1 to n do begin
            read(a[i]);
            l[i] := 1;
            r[i] := 1;
        end;
        readln;

        r[1] := 0;
        l[n] := 0;

        for i := 2 to n-1 do
            if a[i+1] - a[i] > a[i] - a[i-1] then
                r[i+1] := a[i+1] - a[i]
            else
                l[i-1] := a[i] - a[i-1];

        for i := 3 to n do inc(r[i], r[i-1]);
        for i := n-2 downto 1 do inc(l[i], l[i+1]);

        readln(m);
        for i := 1 to m do begin
            readln(x, y);
            if x < y then
                writeln(r[y] - r[x])
            else
                writeln(l[y] - l[x]);
        end;

    end;
end.
