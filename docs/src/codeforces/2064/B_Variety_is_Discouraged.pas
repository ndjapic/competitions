program B_Variety_is_Discouraged;
const
    nn = 200 * 1000;
var
	ntc, tci: int16;
    n, i, x, mx, l, r, l0, r0: int32;
    a, c: array [1 .. nn] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

		readln(n);

        for x := 1 to n do c[x] := 0;

        for i := 1 to n do begin
            read(a[i]);
            inc(c[a[i]]);
        end;
		readln;

        l := 1;
        mx := 0;

        for r := 1 to n do begin
            if c[a[r]] > 1 then l := r+1;
            if mx < r-l+1 then begin
                mx := r-l+1;
                l0 := l;
                r0 := r;
            end;
        end;

        if mx = 0 then
            writeln(0)
        else
            writeln(l0, ' ', r0);

    end;
end.
