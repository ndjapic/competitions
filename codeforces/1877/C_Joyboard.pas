program C_Joyboard;
uses
	math;
var
    ntc, tci: int16;
    n, m, k: int32;
    a: array [1 .. 4] of int32;

begin
    readln(ntc);
    for tci := 1 to ntc do begin

        readln(n, m, k);

		a[1] := 1;
		a[2] := min(m, n-1) + m div n;
		a[3] := m+1 - a[1] - a[2];
		a[4] := 0;
		k := min(k, 4);

		writeln(a[k]);

    end;
end.
