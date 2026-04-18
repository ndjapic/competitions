program E_Mexification;
const
	nn = 200 * 1000;
var
	ntc, tci, n, i, m: int32;
	a, ml, mr, cl, cr: array [1 .. nn] of int32;
	bl, br: array [0 .. nn] of boolean;

begin
	readln(ntc);
	for tci := 1 to ntc do begin

		readln(n, k);

		for i := 1 to n do read(a[i]);
		readln;

		for m := 0 to n do begin
			bl[m] := false;
			br[m] := false;
		end;

		m := 0;
		for i := 1 to n do begin
			while bl[m] do inc(m);
			ml[i] := m;
			bl[a[i]] := true;
		end;

		m := 0;
		for i := n downto 1 do begin
			while br[m] do inc(m);
			mr[i] := m;
			br[a[i]] := true;
		end;



	end;
end.
