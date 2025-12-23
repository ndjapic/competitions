program C_Monopati;
uses
	math;
const
	nn = 200 * 1000 + 1;
var
	notc, tci, n, i, l, r: int32;
	ans: int64;
	a1, a2: array [1 .. nn] of int32;
	b: array [0 .. 2*nn] of int32;
	mn1, mx1, mn2, mx2: array [0 .. nn] of int32;

begin
	readln(notc);
	for tci := 1 to notc do begin

		readln(n);
		for i := 1 to n do read(a1[i]); readln;
		for i := 1 to n do read(a2[i]); readln;

		mn1[0] := 2*n;
		mx1[0] := 1;
		for i := 1 to n do begin
			mn1[i] := min(mn1[i-1], a1[i]);
			mx1[i] := max(mx1[i-1], a1[i]);
		end;

		mn2[n+1] := 2*n;
		mx2[n+1] := 1;
		for i := n downto 1 do begin
			mn2[i] := min(mn2[i+1], a2[i]);
			mx2[i] := max(mx2[i+1], a2[i]);
		end;

		for r := 0 to 2*n do b[r] := 0;

		for i := 1 to n do begin
			l := min(mn1[i], mn2[i]);
			r := max(mx1[i], mx2[i]);
			b[r] := max(b[r], l);
		end;

		ans := 0;
		for r := 1 to 2*n do begin
			b[r] := max(b[r], b[r-1]);
			inc(ans, b[r]);
		end;

		writeln(ans);

	end;
end.
